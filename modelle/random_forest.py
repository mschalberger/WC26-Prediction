"""
Random Forest Modell für Länderspiel-Vorhersagen
=================================================
Sagt erwartete Tore beider Teams vorher basierend auf:
- Rolling-Durchschnitte der letzten 15 Spiele (33 Sofascore-Statistiken)
- Elo-Ratings (eloratings.net Methodik)
- Logistische Baseline-Vorhersage als Anker-Feature
- Lineup-Marktwerte (log10-skaliert, aus Sofascore)
- Heimvorteil und Spielwichtigkeit

Tore werden via Poisson-Verteilung in Gewinnwahrscheinlichkeiten umgerechnet.
Evaluation gegen eloratings.net Baseline aus GitHub-TSV-Dateien.

Voraussetzungen:
    pip install pandas numpy scipy scikit-learn
"""

import pandas as pd
import numpy as np
from scipy.stats import poisson
from sklearn.ensemble import RandomForestRegressor
from sklearn.impute import SimpleImputer
from collections import Counter
import os
import warnings
warnings.filterwarnings('ignore')

# ============================================================
# KONFIGURATION
# ============================================================

# Die Lineup-CSV enthält alle Spieldaten plus home_lineup_value / away_lineup_value

MATCH_CSV           = '../sofa scrape/sofascore_data/all_international_matches_lineups.csv'
TSV_DIR             = 'tsv_data'   # eloratings.net TSV-Dateien, eine pro Team

START_DATE          = '2024-06-01'  # ab hier werden Trainingsdatenpunkte erzeugt;

ROLLING_WINDOW      = 15            # letzte N Spiele für Rolling-Durchschnitte
RF_TREES            = 500           # Anzahl Bäume im Ensemble
RF_MAX_DEPTH        = 5             # maximale Tiefe jedes Baums; begrenzt Overfitting
RF_MIN_SAMPLES_LEAF = 8             # mind. 8 Trainingsspiele pro Blatt
HOME_ADVANTAGE      = 100           # Elo-Punkte Bonus für das Heimteam
TRAIN_SPLIT         = 0.8           # zeitlicher 80/20-Split, kein zufälliges Shuffling

# 33 Sofascore-Statistiken die als Rolling-Durchschnitt pro Team einfließen.
# Für ältere Spiele ohne Sofascore-Daten bleiben diese Werte NaN
# und werden später vom Median-Imputer aufgefüllt.
STAT_FEATURES = [
    'ballPossession', 'shotsOnGoal', 'shotsOffGoal', 'totalShotsOnGoal',
    'totalShotsInsideBox', 'totalShotsOutsideBox', 'blockedScoringAttempt',
    'cornerKicks', 'fouls', 'freeKicks', 'offsides', 'goalKicks',
    'goalkeeperSaves', 'throwIns', 'yellowCards', 'hitWoodwork',
    'passes', 'accuratePasses', 'accurateLongBalls', 'accurateCross',
    'totalTackle', 'interceptionWon', 'totalClearance', 'dispossessed',
    'duelWonPercent', 'groundDuelsPercentage', 'aerialDuelsPercentage',
    'dribblesPercentage', 'bigChanceCreated', 'wonTacklePercent',
    'finalThirdEntries', 'ballRecovery', 'fouledFinalThird',
]

# Sofascore verwendet andere Teamnamen als eloratings.net (TSV-Dateinamen).
# Dieses Dictionary übersetzt die abweichenden Namen.
# Nicht aufgeführte Teams werden durch team.replace(' ', '_') konvertiert.
SOFASCORE_TO_TSV = {
    'USA': 'United_States', "Côte d'Ivoire": 'Ivory_Coast',
    'Cabo Verde': 'Cape_Verde', 'Türkiye': 'Turkey',
    'DR Congo': 'DR_Congo', 'Congo Republic': 'Congo',
    'Macau': 'Macao', 'Chinese Taipei': 'Taiwan',
    'Brunei Darussalam': 'Brunei', 'Myanmar': 'Myanmar',
    'Eswatini': 'Eswatini',
    "São Tomé and Príncipe": 'Sao_Tome_and_Principe',
    'East Timor': 'East_Timor', 'North Korea': 'North_Korea',
    'South Korea': 'South_Korea', 'North Macedonia': 'North_Macedonia',
    'Northern Ireland': 'Northern_Ireland', 'South Africa': 'South_Africa',
    'South Sudan': 'South_Sudan', 'New Zealand': 'New_Zealand',
    'New Caledonia': 'New_Caledonia', 'Hong Kong': 'Hong_Kong',
    'Saudi Arabia': 'Saudi_Arabia', 'Sri Lanka': 'Sri_Lanka',
    'Costa Rica': 'Costa_Rica', 'El Salvador': 'El_Salvador',
    'Dominican Republic': 'Dominican_Republic',
    'Trinidad and Tobago': 'Trinidad_and_Tobago',
    'Antigua and Barbuda': 'Antigua_and_Barbuda',
    'Saint Kitts and Nevis': 'Saint_Kitts_and_Nevis',
    'Saint Lucia': 'Saint_Lucia',
    'Saint Vincent and the Grenadines': 'Saint_Vincent_and_the_Grenadines',
    'Papua New Guinea': 'Papua_New_Guinea',
    'Solomon Islands': 'Solomon_Islands',
    'British Virgin Islands': 'British_Virgin_Islands',
    'Cayman Islands': 'Cayman_Islands', 'Faroe Islands': 'Faroe_Islands',
    'Cook Islands': 'Cook_Islands',
    'United Arab Emirates': 'United_Arab_Emirates',
    'Turks and Caicos Islands': 'Turks_and_Caicos_Islands',
    'Central African Republic': 'Central_African_Republic',
    'Equatorial Guinea': 'Equatorial_Guinea',
    'Guinea-Bissau': 'Guinea-Bissau', 'Burkina Faso': 'Burkina_Faso',
    'Sierra Leone': 'Sierra_Leone',
    'Bosnia & Herzegovina': 'Bosnia_and_Herzegovina',
    'Bosnia and Herzegovina': 'Bosnia_and_Herzegovina',
    'French Guiana': 'French_Guiana',
    'Marshall Islands': 'Marshall_Islands',
    'Northern Mariana Islands': 'Northern_Mariana_Islands',
    'US Virgin Islands': 'US_Virgin_Islands',
    'Puerto Rico': 'Puerto_Rico', 'FS Micronesia': 'FS_Micronesia',
}

# ============================================================
# HILFSFUNKTIONEN
# ============================================================

def sof2tsv(team):
    """Übersetzt einen Sofascore-Teamnamen in den entsprechenden TSV-Dateinamen."""
    return SOFASCORE_TO_TSV.get(team, team.replace(' ', '_'))


def get_k_factor(tournament):
    """
    Gibt den K-Faktor für ein Turnier zurück. Der K-Faktor steuert wie stark
    ein Spiel die internen Elo-Werte verschiebt, höhere Werte für wichtigere
    Turniere. Doppelte Verwendung: Elo-Update nach jedem Spiel + als RF-Feature
    (der RF kann lernen, dass taktisches Verhalten je nach Turnierwichtigkeit
    variiert).

    WM (Endrunde) = 60, EM/Copa América/AFCON etc. = 50,
    Qualifikationen / Nations League = 40, Freundschaftsspiele = 20
    """
    t = str(tournament)
    if 'FIFA World Cup' in t and 'Qual' not in t: return 60
    if any(kw in t for kw in ['Euro, Group', 'Euro, Knockout', 'Copa América',
        'CONMEBOL Copa', 'Africa Cup of Nations', 'AFC Asian Cup',
        'Nations League, Finals', 'Nations League, Final',
        'Confederations Cup', 'Finalissima']): return 50
    if any(kw in t for kw in ['Qual', 'Nations League, League',
        'Nations League, Play-In']): return 40
    if any(kw in t for kw in ['CONCACAF Gold Cup', 'COSAFA', 'WAFF', 'SAFF',
        'EAFF', 'Gulf Cup', 'Arab Cup', 'Caribbean', 'Copa Centroamericana',
        'CAFA', 'Pacific', 'Baltic', 'King', 'Intercontinental', 'OFC',
        'FIFA Series', 'Unity Cup']): return 30
    if 'Friendly' in t: return 20
    return 30  # Fallback für unbekannte Turniere


def goal_diff_multiplier(goal_diff):
    """
    Gewichtungsfaktor für den Elo-Update basierend auf dem Torunterschied.
    Identisch zur eloratings.net-Methodik: ein klarer Sieg verschiebt Elo
    stärker als ein knappes Ergebnis.
    Wird nur für den internen Elo-Update verwendet, nicht als RF-Feature.
    """
    d = abs(goal_diff)
    if d <= 1: return 1.0
    elif d == 2: return 1.5
    else: return (11 + d) / 8


def expected_score(elo_a, elo_b):
    """
    Klassische Elo-Erwartungsformel (identisch zu eloratings.net):
        W_e = 1 / (1 + 10^((elo_b - elo_a) / 400))
    Gibt die erwartete Gewinnwahrscheinlichkeit von Team A zurück.
    Bei Elo-Differenz 0 → 0.5, bei +200 → ~0.76, bei +400 → ~0.91.
    Verwendet für: Baseline-Berechnung, elo_logistic_pred-Feature,
    und den internen Elo-Update.
    """
    return 1 / (1 + 10 ** ((elo_b - elo_a) / 400))


def home_bonus(venue):
    """
    Gibt den Elo-Heimvorteil-Bonus zurück (+100 / 0 / -100).
    'home' / 'host_home': Team spielt auf eigenem Platz → +100
    'host_away': Team spielt im Gastgeberland, nicht im Heimstadion → -100
    'neutral': neutrales Stadion → 0
    """
    if venue in ['home', 'host_home']: return HOME_ADVANTAGE
    elif venue == 'host_away': return -HOME_ADVANTAGE
    else: return 0


def poisson_win_prob(lambda_h, lambda_a, max_goals=8):
    """
    Berechnet die Heimteam-Gewinnwahrscheinlichkeit aus zwei Poisson-Lambdas.
    Summiert alle 81 Torkombinationen (0-8 x 0-8):
        P(Heimsieg) + 0.5 * P(Remis)
    Lambda-Werte unter 0.05 werden geclampt um numerische Probleme
    bei sehr kleinen Lambdas zu vermeiden.
    """
    p_home = p_draw = 0
    for gh in range(max_goals + 1):
        for ga in range(max_goals + 1):
            p = poisson.pmf(gh, max(lambda_h, 0.05)) * poisson.pmf(ga, max(lambda_a, 0.05))
            if gh > ga: p_home += p
            elif gh == ga: p_draw += p
    return p_home + 0.5 * p_draw


def get_rolling_features(history, prefix):
    """
    Berechnet Rolling-Durchschnitte aus den letzten ROLLING_WINDOW Spielen
    eines Teams. Gibt None zurück wenn weniger als 3 Spiele in der History.

    Erzeugte Features (mit Präfix h_ oder a_):
    - avg_gf, avg_ga, gdiff: Tordurchschnitte und -differenz
    - wr: Gewinnrate (Sieg=1, Remis=0.5, Niederlage=0)
    - gp: Anzahl Spiele im Fenster
    - avg_{stat}: Durchschnitt jeder Sofascore-Statistik (NaN wenn keine Daten)
    - avg_xgf, avg_xga: Expected Goals offensiv/defensiv
    """
    if len(history) < 3:
        return None
    hist = history[-ROLLING_WINDOW:]
    f = {}
    gf = [h['gf'] for h in hist]
    ga = [h['ga'] for h in hist]
    f[f'{prefix}avg_gf']  = np.mean(gf)
    f[f'{prefix}avg_ga']  = np.mean(ga)
    f[f'{prefix}gdiff']   = np.mean(gf) - np.mean(ga)
    f[f'{prefix}wr']      = np.mean([1 if g > a else (0.5 if g == a else 0)
                                      for g, a in zip(gf, ga)])
    f[f'{prefix}gp']      = len(hist)
    for s in STAT_FEATURES:
        vals = [h.get(f's_{s}') for h in hist if h.get(f's_{s}') is not None]
        f[f'{prefix}avg_{s}'] = np.mean(vals) if vals else np.nan
    xf = [h.get('xf') for h in hist if h.get('xf') is not None]
    xa = [h.get('xa') for h in hist if h.get('xa') is not None]
    f[f'{prefix}avg_xgf'] = np.mean(xf) if xf else np.nan
    f[f'{prefix}avg_xga'] = np.mean(xa) if xa else np.nan
    return f


def load_tsv_elo(tsv_dir):
    """
    Liest alle .tsv-Dateien aus tsv_dir und baut zwei Lookup-Strukturen:
    - team_elo_lookup: Teamname → chronologische Liste von Elo-Einträgen
    - team_code_map:   Teamname → häufigster Ländercode (für Spaltenzuordnung)

    Jede TSV-Datei entspricht einem Team von eloratings.net. Spalten 0-2 sind
    Datum (Jahr/Monat/Tag), Spalten 3-4 die Ländercodes, Spalten 10-11 die
    Elo-Werte nach dem Spiel.
    """
    team_elo_lookup = {}
    team_code_map   = {}
    for fn in os.listdir(tsv_dir):
        if not fn.endswith('.tsv'): continue
        tsv_name = fn.replace('.tsv', '')
        try:
            df = pd.read_csv(os.path.join(tsv_dir, fn), sep='\t', header=None)
        except: continue
        records = []
        cc = Counter()
        for _, row in df.iterrows():
            try:
                d = f"{int(row[0])}-{int(row[1]):02d}-{int(row[2]):02d}"
                ca, cb = str(row[3]), str(row[4])
                records.append({'date': d, 'ca': ca, 'cb': cb,
                                'ea': row[10], 'eb': row[11]})
                cc[ca] += 1; cc[cb] += 1
            except: continue
        team_elo_lookup[tsv_name] = records
        if cc: team_code_map[tsv_name] = cc.most_common(1)[0][0]
    return team_elo_lookup, team_code_map


def get_real_elo(team, match_date, team_elo_lookup, team_code_map):
    """
    Sucht den letzten TSV-Elo-Eintrag eines Teams VOR dem gegebenen Spieldatum.
    Gibt None zurück wenn kein Eintrag gefunden (z.B. Team nicht in TSV-Daten).
    In diesem Fall wird im Hauptprogramm auf das interne Elo zurückgegriffen.

    """
    tn = sof2tsv(team)
    if tn not in team_elo_lookup: return None
    code = team_code_map.get(tn)
    if not code: return None
    best = None
    for r in team_elo_lookup[tn]:
        if r['date'] >= match_date: break  # nur Einträge vor dem Spieldatum
        if r['ca'] == code: best = r['ea']
        elif r['cb'] == code: best = r['eb']
    return best


def market_value_features(raw_h, raw_a):
    """
    Berechnet vier log10-skalierte Marktwert-Features aus den Lineup-Rohwerten.
    Die Log-Transformation verhindert, dass Extremwerte (z.B. Brasilien ~426M EUR)
    die Skala dominieren. Gibt NaN zurück wenn kein Wert verfügbar — betrifft
    ~8% der Spiele ab START_DATE (Median-Imputer füllt diese auf).

    mv_h / mv_a:   log10 des Lineup-Marktwerts (Heimteam / Gastteam)
    mv_d:          Differenz mv_h - mv_a (analog zu elo_d)
    mv_ratio:      Verhältnis mv_h / mv_a (robuster bei extremen Unterschieden)
    """
    mv_h = np.log10(raw_h) if pd.notna(raw_h) and raw_h > 0 else np.nan
    mv_a = np.log10(raw_a) if pd.notna(raw_a) and raw_a > 0 else np.nan
    mv_d     = mv_h - mv_a if pd.notna(mv_h) and pd.notna(mv_a) else np.nan
    mv_ratio = mv_h / mv_a if pd.notna(mv_h) and pd.notna(mv_a) and mv_a != 0 else np.nan
    return mv_h, mv_a, mv_d, mv_ratio


# ============================================================
# HAUPTPROGRAMM
# ============================================================

if __name__ == '__main__':
    print("=" * 70)
    print("RANDOM FOREST MODELL")
    print("=" * 70)

    # --- Daten laden ---
    # Gefiltert auf Senior-Männerspiele mit bekanntem Ergebnis, chronologisch sortiert.
    # &amp; in Teamnamen wird korrigiert (HTML-Artefakt aus dem Scraper).
    matches = pd.read_csv(MATCH_CSV, low_memory=False)
    matches = matches[matches['is_senior_mens'] == True]
    matches = matches.dropna(subset=['home_score', 'away_score'])
    matches = matches.sort_values('date').reset_index(drop=True)
    matches['home_team'] = matches['home_team'].str.replace('&amp;', '&')
    matches['away_team'] = matches['away_team'].str.replace('&amp;', '&')
    print(f"Senior-Spiele: {len(matches)}")

    team_elo_lookup, team_code_map = load_tsv_elo(TSV_DIR)
    print(f"TSV-Teams: {len(team_elo_lookup)}")

    # Internes Elo-Dictionary: Startwert = TSV-Elo zum START_DATE, Fallback 1500.
    # Wird in der Schleife nach jedem Spiel aktualisiert.
    all_teams = set(matches['home_team'].unique()) | set(matches['away_team'].unique())
    elo       = {t: get_real_elo(t, START_DATE, team_elo_lookup, team_code_map) or 1500
                 for t in all_teams}
    team_hist = {t: [] for t in all_teams}  # Spielhistory pro Team für Rolling-Features
    dataset   = []

    # --- Hauptschleife: chronologisch über alle Spiele ---
    # Reihenfolge innerhalb jedes Spiels ist entscheidend für Data-Leakage-Prävention:
    #   1. Features aus bisheriger History berechnen
    #   2. Datenpunkt speichern (nur ab START_DATE)
    #   3. History und Elo mit dem Ergebnis dieses Spiels updaten
    for _, row in matches.iterrows():
        home, away = row['home_team'], row['away_team']
        has_xg = pd.notna(row['ALL_home_expectedGoals'])
        v = row.get('match_venue', 'home')
        if pd.isna(v): v = 'home'
        b = home_bonus(v)

        if row['date'] >= START_DATE:
            hf = get_rolling_features(team_hist[home], 'h_')
            af = get_rolling_features(team_hist[away], 'a_')
            # TSV-Elo: letzter Snapshot vor diesem Spieldatum
            rh = get_real_elo(home, row['date'], team_elo_lookup, team_code_map)
            ra = get_real_elo(away, row['date'], team_elo_lookup, team_code_map)

            if hf and af:  # mind. 3 Spiele in der History beider Teams
                feat = {**hf, **af}

                # Elo-Features: TSV bevorzugt, internes Elo als Fallback
                elo_h_use = rh if rh is not None else elo[home]
                elo_a_use = ra if ra is not None else elo[away]
                feat['elo_h']             = elo_h_use
                feat['elo_a']             = elo_a_use
                feat['elo_d']             = elo_h_use - elo_a_use
                feat['elo_d_with_bonus']  = (elo_h_use + b) - elo_a_use
                # elo_logistic_pred: vollständige Baseline-Vorhersage als Feature.
                # Gibt dem RF eine untere Schranke — im schlechtesten Fall
                # reicht er diesen Wert durch und landet bei Baseline-Performance.
                feat['elo_logistic_pred'] = expected_score(elo_h_use + b, elo_a_use)
                feat['elo_h_internal']    = elo[home]  # aktuelles internes Elo
                feat['elo_a_internal']    = elo[away]

                # Marktwert-Features (log10-skaliert, NaN wenn nicht verfügbar)
                mv_h, mv_a, mv_d, mv_ratio = market_value_features(
                    row.get('home_lineup_value'), row.get('away_lineup_value')
                )
                feat['mv_h']     = mv_h
                feat['mv_a']     = mv_a
                feat['mv_d']     = mv_d
                feat['mv_ratio'] = mv_ratio

                feat['is_home']   = 1 if v in ['home', 'host_home'] else 0
                feat['is_neutral']= 1 if v == 'neutral' else 0
                # k als Feature: Proxy für Turnierwichtigkeit und taktischen Kontext
                feat['k']         = get_k_factor(str(row['tournament']))
                feat['tgt_h']     = row['home_score']   # Zielgröße Heimtore
                feat['tgt_a']     = row['away_score']   # Zielgröße Auswärtstore
                feat['date']      = row['date']
                feat['home']      = home
                feat['away']      = away
                feat['venue']     = v
                feat['relo_h']    = rh   # TSV-Elo für Baseline-Berechnung
                feat['relo_a']    = ra
                feat['has_real']  = rh is not None and ra is not None
                dataset.append(feat)

        # History-Update: läuft für ALLE Spiele ab 2012, auch vor START_DATE,
        # damit die Rolling-History zum ersten Datenpunkt korrekt aufgebaut ist.
        hs  = {f's_{s}': row[f'ALL_home_{s}'] for s in STAT_FEATURES
               if pd.notna(row.get(f'ALL_home_{s}'))}
        as_ = {f's_{s}': row[f'ALL_away_{s}'] for s in STAT_FEATURES
               if pd.notna(row.get(f'ALL_away_{s}'))}
        xhv = row['ALL_home_expectedGoals'] if has_xg else None
        xav = row['ALL_away_expectedGoals'] if has_xg else None

        team_hist[home].append({'gf': row['home_score'], 'ga': row['away_score'],
                                'xf': xhv, 'xa': xav, **hs})
        team_hist[away].append({'gf': row['away_score'], 'ga': row['home_score'],
                                'xf': xav, 'xa': xhv, **as_})

        # Internes Elo-Update (nach dem Datenpunkt, kein Leakage)
        K   = get_k_factor(str(row['tournament']))
        G   = goal_diff_multiplier(row['home_score'] - row['away_score'])
        act = 1.0 if row['home_score'] > row['away_score'] else \
              (0.0 if row['home_score'] < row['away_score'] else 0.5)
        exp = expected_score(elo[home] + b, elo[away])
        elo[home] += K * G * (act - exp)
        elo[away] += K * G * ((1 - act) - (1 - exp))

    # --- Train/Test Split ---
    # Zeitlich: erste 80% = Training, letzte 20% = Test.
    df    = pd.DataFrame(dataset)
    split = int(len(df) * TRAIN_SPLIT)
    train = df.iloc[:split]
    test  = df.iloc[split:]

    # meta-Spalten werden nicht als Features verwendet
    meta  = ['date', 'home', 'away', 'tgt_h', 'tgt_a',
             'relo_h', 'relo_a', 'has_real', 'venue']
    fcols = [c for c in df.columns if c not in meta]

    print(f"Features: {len(fcols)}, Train: {len(train)}, Test: {len(test)}")
    print(f"Marktwert-Abdeckung: {df['mv_h'].notna().mean():.0%}")

    # Median-Imputation: NaN-Werte mit dem Trainings-Median auffüllen.
    # fit nur auf Training — transform auf Test verhindert Datenleck.
    imp     = SimpleImputer(strategy='median')
    X_train = pd.DataFrame(imp.fit_transform(train[fcols]), columns=fcols)
    X_test  = pd.DataFrame(imp.transform(test[fcols]),  columns=fcols)

    # --- Zwei separate Regressoren: Heim- und Auswärtstore ---
    # Unabhängige Modelle, identische Parameter.
    # Zielgröße sind die tatsächlichen Toranzahlen (Regression).
    print("\nTrainiere Random Forest...")
    rf_h = RandomForestRegressor(n_estimators=RF_TREES, max_depth=RF_MAX_DEPTH,
                                  min_samples_leaf=RF_MIN_SAMPLES_LEAF,
                                  random_state=42, n_jobs=-1)
    rf_a = RandomForestRegressor(n_estimators=RF_TREES, max_depth=RF_MAX_DEPTH,
                                  min_samples_leaf=RF_MIN_SAMPLES_LEAF,
                                  random_state=42, n_jobs=-1)
    rf_h.fit(X_train, train['tgt_h'].values)
    rf_a.fit(X_train, train['tgt_a'].values)

    # Vorhersagen als Poisson-Lambdas interpretieren → Gewinnwahrscheinlichkeit
    pred_h   = rf_h.predict(X_test)
    pred_a   = rf_a.predict(X_test)
    rf_preds = np.array([poisson_win_prob(h, a) for h, a in zip(pred_h, pred_a)])

    # Baseline: eloratings.net logistische Formel auf TSV-Elo
    # Nur Spiele mit verfügbarem TSV-Elo für beide Teams werden ausgewertet.
    actuals = np.array([1.0 if h > a else (0.0 if h < a else 0.5)
                         for h, a in zip(test['tgt_h'].values, test['tgt_a'].values)])
    real_preds = []
    for _, r in test.iterrows():
        if r['has_real'] and pd.notna(r['relo_h']) and pd.notna(r['relo_a']):
            b = home_bonus(r['venue'])
            real_preds.append(expected_score(r['relo_h'] + b, r['relo_a']))
        else:
            real_preds.append(np.nan)
    real_preds = np.array(real_preds)
    mask = ~np.isnan(real_preds)  # nur Spiele mit verfügbarer Baseline

    # Ergebnisse:
    # Brier Score = MSE auf Wahrscheinlichkeitsvorhersagen.
    # Ergebnisse als Punkte: 1.0 = Heimsieg, 0.5 = Remis, 0.0 = Auswärtssieg.
    # Zusätzlich segmentiert nach minimalem Elo beider Teams.
    print(f"\n{'='*70}")
    print("ERGEBNISSE")
    print(f"{'='*70}")

    brier_real = ((real_preds[mask] - actuals[mask]) ** 2).mean()
    brier_rf   = ((rf_preds[mask]   - actuals[mask]) ** 2).mean()

    print(f"\n  Alle Spiele mit verfügbarer Elo ({mask.sum()}):")
    print(f"    eloratings.net: {brier_real:.4f}")
    print(f"    Random Forest:  {brier_rf:.4f} "
          f"({(brier_rf-brier_real)/brier_real*100:+.1f}%)")

    min_elo = test[mask][['relo_h', 'relo_a']].min(axis=1).values
    for threshold in [1600, 1700, 1800]:
        level = min_elo >= threshold
        if level.sum() < 10: continue
        br = ((real_preds[mask][level] - actuals[mask][level]) ** 2).mean()
        bm = ((rf_preds[mask][level]   - actuals[mask][level]) ** 2).mean()
        print(f"\n  Beide ≥{threshold} ({level.sum()} Spiele):")
        print(f"    eloratings.net: {br:.4f}")
        print(f"    Random Forest:  {bm:.4f} ({(bm-br)/br*100:+.1f}%)")

    # Feature-Importance:
    # Basiert auf mittlerer Varianzreduktion pro Split über alle 500 Bäume.
    # Beide Modelle separat, da ihre Feature-Prioritäten abweichen können.
    print(f"\n{'='*70}")
    print("TOP 15 FEATURES")
    print(f"{'='*70}")
    fi_h = pd.DataFrame({'feature': fcols, 'importance': rf_h.feature_importances_})
    fi_a = pd.DataFrame({'feature': fcols, 'importance': rf_a.feature_importances_})
    fi_h = fi_h.sort_values('importance', ascending=False).head(15)
    fi_a = fi_a.sort_values('importance', ascending=False).head(15)
    print(f"\n  Heimtor-Modell:")
    for _, r in fi_h.iterrows():
        print(f"    {r['feature']:30s} {r['importance']:.4f}")
    print(f"\n  Gasttor-Modell:")
    for _, r in fi_a.iterrows():
        print(f"    {r['feature']:30s} {r['importance']:.4f}")

    # Export für Diebold-Mariano-Test in R
    # dm.test() aus dem forecast-Paket prüft ob die Brier-Differenz statistisch
    # signifikant ist. Exportiert werden die quadratischen Fehler pro Spiel
    # für beide Modelle sowie min_elo für Segmentierung nach Spielstärke.
    export = test[mask].copy()
    export['se_real'] = (real_preds[mask] - actuals[mask]) ** 2
    export['se_rf']   = (rf_preds[mask]   - actuals[mask]) ** 2
    export['min_elo'] = min_elo
    export.to_csv('dm_rf_data.csv', index=False)
    print(f"\nExportiert: dm_rf_data.csv")