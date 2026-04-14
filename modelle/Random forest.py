"""
Random Forest Modell für Länderspiel-Vorhersagen
=================================================
Sagt erwartete Tore beider Teams vorher basierend auf:
- Rolling-Durchschnitte der letzten 10 Spiele (33 Sofascore-Statistiken)
- Elo-Ratings (eloratings.net Methodik)
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

# DATEI_PFADE anpassen
MATCH_CSV = '../sofa scrape/sofascore_data/all_international_matches.csv'
TSV_DIR = 'tsv_data'

START_DATE = '2024-06-01'
ROLLING_WINDOW = 10
RF_TREES = 500
RF_MAX_DEPTH = 10
RF_MIN_SAMPLES_LEAF = 10
HOME_ADVANTAGE = 100
TRAIN_SPLIT = 0.8

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
    return SOFASCORE_TO_TSV.get(team, team.replace(' ', '_'))

def get_k_factor(tournament):
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
    return 30

def goal_diff_multiplier(goal_diff):
    d = abs(goal_diff)
    if d <= 1: return 1.0
    elif d == 2: return 1.5
    else: return (11 + d) / 8

def expected_score(elo_a, elo_b):
    return 1 / (1 + 10 ** ((elo_b - elo_a) / 400))

def home_bonus(venue):
    if venue in ['home', 'host_home']: return HOME_ADVANTAGE
    elif venue == 'host_away': return -HOME_ADVANTAGE
    else: return 0

def poisson_win_prob(lambda_h, lambda_a, max_goals=8):
    p_home = p_draw = 0
    for gh in range(max_goals + 1):
        for ga in range(max_goals + 1):
            p = poisson.pmf(gh, max(lambda_h, 0.05)) * poisson.pmf(ga, max(lambda_a, 0.05))
            if gh > ga: p_home += p
            elif gh == ga: p_draw += p
    return p_home + 0.5 * p_draw

def get_rolling_features(history, prefix):
    if len(history) < 3:
        return None
    hist = history[-ROLLING_WINDOW:]
    f = {}
    gf = [h['gf'] for h in hist]
    ga = [h['ga'] for h in hist]
    f[f'{prefix}avg_gf'] = np.mean(gf)
    f[f'{prefix}avg_ga'] = np.mean(ga)
    f[f'{prefix}gdiff'] = np.mean(gf) - np.mean(ga)
    f[f'{prefix}wr'] = np.mean([1 if g > a else (0.5 if g == a else 0)
                                 for g, a in zip(gf, ga)])
    f[f'{prefix}gp'] = len(hist)
    for s in STAT_FEATURES:
        vals = [h.get(f's_{s}') for h in hist if h.get(f's_{s}') is not None]
        f[f'{prefix}avg_{s}'] = np.mean(vals) if vals else np.nan
    xf = [h.get('xf') for h in hist if h.get('xf') is not None]
    xa = [h.get('xa') for h in hist if h.get('xa') is not None]
    f[f'{prefix}avg_xgf'] = np.mean(xf) if xf else np.nan
    f[f'{prefix}avg_xga'] = np.mean(xa) if xa else np.nan
    return f

def load_tsv_elo(tsv_dir):
    team_elo_lookup = {}
    team_code_map = {}
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
    tn = sof2tsv(team)
    if tn not in team_elo_lookup: return None
    code = team_code_map.get(tn)
    if not code: return None
    best = None
    for r in team_elo_lookup[tn]:
        if r['date'] >= match_date: break
        if r['ca'] == code: best = r['ea']
        elif r['cb'] == code: best = r['eb']
    return best

# ============================================================
# HAUPTPROGRAMM
# ============================================================

if __name__ == '__main__':
    print("=" * 70)
    print("RANDOM FOREST MODELL")
    print("=" * 70)

    matches = pd.read_csv(MATCH_CSV)
    matches = matches[matches['is_senior_mens'] == True]
    matches = matches.dropna(subset=['home_score', 'away_score'])
    matches = matches.sort_values('date').reset_index(drop=True)
    matches['home_team'] = matches['home_team'].str.replace('&amp;', '&')
    matches['away_team'] = matches['away_team'].str.replace('&amp;', '&')
    print(f"Senior-Spiele: {len(matches)}")

    team_elo_lookup, team_code_map = load_tsv_elo(TSV_DIR)
    print(f"TSV-Teams: {len(team_elo_lookup)}")

    all_teams = set(matches['home_team'].unique()) | set(matches['away_team'].unique())
    elo = {t: get_real_elo(t, START_DATE, team_elo_lookup, team_code_map) or 1500
           for t in all_teams}
    team_hist = {t: [] for t in all_teams}
    dataset = []

    for _, row in matches.iterrows():
        home, away = row['home_team'], row['away_team']
        has_xg = pd.notna(row['ALL_home_expectedGoals'])
        v = row.get('match_venue', 'home')
        if pd.isna(v): v = 'home'

        if row['date'] >= START_DATE:
            hf = get_rolling_features(team_hist[home], 'h_')
            af = get_rolling_features(team_hist[away], 'a_')
            rh = get_real_elo(home, row['date'], team_elo_lookup, team_code_map)
            ra = get_real_elo(away, row['date'], team_elo_lookup, team_code_map)

            if hf and af:
                feat = {**hf, **af}
                feat['elo_h'] = elo[home]
                feat['elo_a'] = elo[away]
                feat['elo_d'] = elo[home] - elo[away]
                feat['is_home'] = 1 if v in ['home', 'host_home'] else 0
                feat['is_neutral'] = 1 if v == 'neutral' else 0
                feat['k'] = get_k_factor(str(row['tournament']))
                feat['tgt_h'] = row['home_score']
                feat['tgt_a'] = row['away_score']
                feat['date'] = row['date']
                feat['home'] = home
                feat['away'] = away
                feat['venue'] = v
                feat['relo_h'] = rh
                feat['relo_a'] = ra
                feat['has_real'] = rh is not None and ra is not None
                dataset.append(feat)

        hs = {f's_{s}': row[f'ALL_home_{s}'] for s in STAT_FEATURES
              if pd.notna(row.get(f'ALL_home_{s}'))}
        as_ = {f's_{s}': row[f'ALL_away_{s}'] for s in STAT_FEATURES
               if pd.notna(row.get(f'ALL_away_{s}'))}
        xhv = row['ALL_home_expectedGoals'] if has_xg else None
        xav = row['ALL_away_expectedGoals'] if has_xg else None

        team_hist[home].append({'gf': row['home_score'], 'ga': row['away_score'],
                                'xf': xhv, 'xa': xav, **hs})
        team_hist[away].append({'gf': row['away_score'], 'ga': row['home_score'],
                                'xf': xav, 'xa': xhv, **as_})

        K = get_k_factor(str(row['tournament']))
        G = goal_diff_multiplier(row['home_score'] - row['away_score'])
        b = home_bonus(v)
        act = 1.0 if row['home_score'] > row['away_score'] else \
              (0.0 if row['home_score'] < row['away_score'] else 0.5)
        exp = expected_score(elo[home] + b, elo[away])
        elo[home] += K * G * (act - exp)
        elo[away] += K * G * ((1 - act) - (1 - exp))

    # --- Train/Test Split ---
    df = pd.DataFrame(dataset)
    split = int(len(df) * TRAIN_SPLIT)
    train = df.iloc[:split]
    test = df.iloc[split:]

    meta = ['date', 'home', 'away', 'tgt_h', 'tgt_a',
            'relo_h', 'relo_a', 'has_real', 'venue']
    fcols = [c for c in df.columns if c not in meta]

    imp = SimpleImputer(strategy='median')
    X_train = pd.DataFrame(imp.fit_transform(train[fcols]), columns=fcols)
    X_test = pd.DataFrame(imp.transform(test[fcols]), columns=fcols)

    print(f"Features: {len(fcols)}, Train: {len(train)}, Test: {len(test)}")

    # --- Random Forest trainieren ---
    print("\nTrainiere Random Forest...")
    rf_h = RandomForestRegressor(n_estimators=RF_TREES, max_depth=RF_MAX_DEPTH,
                                  min_samples_leaf=RF_MIN_SAMPLES_LEAF,
                                  random_state=42, n_jobs=-1)
    rf_a = RandomForestRegressor(n_estimators=RF_TREES, max_depth=RF_MAX_DEPTH,
                                  min_samples_leaf=RF_MIN_SAMPLES_LEAF,
                                  random_state=42, n_jobs=-1)
    rf_h.fit(X_train, train['tgt_h'].values)
    rf_a.fit(X_train, train['tgt_a'].values)

    pred_h = rf_h.predict(X_test)
    pred_a = rf_a.predict(X_test)
    rf_preds = np.array([poisson_win_prob(h, a) for h, a in zip(pred_h, pred_a)])

    # ---  eloratings.net Baseline (mit korrektem Venue) ---
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

    mask = ~np.isnan(real_preds)

    # --- Ergebnisse ---
    print(f"\n{'='*70}")
    print("ERGEBNISSE")
    print(f"{'='*70}")

    brier_real = ((real_preds[mask] - actuals[mask]) ** 2).mean()
    brier_rf = ((rf_preds[mask] - actuals[mask]) ** 2).mean()

    print(f"\n  Alle Spiele mit verfügbarer Elo ({mask.sum()}):")
    print(f"    eloratings.net: {brier_real:.4f}")
    print(f"    Random Forest:  {brier_rf:.4f} ({(brier_rf-brier_real)/brier_real*100:+.1f}%)")

    min_elo = test[mask][['relo_h', 'relo_a']].min(axis=1).values
    for threshold in [1600, 1700, 1800]:
        level = min_elo >= threshold
        if level.sum() < 10: continue
        br = ((real_preds[mask][level] - actuals[mask][level]) ** 2).mean()
        bm = ((rf_preds[mask][level] - actuals[mask][level]) ** 2).mean()
        print(f"\n  Beide ≥{threshold} ({level.sum()} Spiele):")
        print(f"    eloratings.net: {br:.4f}")
        print(f"    Random Forest:  {bm:.4f} ({(bm-br)/br*100:+.1f}%)")

    # --- Export für DM-Test in R ---
    export = test[mask].copy()
    export['se_real'] = (real_preds[mask] - actuals[mask]) ** 2
    export['se_rf'] = (rf_preds[mask] - actuals[mask]) ** 2
    export['min_elo'] = min_elo
    export.to_csv('dm_rf_data.csv', index=False)
    print(f"\nExportiert: dm_rf_data.csv")