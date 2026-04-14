"""
Poisson + xG Modell für Länderspiel-Vorhersagen
=================================================
Berechnet erwartete Tore pro Team basierend auf:
- Elo-Differenz (eloratings.net Methodik)
- Rolling xG-Durchschnitt der letzten 30 Spiele (offensiv + defensiv)

Gewinnwahrscheinlichkeiten über Poisson-Verteilung.
Evaluation gegen eloratings.net Baseline aus GitHub-TSV-Dateien.

Voraussetzungen:
    pip install pandas numpy scipy
"""

import pandas as pd
import numpy as np
from scipy.stats import poisson
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
POISSON_WINDOW = 30      # Rolling-Fenster für xG-Durchschnitte
AVG_GOALS = 1.3          # Durchschnitt Tore pro Team im Länderspiel
ELO_XG_WEIGHT = 0.5      # 50% Elo, 50% xG-Historie
HOME_ADVANTAGE = 100
TRAIN_SPLIT = 0.8

# Sofascore → TSV Dateiname Mapping
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
    """K-Faktor nach eloratings.net Gewichtung."""
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
    """Konvertiere erwartete Tore in P(Heimsieg) + 0.5*P(Unentschieden)."""
    p_home = p_draw = 0
    for gh in range(max_goals + 1):
        for ga in range(max_goals + 1):
            p = poisson.pmf(gh, max(lambda_h, 0.05)) * poisson.pmf(ga, max(lambda_a, 0.05))
            if gh > ga: p_home += p
            elif gh == ga: p_draw += p
    return p_home + 0.5 * p_draw

def avg_from_history(history, key):
    """Durchschnitt eines Werts aus der Spielhistorie (None ignorieren)."""
    vals = [h[key] for h in history if h[key] is not None]
    return np.mean(vals) if vals else None

# ============================================================
# ELORATINGS.NET DATEN LADEN (GitHub TSV)
# ============================================================

def load_tsv_elo(tsv_dir):
    """Lade eloratings.net Elo-Werte aus TSV-Dateien."""
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
    """Hole eloratings.net Elo-Wert VOR dem Spiel."""
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
    print("POISSON + xG MODELL")
    print("=" * 70)

    # --- Daten laden ---
    matches = pd.read_csv(MATCH_CSV)
    matches = matches[matches['is_senior_mens'] == True]
    matches = matches.dropna(subset=['home_score', 'away_score'])
    matches = matches.sort_values('date').reset_index(drop=True)
    matches['home_team'] = matches['home_team'].str.replace('&amp;', '&')
    matches['away_team'] = matches['away_team'].str.replace('&amp;', '&')
    print(f"Senior-Spiele: {len(matches)}")

    # --- TSV Elo laden ---
    team_elo_lookup, team_code_map = load_tsv_elo(TSV_DIR)
    print(f"TSV-Teams: {len(team_elo_lookup)}")

    # --- Modell laufen lassen ---
    all_teams = set(matches['home_team'].unique()) | set(matches['away_team'].unique())
    elo = {t: get_real_elo(t, START_DATE, team_elo_lookup, team_code_map) or 1500
           for t in all_teams}
    xg_hist = {t: [] for t in all_teams}

    records = []
    match_count = 0

    for _, row in matches.iterrows():
        home, away = row['home_team'], row['away_team']
        has_xg = pd.notna(row['ALL_home_expectedGoals'])
        v = row.get('match_venue', 'home')
        if pd.isna(v): v = 'home'
        b = home_bonus(v)
        K = get_k_factor(str(row['tournament']))
        G = goal_diff_multiplier(row['home_score'] - row['away_score'])
        act_h = 1.0 if row['home_score'] > row['away_score'] else \
                (0.0 if row['home_score'] < row['away_score'] else 0.5)

        if row['date'] >= START_DATE:
            match_count += 1

            # --- Poisson + xG Vorhersage ---
            h_hist = xg_hist[home][-POISSON_WINDOW:]
            a_hist = xg_hist[away][-POISSON_WINDOW:]

            elo_diff = (elo[home] - elo[away] + b) / 400

            # Erwartete Tore Heim
            lambda_h = AVG_GOALS * np.exp(0.3 * elo_diff)
            home_xgf = avg_from_history(h_hist, 'xf')
            away_xga = avg_from_history(a_hist, 'xa')
            if home_xgf is not None and away_xga is not None:
                xg_factor = (home_xgf + away_xga) / 2 / AVG_GOALS
                lambda_h *= (ELO_XG_WEIGHT + (1 - ELO_XG_WEIGHT) * xg_factor)

            # Erwartete Tore Gast
            lambda_a = AVG_GOALS * np.exp(-0.3 * elo_diff)
            away_xgf = avg_from_history(a_hist, 'xf')
            home_xga = avg_from_history(h_hist, 'xa')
            if away_xgf is not None and home_xga is not None:
                xg_factor = (away_xgf + home_xga) / 2 / AVG_GOALS
                lambda_a *= (ELO_XG_WEIGHT + (1 - ELO_XG_WEIGHT) * xg_factor)

            lambda_h = np.clip(lambda_h, 0.1, 5.0)
            lambda_a = np.clip(lambda_a, 0.1, 5.0)

            pred_poisson = poisson_win_prob(lambda_h, lambda_a)

            # eloratings.net Vorhersage
            rh = get_real_elo(home, row['date'], team_elo_lookup, team_code_map)
            ra = get_real_elo(away, row['date'], team_elo_lookup, team_code_map)
            has_real = rh is not None and ra is not None
            pred_real = expected_score(rh + b, ra) if has_real else np.nan

            records.append({
                'date': row['date'],
                'home': home, 'away': away,
                'actual': act_h,
                'pred_poisson': pred_poisson,
                'pred_real': pred_real,
                'has_real': has_real,
                'relo_h': rh, 'relo_a': ra,
                'lambda_h': lambda_h, 'lambda_a': lambda_a,
                'match_idx': match_count,
            })

        # History updaten (auch vor START_DATE)
        xhv = row['ALL_home_expectedGoals'] if has_xg else None
        xav = row['ALL_away_expectedGoals'] if has_xg else None
        xg_hist[home].append({'xf': xhv, 'xa': xav})
        xg_hist[away].append({'xf': xav, 'xa': xhv})

        # Elo updaten
        exp = expected_score(elo[home] + b, elo[away])
        elo[home] += K * G * (act_h - exp)
        elo[away] += K * G * ((1 - act_h) - (1 - exp))

    # --- Train/Test Split ---
    df = pd.DataFrame(records)
    split = int(len(df) * TRAIN_SPLIT)
    test = df.iloc[split:]

    mask = test['has_real'].values & ~np.isnan(test['pred_real'].values)
    test_real = test[mask]

    print(f"Spiele ab {START_DATE}: {len(df)}")
    print(f"Test: {len(test)}, davon mit verfügbarer Elo: {mask.sum()}")

    # --- Ergebnisse ---
    print(f"\n{'='*70}")
    print("ERGEBNISSE")
    print(f"{'='*70}")

    actuals = test_real['actual'].values
    p_real = test_real['pred_real'].values
    p_poi = test_real['pred_poisson'].values

    brier_real = ((p_real - actuals) ** 2).mean()
    brier_poi = ((p_poi - actuals) ** 2).mean()

    print(f"\n  Alle Spiele mit verfügbarer Elo ({len(test_real)}):")
    print(f"    eloratings.net: {brier_real:.4f}")
    print(f"    Poisson + xG:   {brier_poi:.4f} ({(brier_poi-brier_real)/brier_real*100:+.1f}%)")

    # Nach Spielniveau
    min_elo = test_real[['relo_h', 'relo_a']].min(axis=1).values
    for threshold in [1600, 1700, 1800]:
        level = min_elo >= threshold
        if level.sum() < 10: continue
        br = ((p_real[level] - actuals[level]) ** 2).mean()
        bp = ((p_poi[level] - actuals[level]) ** 2).mean()
        print(f"\n  Beide ≥{threshold} ({level.sum()} Spiele):")
        print(f"    eloratings.net: {br:.4f}")
        print(f"    Poisson + xG:   {bp:.4f} ({(bp-br)/br*100:+.1f}%)")

    # --- Export für DM-Test in R ---
    export = test_real.copy()
    export['se_real'] = (p_real - actuals) ** 2
    export['se_poisson'] = (p_poi - actuals) ** 2
    export['min_elo'] = min_elo
    export.to_csv('dm_poisson_data.csv', index=False)
    print(f"\nExportiert: dm_poisson_data.csv")