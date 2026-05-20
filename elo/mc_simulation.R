library(dplyr)
library(tidyr)
library(Rcpp)

# ── CONFIG ───────────────────────────────────────────────────

N_SIMS       <- 50000
SEED         <- 42
K            <- 60
USE_HIST     <- TRUE
GERMANY_NAME <- "Germany"
TOP_N        <- 10

# ── DATA LOADING ─────────────────────────────────────────────

load_data <- function() {
  teams   <- read.csv("data/teams.csv", stringsAsFactors = FALSE)
  elo_raw <- read.delim("https://www.eloratings.net/World.tsv?_=1776984277329",
                        sep = "\t", header = FALSE)
  ctry    <- read.delim("https://www.eloratings.net/en.teams.tsv?_=1772102421794",
                        sep = "\t", header = FALSE)

  elo <- elo_raw %>%
    left_join(ctry, by = c("V3" = "V1")) %>%
    select(country = V2.y, elo = V4) %>%
    mutate(country = case_when(
      country == "United States"                ~ "USA",
      country == "Iran"                         ~ "IR Iran",
      country == "Cape Verde"                   ~ "Cabo Verde",
      country == "Ivory Coast"                  ~ "Côte d'Ivoire",
      country == "Democratic Republic of Congo" ~ "DR Congo",
      TRUE ~ country
    ))

  teams_init <- teams %>%
    mutate(team_name = case_when(
      team_name == "Winner FIFA Playoff 1" ~ "DR Congo",
      team_name == "Winner FIFA Playoff 2" ~ "Iraq",
      team_name == "Winner UEFA Playoff B" ~ "Sweden",
      team_name == "Winner UEFA Playoff C" ~ "Turkey",
      team_name == "Winner UEFA Playoff A" ~ "Bosnia and Herzegovina",
      team_name == "Winner UEFA Playoff D" ~ "Czechia",
      TRUE ~ team_name
    )) %>%
    left_join(elo, by = c("team_name" = "country"))

  missing <- teams_init %>% filter(is.na(elo)) %>% pull(team_name)
  if (length(missing) > 0) {
    message("⚠️  No ELO for: ", paste(missing, collapse = ", "), " — using median")
    teams_init <- teams_init %>%
      mutate(elo = ifelse(is.na(elo), median(elo, na.rm = TRUE), elo))
  }

  hist_sd <- read.csv("data/score_dist.csv", stringsAsFactors = FALSE)
  list(teams_init = teams_init, hist_sd = hist_sd)
}

# ── FLAGS ────────────────────────────────────────────────────

flag_map <- c(
  MEX="🇲🇽", RSA="🇿🇦", KOR="🇰🇷", CZE="🇨🇿", CAN="🇨🇦", BIH="🇧🇦", QAT="🇶🇦",
  SUI="🇨🇭", BRA="🇧🇷", MAR="🇲🇦", HAI="🇭🇹", SCO="🏴󠁧󠁢󠁳󠁣󠁴󠁿", USA="🇺🇸", PAR="🇵🇾",
  AUS="🇦🇺", TUR="🇹🇷", GER="🇩🇪", CUR="🇨🇼", CIV="🇨🇮", ECU="🇪🇨", NED="🇳🇱",
  JPN="🇯🇵", SWE="🇸🇪", TUN="🇹🇳", BEL="🇧🇪", EGY="🇪🇬", IRN="🇮🇷", NZL="🇳🇿",
  ESP="🇪🇸", CPV="🇨🇻", KSA="🇸🇦", URU="🇺🇾", FRA="🇫🇷", SEN="🇸🇳", IRQ="🇮🇶",
  NOR="🇳🇴", ARG="🇦🇷", ALG="🇩🇿", AUT="🇦🇹", JOR="🇯🇴", POR="🇵🇹", COD="🇨🇩",
  UZB="🇺🇿", COL="🇨🇴", ENG="🏴󠁧󠁢󠁥󠁮󠁧󠁿", CRO="🇭🇷", GHA="🇬🇭", PAN="🇵🇦"
)
get_flag <- function(code) ifelse(is.na(flag_map[code]), "🏳️", unname(flag_map[code]))

# ── ELO HELPER (R — used only by analytical_score_dist) ──────

elo_expected <- function(ea, eb) 1 / (1 + 10^((eb - ea) / 400))

# ── ANALYTICAL SCORE DISTRIBUTION ────────────────────────────

analytical_score_dist <- function(elo_ger, elo_opp, hist_sd) {
  p_raw      <- elo_expected(elo_ger, elo_opp)
  ger_is_fav <- (p_raw >= 0.5)
  p_fav      <- if (ger_is_fav) p_raw else (1 - p_raw)

  draw_p  <- 1/3 * exp(-((p_fav - 0.5)^2) / (2 * 0.236875^2))
  p_fav_w <- p_fav * (1 - draw_p)
  p_und_w <- (1 - p_fav) * (1 - draw_p)

  outcome_weights <- list(
    list(csv = "fav_win", weight = p_fav_w),
    list(csv = "draw",    weight = draw_p),
    list(csv = "und_win", weight = p_und_w)
  )

  rows <- lapply(outcome_weights, function(oc) {
    if (oc$weight == 0) return(NULL)
    bin <- hist_sd %>%
      filter(outcome == oc$csv, p_lo <= p_fav_w, p_fav_w < p_hi)
    if (nrow(bin) == 0)
      bin <- hist_sd %>%
      filter(outcome == oc$csv, p_hi == max(p_hi[outcome == oc$csv]))
    if (nrow(bin) == 0) return(NULL)
    bin %>%
      mutate(
        ger_goals  = if (ger_is_fav) fav_goals else und_goals,
        opp_goals  = if (ger_is_fav) und_goals else fav_goals,
        joint_prob = prob * oc$weight
      ) %>%
      select(ger_goals, opp_goals, joint_prob)
  })

  bind_rows(rows) %>%
    group_by(ger_goals, opp_goals) %>%
    summarise(prob = sum(joint_prob), .groups = "drop") %>%
    mutate(prob = prob / sum(prob))
}

# ── FULL C++ MONTE CARLO CORE ─────────────────────────────────

cppFunction('
#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>
using namespace Rcpp;

inline double elo_exp(double ea, double eb) {
  return 1.0 / (1.0 + std::pow(10.0, (eb - ea) / 400.0));
}
inline double draw_p(double ph) {
  double z = ph - 0.5;
  return (0.3) * std::exp(-(z*z) / (2.0*0.3*0.3));
}

void sample_score(double pfw, int oc_code,
                  const IntegerVector& sd_oc,
                  const NumericVector& sd_plo,
                  const NumericVector& sd_phi,
                  const NumericVector& sd_fg,
                  const NumericVector& sd_ug,
                  const NumericVector& sd_prob,
                  int& fg_out, int& ug_out)
{
  int n = sd_oc.size();
  std::vector<int> idx;
  idx.reserve(16);
  for (int i = 0; i < n; ++i)
    if (sd_oc[i] == oc_code && sd_plo[i] <= pfw && pfw < sd_phi[i])
      idx.push_back(i);
  if (idx.empty()) {
    double mh = -1;
    for (int i = 0; i < n; ++i)
      if (sd_oc[i] == oc_code && sd_phi[i] > mh) mh = sd_phi[i];
    for (int i = 0; i < n; ++i)
      if (sd_oc[i] == oc_code && sd_phi[i] == mh) idx.push_back(i);
  }
  if (idx.empty()) { fg_out = 1; ug_out = 0; return; }
  double wsum = 0;
  for (int j : idx) wsum += sd_prob[j];
  double u = R::runif(0,1) * wsum, cum = 0;
  int pick = idx.back();
  for (int j : idx) { cum += sd_prob[j]; if (u <= cum) { pick = j; break; } }
  fg_out = (int)sd_fg[pick];
  ug_out = (int)sd_ug[pick];
}

void sim_match(double& elo_h, double& elo_a, double k,
               bool use_hist,
               const IntegerVector& sd_oc,
               const NumericVector& sd_plo,
               const NumericVector& sd_phi,
               const NumericVector& sd_fg,
               const NumericVector& sd_ug,
               const NumericVector& sd_prob,
               int& gh, int& ga)
{
  double p_h = elo_exp(elo_h, elo_a);
  double dp  = draw_p(p_h);
  double ph  = p_h * (1.0 - dp);
  double pa  = (1.0 - p_h) * (1.0 - dp);

  double u = R::runif(0,1);
  int outcome = (u < ph) ? 0 : (u < ph + dp) ? 1 : 2;

  bool hf    = (p_h >= 0.5);
  double pfav = hf ? p_h : 1.0 - p_h;
  double pfw  = pfav * (1.0 - draw_p(pfav));
  int oc_code = (outcome == 1) ? 1 : ((outcome == 0) == hf) ? 0 : 2;

  gh = -1; ga = -1;
  if (use_hist) {
    int fg, ug;
    sample_score(pfw, oc_code, sd_oc, sd_plo, sd_phi, sd_fg, sd_ug, sd_prob, fg, ug);
    gh = hf ? fg : ug;
    ga = hf ? ug : fg;
  }
  if (gh < 0) {
    double lh = 1.99419*(p_h) + 0.24629, la = 1.99419*(1-p_h) + 0.24629;
    int att = 0;
    do {
      gh = R::rpois(lh); ga = R::rpois(la); ++att;
      if (att > 1000) break;
    } while (!((outcome==0&&gh>ga)||(outcome==2&&ga>gh)||(outcome==1&&gh==ga)));
  }

  double act_h = (outcome==0)?1.0:(outcome==1?0.5:0.0);
  int    gd    = std::abs(gh - ga);
  double kadj  = k * (gd<=1?1.0:gd==2?1.5:gd==3?1.75:1.75+(gd-3.0)/8.0);
  double delta = kadj * (act_h - p_h);
  elo_h += delta; elo_a -= delta;
}

inline int ko_winner(int id_h, int id_a, double elo_h, double elo_a) {
  return (R::runif(0,1) < elo_exp(elo_h, elo_a)) ? id_h : id_a;
}

// [[Rcpp::export]]
List run_mc_cpp(
    IntegerVector  teams_id,
    NumericVector  teams_elo_init,
    IntegerVector  teams_group,
    int            ger_idx,
    IntegerVector  sd_oc,
    NumericVector  sd_plo,
    NumericVector  sd_phi,
    NumericVector  sd_fg,
    NumericVector  sd_ug,
    NumericVector  sd_prob,
    int n_sims, double k,
    bool use_hist)
{
  int N = teams_id.size();
  const int S = 7;
  const int G = 12;

  // Use flat std::vector<int> for 2-D accumulators: [team * ncol + col]
  std::vector<int> elim(N * S, 0);
  std::vector<int> gwin(N * G, 0);

  std::vector<int> ger_opp_ids, ger_g_vec, ger_opp_g_vec;
  ger_opp_ids.reserve(n_sims * 3);
  ger_g_vec.reserve(n_sims * 3);
  ger_opp_g_vec.reserve(n_sims * 3);

  for (int sim = 0; sim < n_sims; ++sim) {

    std::vector<double> elo(teams_elo_init.begin(), teams_elo_init.end());
    std::vector<int> pts(N,0), gf(N,0), ga_t(N,0);

    // ── Group stage ─────────────────────────────────────────
    for (int grp = 0; grp < G; ++grp) {
      std::vector<int> ids;
      ids.reserve(4);
      for (int i = 0; i < N; ++i) if (teams_group[i] == grp) ids.push_back(i);

      for (int a = 0; a < 4; ++a) {
        for (int b = a+1; b < 4; ++b) {
          int h = ids[a], av = ids[b];
          int gh, ga_m;
          sim_match(elo[h], elo[av], k, use_hist,
                    sd_oc, sd_plo, sd_phi, sd_fg, sd_ug, sd_prob, gh, ga_m);
          gf[h]   += gh;  ga_t[h]  += ga_m;
          gf[av]  += ga_m; ga_t[av] += gh;
          int diff = gh - ga_m;
          if      (diff > 0) pts[h]  += 3;
          else if (diff < 0) pts[av] += 3;
          else { pts[h]++; pts[av]++; }

          if (h == ger_idx || av == ger_idx) {
            int opp = (h == ger_idx) ? av : h;
            int gg  = (h == ger_idx) ? gh   : ga_m;
            int og  = (h == ger_idx) ? ga_m : gh;
            ger_opp_ids.push_back(teams_id[opp]);
            ger_g_vec.push_back(gg);
            ger_opp_g_vec.push_back(og);
          }
        }
      }
    }

    // ── Sort standings ───────────────────────────────────────
    std::vector<std::vector<int>> grp_pos(G, std::vector<int>(4,-1));
    for (int grp = 0; grp < G; ++grp) {
      std::vector<int> ids;
      for (int i = 0; i < N; ++i) if (teams_group[i] == grp) ids.push_back(i);
      std::sort(ids.begin(), ids.end(), [&](int x, int y){
        if (pts[x] != pts[y]) return pts[x] > pts[y];
        int gdx = gf[x]-ga_t[x], gdy = gf[y]-ga_t[y];
        if (gdx != gdy) return gdx > gdy;
        if (gf[x] != gf[y]) return gf[x] > gf[y];
        return elo[x] > elo[y];
      });
      for (int r = 0; r < 4; ++r) grp_pos[grp][r] = ids[r];
      gwin[ids[0] * G + grp]++;
      elim[ids[3] * S + 0]++;   // 4th place out at group stage
    }

    // ── Best 8 of 12 thirds ──────────────────────────────────
    std::vector<int> thirds;
    thirds.reserve(12);
    for (int grp = 0; grp < G; ++grp) thirds.push_back(grp_pos[grp][2]);
    std::sort(thirds.begin(), thirds.end(), [&](int x, int y){
      if (pts[x] != pts[y]) return pts[x] > pts[y];
      int gdx = gf[x]-ga_t[x], gdy = gf[y]-ga_t[y];
      if (gdx != gdy) return gdx > gdy;
      if (gf[x] != gf[y]) return gf[x] > gf[y];
      return elo[x] > elo[y];
    });
    for (int i = 8; i < 12; ++i) elim[thirds[i] * S + 0]++;

    // ── R32 bracket ──────────────────────────────────────────
    #define GP(g,r) grp_pos[g][r]
    std::vector<std::pair<int,int>> r32 = {
      {GP(0,1),GP(1,1)},  {GP(2,0),GP(5,1)},
      {GP(4,0),thirds[0]},{GP(5,0),GP(2,1)},
      {GP(4,1),GP(8,1)},  {GP(8,0),thirds[1]},
      {GP(0,0),thirds[2]},{GP(11,0),thirds[3]},
      {GP(6,0),thirds[4]},{GP(3,0),thirds[5]},
      {GP(7,0),GP(9,1)},  {GP(10,1),GP(11,1)},
      {GP(1,0),thirds[6]},{GP(3,1),GP(6,1)},
      {GP(9,0),GP(7,1)},  {GP(10,0),thirds[7]}
    };
    #undef GP

    // run_round: simulate a list of pairs, return winners, mark losers
    auto run_round = [&](std::vector<std::pair<int,int>>& pairs, int elim_stage)
        -> std::vector<int>
    {
      std::vector<int> winners;
      winners.reserve(pairs.size());
      for (auto& p : pairs) {
        int h = p.first, av = p.second;
        int gh, ga_m;
        sim_match(elo[h], elo[av], k, use_hist,
                  sd_oc, sd_plo, sd_phi, sd_fg, sd_ug, sd_prob, gh, ga_m);
        int w;
        int diff = gh - ga_m;
        if      (diff > 0) w = h;
        else if (diff < 0) w = av;
        else               w = ko_winner(h, av, elo[h], elo[av]);
        int loser = (w == h) ? av : h;
        elim[loser * S + elim_stage]++;
        winners.push_back(w);
      }
      return winners;
    };

    auto make_pairs = [](const std::vector<int>& v) {
      std::vector<std::pair<int,int>> p;
      p.reserve(v.size()/2);
      for (int i = 0; i+1 < (int)v.size(); i+=2)
        p.push_back({v[i], v[i+1]});
      return p;
    };

    auto w32 = run_round(r32, 1);
    auto p16 = make_pairs(w32);
    auto w16 = run_round(p16, 2);
    auto pqf = make_pairs(w16);
    auto wqf = run_round(pqf, 3);
    auto psf = make_pairs(wqf);
    auto wsf = run_round(psf, 4);

    // 3rd-place match
    std::vector<std::pair<int,int>> tp_pair = {
      { (wsf[0]==psf[0].first) ? psf[0].second : psf[0].first,
        (wsf[1]==psf[1].first) ? psf[1].second : psf[1].first }
    };
    run_round(tp_pair, 5);

    // Final
    std::vector<std::pair<int,int>> fin_pair = {{wsf[0], wsf[1]}};
    auto wfin = run_round(fin_pair, 5);   // loser → Final bucket
    elim[wfin[0] * S + 6]++;             // winner → Champion bucket
  }

  // ── Reach probabilities ──────────────────────────────────
  NumericMatrix reach(N, S);
  for (int i = 0; i < N; ++i) {
    reach(i,0) = 1.0;
    reach(i,1) = 1.0 - (double)elim[i*S+0]/n_sims;
    for (int s = 2; s <= 5; ++s)
      reach(i,s) = reach(i,s-1) - (double)elim[i*S+s-1]/n_sims;
    reach(i,6) = (double)elim[i*S+6]/n_sims;
  }

  NumericMatrix gwin_prob(N, G);
  for (int i = 0; i < N; ++i)
    for (int g = 0; g < G; ++g)
      gwin_prob(i,g) = (double)gwin[i*G+g]/n_sims;

  int ng = (int)ger_opp_ids.size();
  IntegerVector go_ids(ng), gg_v(ng), og_v(ng);
  for (int i = 0; i < ng; ++i) {
    go_ids[i] = ger_opp_ids[i];
    gg_v[i]   = ger_g_vec[i];
    og_v[i]   = ger_opp_g_vec[i];
  }

  return List::create(
    Named("reach")     = reach,
    Named("gwin")      = gwin_prob,
    Named("ger_opp")   = go_ids,
    Named("ger_goals") = gg_v,
    Named("opp_goals") = og_v
  );
}
')

# ── MONTE CARLO ───────────────────────────────────────────────

run_mc <- function(n_sims, teams_init, hist_sd,
                   k = 20, use_hist = TRUE, seed_base = 42,
                   germany_name = "Germany") {
  message(sprintf("Running %d simulations...", n_sims))

  sd <- hist_sd %>%
    mutate(oc_int = case_when(outcome == "fav_win" ~ 0L,
                              outcome == "draw"    ~ 1L,
                              TRUE                 ~ 2L))

  grp_ltrs   <- sort(unique(teams_init$group_letter))
  grp_map    <- setNames(seq_along(grp_ltrs) - 1L, grp_ltrs)
  teams_init <- teams_init %>%
    mutate(group_idx = grp_map[group_letter])

  ger_idx <- which(teams_init$team_name == germany_name) - 1L  # 0-based

  set.seed(seed_base)   # ← seed R's RNG once before handing off to C++

  cpp_res <- run_mc_cpp(
    teams_id       = teams_init$id,
    teams_elo_init = teams_init$elo,
    teams_group    = teams_init$group_idx,
    ger_idx        = ger_idx,
    sd_oc          = sd$oc_int,
    sd_plo         = sd$p_lo,
    sd_phi         = sd$p_hi,
    sd_fg          = sd$fav_goals,
    sd_ug          = sd$und_goals,
    sd_prob        = sd$prob,
    n_sims         = n_sims,
    k              = k,
    use_hist       = use_hist
  )

  stages <- c("Group Stage","Round of 32","Round of 16",
              "Quarter-Final","Semi-Final","Final","Champion")

  reach_mat <- cpp_res$reach
  colnames(reach_mat) <- stages
  reach_df <- as.data.frame(reach_mat) %>%
    mutate(id = teams_init$id) %>%
    left_join(teams_init %>% select(id, team_name, fifa_code, group_letter), by = "id")

  gwin_mat <- cpp_res$gwin
  colnames(gwin_mat) <- grp_ltrs
  gwin_df <- as.data.frame(gwin_mat) %>%
    mutate(id = teams_init$id) %>%
    left_join(teams_init %>% select(id, team_name, fifa_code, group_letter), by = "id") %>%
    pivot_longer(cols = all_of(grp_ltrs), names_to = "group_col", values_to = "win_prob") %>%
    filter(group_col == group_letter) %>%
    select(id, team_name, fifa_code, group = group_letter, win_prob)

  id_to_name <- setNames(teams_init$team_name, teams_init$id)
  ger_scores_df <- data.frame(
    opponent  = id_to_name[as.character(cpp_res$ger_opp)],
    ger_goals = cpp_res$ger_goals,
    opp_goals = cpp_res$opp_goals
  )

  list(reach_df      = reach_df,
       gwin_df       = gwin_df,
       ger_scores_df = ger_scores_df,
       n_sims        = n_sims)
}

# ══════════════════════════════════════════════════════════════
# MAIN
# ══════════════════════════════════════════════════════════════

message("Loading data...")
dat <- load_data()

mc <- run_mc(dat$teams_init, dat$hist_sd,
             k         = K,
             use_hist  = USE_HIST,
             seed_base = SEED,
             n_sims    = N_SIMS)
saveRDS(mc, "output/wc2026_mc_results.rds")
