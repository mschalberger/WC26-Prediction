#include <Rcpp.h>
#include <vector>
#include <string>
#include <unordered_map>
#include <algorithm>
#include <numeric>
#include <random>

using namespace Rcpp;

// ─────────────────────────────────────────────────────────────────────────────
// Internal helpers
// ─────────────────────────────────────────────────────────────────────────────

struct MatchResult {
  int home_goals;
  int away_goals;
  int outcome; // 1=home win, 0=draw, -1=away win
};

struct KOResult {
  int winner, loser;
  int winner_goals, loser_goals;
};

MatchResult sample_score(const NumericMatrix& mat, std::mt19937& rng) {
  int n = mat.nrow();
  int total = n * n;
  std::vector<double> flat(total);
  for (int i = 0; i < n; ++i)
    for (int j = 0; j < n; ++j)
      flat[i * n + j] = mat(i, j);
  std::discrete_distribution<int> dist(flat.begin(), flat.end());
  int idx = dist(rng);
  int hg = idx / n;
  int ag = idx % n;
  int outcome = (hg > ag) ? 1 : (hg < ag) ? -1 : 0;
  return {hg, ag, outcome};
}

// Penalty shootout: shrink home win probability toward 0.5
// alpha=1.0 → full 90-min strength; alpha=0.0 → pure 50/50
int resolve_penalties(double p_home_wins, double alpha,
                      std::mt19937& rng, int team_h, int team_a) {
  double p_pen = 0.5 + alpha * (p_home_wins - 0.5);
  std::uniform_real_distribution<double> u(0.0, 1.0);
  return (u(rng) < p_pen) ? team_h : team_a;
}

// ─────────────────────────────────────────────────────────────────────────────
// Group stage
// ─────────────────────────────────────────────────────────────────────────────

struct TeamStat {
  int id;
  int pts = 0, gf = 0, ga = 0;
  double tiebreak_rng = 0.0;
};

std::vector<TeamStat> simulate_group(
    const std::vector<int>& group_ids,
    const std::unordered_map<std::string, NumericMatrix>& prob_map,
    std::mt19937& rng)
{
  std::unordered_map<int, TeamStat> stats;
  for (int id : group_ids) {
    stats[id].id = id;
    std::uniform_real_distribution<double> u(0.0, 1.0);
    stats[id].tiebreak_rng = u(rng);
  }

  for (int i = 0; i < (int)group_ids.size(); ++i) {
    for (int j = i + 1; j < (int)group_ids.size(); ++j) {
      int h = group_ids[i], a = group_ids[j];
      std::string key = std::to_string(h) + "_" + std::to_string(a);
      auto it = prob_map.find(key);

      MatchResult res;
      if (it != prob_map.end()) {
        res = sample_score(it->second, rng);
      } else {
        key = std::to_string(a) + "_" + std::to_string(h);
        it  = prob_map.find(key);
        if (it == prob_map.end()) continue;
        res = sample_score(it->second, rng);
        int tmp = res.home_goals; res.home_goals = res.away_goals;
        res.away_goals = tmp; res.outcome = -res.outcome;
      }

      stats[h].gf += res.home_goals; stats[h].ga += res.away_goals;
      stats[a].gf += res.away_goals; stats[a].ga += res.home_goals;
      if      (res.outcome ==  1) stats[h].pts += 3;
      else if (res.outcome == -1) stats[a].pts += 3;
      else { stats[h].pts += 1; stats[a].pts += 1; }
    }
  }

  std::vector<TeamStat> result;
  result.reserve(group_ids.size());
  for (int id : group_ids) result.push_back(stats[id]);

  std::sort(result.begin(), result.end(), [](const TeamStat& a, const TeamStat& b) {
    if (a.pts != b.pts) return a.pts > b.pts;
    int gda = a.gf - a.ga, gdb = b.gf - b.ga;
    if (gda != gdb) return gda > gdb;
    if (a.gf != b.gf) return a.gf > b.gf;
    return a.tiebreak_rng > b.tiebreak_rng;
  });

  return result;
}

// ─────────────────────────────────────────────────────────────────────────────
// Knockout round
// ─────────────────────────────────────────────────────────────────────────────

KOResult simulate_ko_match(
    int team_h, int team_a,
    const std::unordered_map<std::string, NumericMatrix>& prob_map,
    std::mt19937& rng,
    double pen_alpha = 0.35)
{
  std::string key = std::to_string(team_h) + "_" + std::to_string(team_a);
  auto it = prob_map.find(key);

  MatchResult res;
  bool swapped = false;

  if (it != prob_map.end()) {
    res = sample_score(it->second, rng);
  } else {
    key = std::to_string(team_a) + "_" + std::to_string(team_h);
    it  = prob_map.find(key);
    if (it != prob_map.end()) {
      res = sample_score(it->second, rng);
      int tmp = res.home_goals; res.home_goals = res.away_goals;
      res.away_goals = tmp; res.outcome = -res.outcome;
      swapped = true;
    } else {
      res = {0, 0, 0};
    }
  }

  int winner, loser;
  int winner_goals, loser_goals;

  if (res.outcome == 1) {
    winner = team_h;        loser  = team_a;
    winner_goals = res.home_goals; loser_goals = res.away_goals;
  } else if (res.outcome == -1) {
    winner = team_a;        loser  = team_h;
    winner_goals = res.away_goals; loser_goals = res.home_goals;
  } else {
    // Draw → penalties; goals reflect the drawn scoreline
    double p_home_wins = 0.5;
    if (it != prob_map.end()) {
      const NumericMatrix& m = it->second;
      int n = m.nrow();
      double p_hw = 0.0;
      for (int i = 1; i < n; ++i)
        for (int j = 0; j < i; ++j)
          p_hw += m(i, j);
      p_home_wins = swapped ? (1.0 - p_hw) : p_hw;
    }
    winner = resolve_penalties(p_home_wins, pen_alpha, rng, team_h, team_a);
    loser  = (winner == team_h) ? team_a : team_h;
    // Both teams scored equally in the draw; assign home/away goals accordingly
    winner_goals = (winner == team_h) ? res.home_goals : res.away_goals;
    loser_goals  = (loser  == team_h) ? res.home_goals : res.away_goals;
  }

  return {winner, loser, winner_goals, loser_goals};
}

// ─────────────────────────────────────────────────────────────────────────────
// Third-place ranking (best 8 of 12)
// ─────────────────────────────────────────────────────────────────────────────

std::vector<int> select_best_thirds(const std::vector<TeamStat>& thirds) {
  std::vector<int> idx(thirds.size());
  std::iota(idx.begin(), idx.end(), 0);
  std::sort(idx.begin(), idx.end(), [&](int a, int b) {
    const TeamStat& ta = thirds[a]; const TeamStat& tb = thirds[b];
    if (ta.pts != tb.pts) return ta.pts > tb.pts;
    int gda = ta.gf - ta.ga, gdb = tb.gf - tb.ga;
    if (gda != gdb) return gda > gdb;
    if (ta.gf != tb.gf) return ta.gf > tb.gf;
    return false;
  });
  std::vector<int> best8;
  for (int k = 0; k < 8 && k < (int)idx.size(); ++k)
    best8.push_back(thirds[idx[k]].id);
  return best8;
}

// ─────────────────────────────────────────────────────────────────────────────
// Single tournament
// ─────────────────────────────────────────────────────────────────────────────

// [[Rcpp::export]]
List simulate_tournament_cpp(
    List groups_list,
    List prob_list,
    int  seed = -1,
    double pen_alpha = 0.35)
{
  std::mt19937 rng(seed < 0 ? std::random_device{}() : (unsigned)seed);

  std::unordered_map<std::string, NumericMatrix> prob_map;
  CharacterVector prob_names = prob_list.names();
  for (int i = 0; i < prob_list.size(); ++i)
    prob_map[as<std::string>(prob_names[i])] = as<NumericMatrix>(prob_list[i]);

  CharacterVector grp_names = groups_list.names();
  int n_groups = groups_list.size();

  std::vector<std::vector<TeamStat>> standings(n_groups);
  std::vector<std::string> grp_labels(n_groups);
  std::unordered_map<std::string, int> grp_idx;

  for (int g = 0; g < n_groups; ++g) {
    grp_labels[g] = as<std::string>(grp_names[g]);
    grp_idx[grp_labels[g]] = g;
    IntegerVector ids = as<IntegerVector>(groups_list[g]);
    std::vector<int> id_vec(ids.begin(), ids.end());
    standings[g] = simulate_group(id_vec, prob_map, rng);
  }

  // ── Group winners ─────────────────────────────────────────
  std::vector<int> group_winners(n_groups);
  for (int g = 0; g < n_groups; ++g)
    group_winners[g] = standings[g][0].id;

  // ── Build per-team accumulators (group stage baseline) ────
  // All teams indexed by id
  std::unordered_map<int, int> acc_gf, acc_ga, acc_gp;
  for (int g = 0; g < n_groups; ++g) {
    for (int r = 0; r < 4; ++r) {
      int id = standings[g][r].id;
      acc_gf[id] = standings[g][r].gf;
      acc_ga[id] = standings[g][r].ga;
      acc_gp[id] = 3;
    }
  }

  // Helper: record a KO match result into accumulators
  auto record_ko = [&](const KOResult& kr) {
    acc_gf[kr.winner] += kr.winner_goals;
    acc_ga[kr.winner] += kr.loser_goals;
    acc_gf[kr.loser]  += kr.loser_goals;
    acc_ga[kr.loser]  += kr.winner_goals;
    acc_gp[kr.winner]++;
    acc_gp[kr.loser]++;
  };

  // ── Elimination lists ─────────────────────────────────────
  std::vector<int> group_out;
  std::vector<int> r32_losers, r16_losers, qf_losers, sf_losers_vec;
  int finalist = -1, champion = -1;

  for (int g = 0; g < n_groups; ++g)
    group_out.push_back(standings[g][3].id);

  std::vector<TeamStat> thirds(n_groups);
  for (int g = 0; g < n_groups; ++g)
    thirds[g] = standings[g][2];

  std::vector<int> best8 = select_best_thirds(thirds);

  for (int g = 0; g < n_groups; ++g) {
    int tid = standings[g][2].id;
    bool advances = std::find(best8.begin(), best8.end(), tid) != best8.end();
    if (!advances) group_out.push_back(tid);
  }

  auto gt = [&](const std::string& grp, int rank) -> int {
    int gi = grp_idx.count(grp) ? grp_idx[grp] : -1;
    if (gi < 0 || rank >= (int)standings[gi].size()) return -1;
    return standings[gi][rank].id;
  };

  // ── Round of 32 ───────────────────────────────────────────
  std::vector<std::pair<int,int>> r32_pairs = {
    {gt("A",1), gt("B",1)},
    {gt("C",0), gt("F",1)},
    {gt("E",0), best8[0]},
    {gt("F",0), gt("C",1)},
    {gt("E",1), gt("I",1)},
    {gt("I",0), best8[1]},
    {gt("A",0), best8[2]},
    {gt("L",0), best8[3]},
    {gt("G",0), best8[4]},
    {gt("D",0), best8[5]},
    {gt("H",0), gt("J",1)},
    {gt("K",1), gt("L",1)},
    {gt("B",0), best8[6]},
    {gt("D",1), gt("G",1)},
    {gt("J",0), gt("H",1)},
    {gt("K",0), best8[7]}
  };

  std::vector<int> r32_winners;
  for (auto& p : r32_pairs) {
    if (p.first < 0 || p.second < 0) { r32_winners.push_back(-1); continue; }
    KOResult kr = simulate_ko_match(p.first, p.second, prob_map, rng, pen_alpha);
    record_ko(kr);
    r32_winners.push_back(kr.winner);
    r32_losers.push_back(kr.loser);
  }

  // ── Round of 16 ───────────────────────────────────────────
  std::vector<int> r16_winners;
  for (int i = 0; i < 16; i += 2) {
    if (r32_winners[i] < 0 || r32_winners[i+1] < 0) {
      r16_winners.push_back(-1); continue;
    }
    KOResult kr = simulate_ko_match(r32_winners[i], r32_winners[i+1], prob_map, rng, pen_alpha);
    record_ko(kr);
    r16_winners.push_back(kr.winner);
    r16_losers.push_back(kr.loser);
  }

  // ── Quarter-finals ────────────────────────────────────────
  std::vector<int> qf_winners;
  for (int i = 0; i < 8; i += 2) {
    if (r16_winners[i] < 0 || r16_winners[i+1] < 0) {
      qf_winners.push_back(-1); continue;
    }
    KOResult kr = simulate_ko_match(r16_winners[i], r16_winners[i+1], prob_map, rng, pen_alpha);
    record_ko(kr);
    qf_winners.push_back(kr.winner);
    qf_losers.push_back(kr.loser);
  }

  // ── Semi-finals ───────────────────────────────────────────
  std::vector<int> sf_winners;
  for (int i = 0; i < 4; i += 2) {
    if (qf_winners[i] < 0 || qf_winners[i+1] < 0) {
      sf_winners.push_back(-1); sf_losers_vec.push_back(-1); continue;
    }
    KOResult kr = simulate_ko_match(qf_winners[i], qf_winners[i+1], prob_map, rng, pen_alpha);
    record_ko(kr);
    sf_winners.push_back(kr.winner);
    sf_losers_vec.push_back(kr.loser);
  }

  // ── Final ─────────────────────────────────────────────────
  if ((int)sf_winners.size() >= 2 && sf_winners[0] > 0 && sf_winners[1] > 0) {
    KOResult kr = simulate_ko_match(sf_winners[0], sf_winners[1], prob_map, rng, pen_alpha);
    record_ko(kr);
    champion = kr.winner;
    finalist = kr.loser;
  }

  // ── Pack goals/games into output vectors ──────────────────
  int n_teams = n_groups * 4;
  IntegerVector goals_id_vec(n_teams), goals_for_vec(n_teams),
  goals_ag_vec(n_teams), games_vec(n_teams);

  int k = 0;
  for (int g = 0; g < n_groups; ++g) {
    for (int r = 0; r < 4; ++r, ++k) {
      int id = standings[g][r].id;
      goals_id_vec[k]  = id;
      goals_for_vec[k] = acc_gf[id];
      goals_ag_vec[k]  = acc_ga[id];
      games_vec[k]     = acc_gp[id];
    }
  }

  return List::create(
    Named("group_out")     = wrap(group_out),
    Named("r32_losers")    = wrap(r32_losers),
    Named("r16_losers")    = wrap(r16_losers),
    Named("qf_losers")     = wrap(qf_losers),
    Named("sf_losers")     = wrap(sf_losers_vec),
    Named("finalist")      = finalist,
    Named("champion")      = champion,
    Named("group_winners") = wrap(group_winners),
    Named("group_labels")  = wrap(grp_labels),
    Named("goals_id")      = goals_id_vec,
    Named("goals_for")     = goals_for_vec,
    Named("goals_ag")      = goals_ag_vec,
    Named("games_played")  = games_vec
  );
}

// ─────────────────────────────────────────────────────────────────────────────
// Monte Carlo wrapper
// ─────────────────────────────────────────────────────────────────────────────

// [[Rcpp::export]]
List run_mc_cpp(
    List   groups_list,
    List   prob_list,
    int    n_sims,
    int    seed_base  = 42,
    double pen_alpha  = 0.35)
{
  std::mt19937 seed_rng(seed_base);
  std::uniform_int_distribution<int> seed_dist(0, 1e8);

  std::vector<int> all_ids;
  CharacterVector grp_names = groups_list.names();
  int n_groups = groups_list.size();

  std::unordered_map<int, std::string> team_group;
  for (int g = 0; g < n_groups; ++g) {
    IntegerVector ids = as<IntegerVector>(groups_list[g]);
    std::string glabel = as<std::string>(grp_names[g]);
    for (int id : ids) {
      all_ids.push_back(id);
      team_group[id] = glabel;
    }
  }

  // Stage counters: 0=GroupOut, 1=R32, 2=R16, 3=QF, 4=SF, 5=Final, 6=Champion
  std::unordered_map<int, std::array<int,7>> elim;
  std::unordered_map<int, int> gwin;
  std::unordered_map<int, int> total_gf;
  std::unordered_map<int, int> total_ga;
  std::unordered_map<int, int> total_gp;

  for (int id : all_ids) {
    elim[id]     = {0,0,0,0,0,0,0};
    gwin[id]     = 0;
    total_gf[id] = 0;
    total_ga[id] = 0;
    total_gp[id] = 0;
  }

  for (int sim = 0; sim < n_sims; ++sim) {
    int s = seed_dist(seed_rng);
    List r = simulate_tournament_cpp(groups_list, prob_list, s, pen_alpha);

    IntegerVector go  = r["group_out"];
    IntegerVector r32 = r["r32_losers"];
    IntegerVector r16 = r["r16_losers"];
    IntegerVector qf  = r["qf_losers"];
    IntegerVector sf  = r["sf_losers"];
    int fin  = as<int>(r["finalist"]);
    int chmp = as<int>(r["champion"]);

    // ── Stage elimination counts ──────────────────────────────
    for (int id : go)  if (elim.count(id)) elim[id][0]++;
    for (int id : r32) if (elim.count(id)) elim[id][1]++;
    for (int id : r16) if (elim.count(id)) elim[id][2]++;
    for (int id : qf)  if (elim.count(id)) elim[id][3]++;
    for (int id : sf)  if (elim.count(id)) elim[id][4]++;
    if (fin  > 0 && elim.count(fin))  elim[fin][5]++;
    if (chmp > 0 && elim.count(chmp)) elim[chmp][6]++;

    // ── Group winners ─────────────────────────────────────────
    IntegerVector gw = r["group_winners"];
    for (int id : gw)
      if (gwin.count(id)) gwin[id]++;

      // ── Goals and games (all stages, from simulate_tournament_cpp) ──
      IntegerVector g_ids = r["goals_id"];
      IntegerVector g_for = r["goals_for"];
      IntegerVector g_ag  = r["goals_ag"];
      IntegerVector g_gp  = r["games_played"];

      for (int k = 0; k < g_ids.size(); ++k) {
        int id = g_ids[k];
        if (total_gf.count(id)) {
          total_gf[id] += g_for[k];
          total_ga[id] += g_ag[k];
          total_gp[id] += g_gp[k];
        }
      }
  }

  int n = all_ids.size();
  IntegerVector ids_out(n);
  NumericVector p_group(n), p_r32(n), p_r16(n), p_qf(n),
  p_sf(n), p_final(n), p_champ(n);
  NumericVector p_gwin(n);
  NumericVector avg_gf(n), avg_ga(n), avg_gp(n);

  for (int i = 0; i < n; ++i) {
    int id    = all_ids[i];
    ids_out[i] = id;
    double ns  = (double)n_sims;

    double pg   = elim[id][0] / ns;
    double pr32 = elim[id][1] / ns;
    double pr16 = elim[id][2] / ns;
    double pqf  = elim[id][3] / ns;
    double psf  = elim[id][4] / ns;
    double pch  = elim[id][6] / ns;

    p_group[i] = 1.0;
    p_r32[i]   = 1.0 - pg;
    p_r16[i]   = p_r32[i]  - pr32;
    p_qf[i]    = p_r16[i]  - pr16;
    p_sf[i]    = p_qf[i]   - pqf;
    p_final[i] = p_sf[i]   - psf;
    p_champ[i] = pch;
    p_gwin[i]  = gwin[id]  / ns;

    double gp_total = (double)total_gp[id];
    avg_gf[i]  = gp_total > 0 ? (double)total_gf[id] / gp_total : 0.0;
    avg_ga[i]  = gp_total > 0 ? (double)total_ga[id] / gp_total : 0.0;
    avg_gp[i]  = gp_total / ns;
  }

  return List::create(
    Named("id")              = ids_out,
    Named("Group.Stage")     = p_group,
    Named("Round.of.32")     = p_r32,
    Named("Round.of.16")     = p_r16,
    Named("Quarter.Final")   = p_qf,
    Named("Semi.Final")      = p_sf,
    Named("Final")           = p_final,
    Named("Champion")        = p_champ,
    Named("GroupWinner")     = p_gwin,
    Named("avg_gf_per_game") = avg_gf,
    Named("avg_ga_per_game") = avg_ga,
    Named("avg_games")       = avg_gp,
    Named("n_sims")          = n_sims
  );
}

