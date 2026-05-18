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

// Match result struct
struct MatchResult {
  int home_goals;
  int away_goals;
  int outcome; // 1=home win, 0=draw, -1=away win
};

// Sample a score from a (maxgoal+1)x(maxgoal+1) probability matrix.
// mat is stored row-major: mat[i][j] = P(home=i, away=j)
MatchResult sample_score(const NumericMatrix& mat, std::mt19937& rng) {
  int n = mat.nrow(); // maxgoal + 1
  int total = n * n;

  // Build flat CDF
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

// In knockout matches a draw goes to penalties — coin-flip weighted by
// the home-win probability derived from the score matrix.
int resolve_draw_ko(int team_h, int team_a, std::mt19937& rng) {
  std::uniform_real_distribution<double> u(0.0, 1.0);
  return (u(rng) < 0.5) ? team_h : team_a;
}

// ─────────────────────────────────────────────────────────────────────────────
// Group stage
// ─────────────────────────────────────────────────────────────────────────────

struct TeamStat {
  int id;
  int pts = 0, gf = 0, ga = 0;
  double tiebreak_rng = 0.0; // random tiebreaker
};

// Returns group standings sorted by (pts desc, gd desc, gf desc, random)
// group_ids: team IDs in this group (exactly 4)
// prob_map:  key = "h_a" string → NumericMatrix
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

  // Round-robin: all pairs
  for (int i = 0; i < (int)group_ids.size(); ++i) {
    for (int j = i + 1; j < (int)group_ids.size(); ++j) {
      int h = group_ids[i], a = group_ids[j];
      std::string key = std::to_string(h) + "_" + std::to_string(a);
      auto it = prob_map.find(key);
      if (it == prob_map.end()) {
        // Reversed lookup
        key = std::to_string(a) + "_" + std::to_string(h);
        it  = prob_map.find(key);
        if (it == prob_map.end()) continue; // missing — skip

        // Swap: matrix was built for (a, h), transpose roles
        MatchResult res = sample_score(it->second, rng);
        // away goals become home, home become away
        int tmp = res.home_goals; res.home_goals = res.away_goals; res.away_goals = tmp;
        res.outcome = -res.outcome;
        stats[h].gf += res.home_goals; stats[h].ga += res.away_goals;
        stats[a].gf += res.away_goals; stats[a].ga += res.home_goals;
        if      (res.outcome ==  1) stats[h].pts += 3;
        else if (res.outcome == -1) stats[a].pts += 3;
        else { stats[h].pts += 1; stats[a].pts += 1; }
        continue;
      }
      MatchResult res = sample_score(it->second, rng);
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

// Returns {winner_id, loser_id}
std::pair<int,int> simulate_ko_match(
    int team_h, int team_a,
    const std::unordered_map<std::string, NumericMatrix>& prob_map,
    std::mt19937& rng)
{
  std::string key = std::to_string(team_h) + "_" + std::to_string(team_a);
  auto it = prob_map.find(key);

  MatchResult res;
  if (it != prob_map.end()) {
    res = sample_score(it->second, rng);
  } else {
    key = std::to_string(team_a) + "_" + std::to_string(team_h);
    it  = prob_map.find(key);
    if (it != prob_map.end()) {
      res = sample_score(it->second, rng);
      // swap perspective
      int tmp = res.home_goals; res.home_goals = res.away_goals; res.away_goals = tmp;
      res.outcome = -res.outcome;
    } else {
      // Fallback: 50/50 coin flip
      res = {0, 0, 0};
    }
  }

  int winner, loser;
  if (res.outcome == 1)       { winner = team_h; loser = team_a; }
  else if (res.outcome == -1) { winner = team_a; loser = team_h; }
  else { // draw → penalty shootout
    winner = resolve_draw_ko(team_h, team_a, rng);
    loser  = (winner == team_h) ? team_a : team_h;
  }
  return {winner, loser};
}

// ─────────────────────────────────────────────────────────────────────────────
// Third-place ranking (FIFA 2026 rules: best 8 of 12 groups)
// ─────────────────────────────────────────────────────────────────────────────

std::vector<int> select_best_thirds(
    const std::vector<TeamStat>& thirds,
    const std::vector<int>& group_order) // group index 0..11
{
  // thirds[i] corresponds to group_order[i]
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
  std::vector<int> best8_ids;
  for (int k = 0; k < 8 && k < (int)idx.size(); ++k)
    best8_ids.push_back(thirds[idx[k]].id);
  return best8_ids;
}

// ─────────────────────────────────────────────────────────────────────────────
// Single tournament simulation
// ─────────────────────────────────────────────────────────────────────────────

// groups_list: named list of integer vectors (team IDs per group, A..L)
// prob_list:   named list of NumericMatrix, names like "7_12"
// Returns a named list with elimination counts per team per stage

// [[Rcpp::export]]
List simulate_tournament_cpp(
    List groups_list,       // named list: group letter → int vector of team IDs
    List prob_list,         // named list: "h_a" → NumericMatrix
    int  seed = -1)
{
  std::mt19937 rng(seed < 0 ? std::random_device{}() : (unsigned)seed);

  // Build probability map
  std::unordered_map<std::string, NumericMatrix> prob_map;
  CharacterVector prob_names = prob_list.names();
  for (int i = 0; i < prob_list.size(); ++i)
    prob_map[as<std::string>(prob_names[i])] = as<NumericMatrix>(prob_list[i]);

  // Simulate all groups
  CharacterVector grp_names = groups_list.names();
  int n_groups = groups_list.size(); // should be 12

  // Per-group standings
  // groups A..L → index 0..11
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

  std::vector<int> group_winners(n_groups);
  for (int g = 0; g < n_groups; ++g)
    group_winners[g] = standings[g][0].id;

  // Collect elimination lists
  std::vector<int> group_out;   // 4th place + non-advancing 3rds
  std::vector<int> r32_losers, r16_losers, qf_losers, sf_losers;
  int finalist = -1, champion = -1;

  // 4th place teams are eliminated
  for (int g = 0; g < n_groups; ++g)
    group_out.push_back(standings[g][3].id);

  // Collect 3rd place teams
  std::vector<TeamStat> thirds(n_groups);
  for (int g = 0; g < n_groups; ++g)
    thirds[g] = standings[g][2];

  // Best 8 thirds advance
  std::vector<int> group_order(n_groups);
  std::iota(group_order.begin(), group_order.end(), 0);
  std::vector<int> best8 = select_best_thirds(thirds, group_order);

  // Non-advancing thirds → group_out
  for (int g = 0; g < n_groups; ++g) {
    int tid = standings[g][2].id;
    bool advances = std::find(best8.begin(), best8.end(), tid) != best8.end();
    if (!advances) group_out.push_back(tid);
  }

  // Helper lambda: get team ID by group + rank (0-indexed rank)
  auto gt = [&](const std::string& grp, int rank) -> int {
    int gi = grp_idx.count(grp) ? grp_idx[grp] : -1;
    if (gi < 0 || rank >= (int)standings[gi].size()) return -1;
    return standings[gi][rank].id;
  };

  std::vector<std::pair<int,int>> r32_pairs = {
    {gt("A",1), gt("B",1)},   // 2A vs 2B
    {gt("C",0), gt("F",1)},   // 1C vs 2F
    {gt("E",0), best8[0]},    // 1E vs 3rd
    {gt("F",0), gt("C",1)},   // 1F vs 2C
    {gt("E",1), gt("I",1)},   // 2E vs 2I
    {gt("I",0), best8[1]},    // 1I vs 3rd
    {gt("A",0), best8[2]},    // 1A vs 3rd
    {gt("L",0), best8[3]},    // 1L vs 3rd
    {gt("G",0), best8[4]},    // 1G vs 3rd
    {gt("D",0), best8[5]},    // 1D vs 3rd
    {gt("H",0), gt("J",1)},   // 1H vs 2J
    {gt("K",1), gt("L",1)},   // 2K vs 2L
    {gt("B",0), best8[6]},    // 1B vs 3rd
    {gt("D",1), gt("G",1)},   // 2D vs 2G
    {gt("J",0), gt("H",1)},   // 1J vs 2H
    {gt("K",0), best8[7]}     // 1K vs 3rd
  };

  std::vector<int> r32_winners;
  for (auto& p : r32_pairs) {
    if (p.first < 0 || p.second < 0) { r32_winners.push_back(-1); continue; }
    auto [w, l] = simulate_ko_match(p.first, p.second, prob_map, rng);
    r32_winners.push_back(w);
    r32_losers.push_back(l);
  }

  // Round of 16 — 8 matches: winners[0]vs[1], [2]vs[3], ...
  std::vector<int> r16_winners;
  for (int i = 0; i < 16; i += 2) {
    if (r32_winners[i] < 0 || r32_winners[i+1] < 0) {
      r16_winners.push_back(-1); continue;
    }
    auto [w, l] = simulate_ko_match(r32_winners[i], r32_winners[i+1], prob_map, rng);
    r16_winners.push_back(w);
    r16_losers.push_back(l);
  }

  // Quarter-finals — 4 matches
  std::vector<int> qf_winners;
  for (int i = 0; i < 8; i += 2) {
    if (r16_winners[i] < 0 || r16_winners[i+1] < 0) {
      qf_winners.push_back(-1); continue;
    }
    auto [w, l] = simulate_ko_match(r16_winners[i], r16_winners[i+1], prob_map, rng);
    qf_winners.push_back(w);
    qf_losers.push_back(l);
  }

  // Semi-finals — 2 matches
  std::vector<int> sf_winners;
  std::vector<int> sf_losers_vec;
  for (int i = 0; i < 4; i += 2) {
    if (qf_winners[i] < 0 || qf_winners[i+1] < 0) {
      sf_winners.push_back(-1); sf_losers_vec.push_back(-1); continue;
    }
    auto [w, l] = simulate_ko_match(qf_winners[i], qf_winners[i+1], prob_map, rng);
    sf_winners.push_back(w);
    sf_losers_vec.push_back(l);
  }

  // Third-place play-off (ignored for reach probabilities, but recorded)
  // Final
  if (sf_winners.size() >= 2 && sf_winners[0] > 0 && sf_winners[1] > 0) {
    auto [w, l] = simulate_ko_match(sf_winners[0], sf_winners[1], prob_map, rng);
    champion = w;
    finalist = l;
  }

  return List::create(
    Named("group_out")  = wrap(group_out),
    Named("r32_losers") = wrap(r32_losers),
    Named("r16_losers") = wrap(r16_losers),
    Named("qf_losers")  = wrap(qf_losers),
    Named("sf_losers")  = wrap(sf_losers_vec),
    Named("finalist")   = finalist,
    Named("champion")   = champion,
    Named("group_winners") = wrap(group_winners),
    Named("group_labels")  = wrap(grp_labels)
  );
}


// ─────────────────────────────────────────────────────────────────────────────
// Monte Carlo wrapper — called once from R, runs n_sims internally
// ─────────────────────────────────────────────────────────────────────────────

// [[Rcpp::export]]
List run_mc_cpp(
    List   groups_list,
    List   prob_list,
    int    n_sims,
    int    seed_base = 42)
{
  std::mt19937 seed_rng(seed_base);
  std::uniform_int_distribution<int> seed_dist(0, 1e8);

  std::vector<int> all_ids;
  CharacterVector grp_names = groups_list.names();
  int n_groups = groups_list.size();

  // Collect team IDs and build group membership map
  std::unordered_map<int, std::string> team_group;
  for (int g = 0; g < n_groups; ++g) {
    IntegerVector ids = as<IntegerVector>(groups_list[g]);
    std::string glabel = as<std::string>(grp_names[g]);
    for (int id : ids) {
      all_ids.push_back(id);
      team_group[id] = glabel;
    }
  }

  std::unordered_map<int, std::array<int,7>> elim;
  for (int id : all_ids) elim[id] = {0,0,0,0,0,0,0};

  // ── NEW: group-winner counter ─────────────────────────────────
  std::unordered_map<int, int> gwin;
  for (int id : all_ids) gwin[id] = 0;
  // ─────────────────────────────────────────────────────────────

  for (int sim = 0; sim < n_sims; ++sim) {
    int s = seed_dist(seed_rng);
    List r = simulate_tournament_cpp(groups_list, prob_list, s);

    IntegerVector go  = r["group_out"];
    IntegerVector r32 = r["r32_losers"];
    IntegerVector r16 = r["r16_losers"];
    IntegerVector qf  = r["qf_losers"];
    IntegerVector sf  = r["sf_losers"];
    int fin  = as<int>(r["finalist"]);
    int chmp = as<int>(r["champion"]);

    for (int id : go)  if (elim.count(id)) elim[id][0]++;
    for (int id : r32) if (elim.count(id)) elim[id][1]++;
    for (int id : r16) if (elim.count(id)) elim[id][2]++;
    for (int id : qf)  if (elim.count(id)) elim[id][3]++;
    for (int id : sf)  if (elim.count(id)) elim[id][4]++;
    if (fin  > 0 && elim.count(fin))  elim[fin][5]++;
    if (chmp > 0 && elim.count(chmp)) elim[chmp][6]++;

    // ── NEW: record group winners ─────────────────────────────
    IntegerVector gw = r["group_winners"];
    for (int id : gw)
      if (gwin.count(id)) gwin[id]++;
      // ─────────────────────────────────────────────────────────
  }

  int n = all_ids.size();
  IntegerVector  ids_out(n);
  NumericVector  p_group(n), p_r32(n), p_r16(n), p_qf(n), p_sf(n), p_final(n), p_champ(n);
  NumericVector  p_gwin(n);

  for (int i = 0; i < n; ++i) {
    int id = all_ids[i];
    ids_out[i] = id;
    double ns   = (double)n_sims;
    double pg   = elim[id][0] / ns;
    double pr32 = elim[id][1] / ns;
    double pr16 = elim[id][2] / ns;
    double pqf  = elim[id][3] / ns;
    double psf  = elim[id][4] / ns;
    double pch  = elim[id][6] / ns;

    p_group[i] = 1.0;
    p_r32[i]   = 1.0 - pg;
    p_r16[i]   = p_r32[i]   - pr32;
    p_qf[i]    = p_r16[i]   - pr16;
    p_sf[i]    = p_qf[i]    - pqf;
    p_final[i] = p_sf[i]    - psf;
    p_champ[i] = pch;
    p_gwin[i]  = gwin[id] / ns;
  }

  return List::create(
    Named("id")           = ids_out,
    Named("Group.Stage")  = p_group,
    Named("Round.of.32")  = p_r32,
    Named("Round.of.16")  = p_r16,
    Named("Quarter.Final")= p_qf,
    Named("Semi.Final")   = p_sf,
    Named("Final")        = p_final,
    Named("Champion")     = p_champ,
    Named("GroupWinner")  = p_gwin,
    Named("n_sims")       = n_sims
  );
}
