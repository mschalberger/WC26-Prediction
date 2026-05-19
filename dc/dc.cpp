#include <Rcpp.h>
#include <vector>
#include <string>
#include <unordered_map>
#include <unordered_set>
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

int resolve_draw_ko(double p_home_wins, double alpha, std::mt19937& rng,
                    int team_h, int team_a) {
  // Shrink toward 0.5
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
      if (it == prob_map.end()) {
        key = std::to_string(a) + "_" + std::to_string(h);
        it  = prob_map.find(key);
        if (it == prob_map.end()) continue;
        MatchResult res = sample_score(it->second, rng);
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

std::pair<int,int> simulate_ko_match(
    int team_h, int team_a,
    const std::unordered_map<std::string, NumericMatrix>& prob_map,
    std::mt19937& rng,
    double pen_alpha = 0.5)
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
      int tmp = res.home_goals; res.home_goals = res.away_goals; res.away_goals = tmp;
      res.outcome = -res.outcome;
    } else {
      res = {0, 0, 0};
    }
  }

  int winner, loser;
  if (res.outcome == 1)       { winner = team_h; loser = team_a; }
  else if (res.outcome == -1) { winner = team_a; loser = team_h; }
  else {
    double p_home_wins = 0.5;  // fallback
    if (it != prob_map.end()) {
      const NumericMatrix& m = it->second;
      int n = m.nrow();
      double p_hw = 0.0;
      for (int i = 1; i < n; ++i)
        for (int j = 0; j < i; ++j)
          p_hw += m(i, j);
      p_home_wins = swapped ? (1.0 - p_hw) : p_hw;
    }
    int ko_winner = resolve_draw_ko(p_home_wins, pen_alpha, rng, team_h, team_a);
    winner = ko_winner;
    loser  = (winner == team_h) ? team_a : team_h;
  }
  return {winner, loser};
}

// ─────────────────────────────────────────────────────────────────────────────
// Third-place selection (best 8 of 12) — also returns group letters in rank order
// ─────────────────────────────────────────────────────────────────────────────

std::vector<TeamStat> select_best_thirds(
    const std::vector<TeamStat>& thirds,
    const std::vector<std::string>& grp_labels,
    std::vector<std::string>& out_sorted_groups)
{
  std::vector<int> idx(thirds.size());
  std::iota(idx.begin(), idx.end(), 0);
  std::sort(idx.begin(), idx.end(), [&](int a, int b) {
    const TeamStat& ta = thirds[a]; const TeamStat& tb = thirds[b];
    if (ta.pts != tb.pts) return ta.pts > tb.pts;
    int gda = ta.gf - ta.ga, gdb = tb.gf - tb.ga;
    if (gda != gdb) return gda > gdb;
    if (ta.gf != tb.gf) return ta.gf > tb.gf;
    return ta.tiebreak_rng > tb.tiebreak_rng;
  });

  std::vector<TeamStat> best8;
  out_sorted_groups.clear();
  for (int k = 0; k < 8 && k < (int)idx.size(); ++k) {
    best8.push_back(thirds[idx[k]]);
    out_sorted_groups.push_back(grp_labels[idx[k]]);
  }
  return best8;
}

// ─────────────────────────────────────────────────────────────────────────────
// FIFA 2026 Annex C — all 495 combinations
// Key   = 8 sorted group letters (e.g. "ABCDEFGH")
// Value = 8-char assignment string for winners 1A,1B,1D,1E,1G,1I,1K,1L
// ─────────────────────────────────────────────────────────────────────────────

static std::unordered_map<std::string, std::string> build_annexC() {
  return {
  {"ABCDEFGH","HGBCAFDE"}, {"ABCDEFGI","CGBDAFEI"}, {"ABCDEFGJ","CGBDAFEJ"}, {"ABCDEFGK","CGBDAFEK"}, {"ABCDEFGL","CGBDAFLE"},
  {"ABCDEFHI","HEBCAFDI"}, {"ABCDEFHJ","HJBCAFDE"}, {"ABCDEFHK","HEBCAFDK"}, {"ABCDEFHL","HFBCADLE"}, {"ABCDEFIJ","CJBDAFEI"},
  {"ABCDEFIK","CEBDAFIK"}, {"ABCDEFIL","CEBDAFLI"}, {"ABCDEFJK","CJBDAFEK"}, {"ABCDEFJL","CJBDAFLE"}, {"ABCDEFKL","CEBDAFLK"},
  {"ABCDEGHI","HGBCADEI"}, {"ABCDEGHJ","HGBCADEJ"}, {"ABCDEGHK","HGBCADEK"}, {"ABCDEGHL","HGBCADLE"}, {"ABCDEGIJ","EGBCADIJ"},
  {"ABCDEGIK","EGBCADIK"}, {"ABCDEGIL","EGBCADLI"}, {"ABCDEGJK","EGBCADJK"}, {"ABCDEGJL","EGBCADLJ"}, {"ABCDEGKL","EGBCADLK"},
  {"ABCDEHIJ","HJBCADEI"}, {"ABCDEHIK","HEBCADIK"}, {"ABCDEHIL","HEBCADLI"}, {"ABCDEHJK","HJBCADEK"}, {"ABCDEHJL","HJBCADLE"},
  {"ABCDEHKL","HEBCADLK"}, {"ABCDEIJK","EJBCADIK"}, {"ABCDEIJL","EJBCADLI"}, {"ABCDEIKL","EIBCADLK"}, {"ABCDEJKL","EJBCADLK"},
  {"ABCDFGHI","HGBCAFDI"}, {"ABCDFGHJ","HGBCAFDJ"}, {"ABCDFGHK","HGBCAFDK"}, {"ABCDFGHL","CGBDAFLH"}, {"ABCDFGIJ","CGBDAFIJ"},
  {"ABCDFGIK","CGBDAFIK"}, {"ABCDFGIL","CGBDAFLI"}, {"ABCDFGJK","CGBDAFJK"}, {"ABCDFGJL","CGBDAFLJ"}, {"ABCDFGKL","CGBDAFLK"},
  {"ABCDFHIJ","HJBCAFDI"}, {"ABCDFHIK","HFBCADIK"}, {"ABCDFHIL","HFBCADLI"}, {"ABCDFHJK","HJBCAFDK"}, {"ABCDFHJL","CJBDAFLH"},
  {"ABCDFHKL","HFBCADLK"}, {"ABCDFIJK","CJBDAFIK"}, {"ABCDFIJL","CJBDAFLI"}, {"ABCDFIKL","CIBDAFLK"}, {"ABCDFJKL","CJBDAFLK"},
  {"ABCDGHIJ","HGBCADIJ"}, {"ABCDGHIK","HGBCADIK"}, {"ABCDGHIL","HGBCADLI"}, {"ABCDGHJK","HGBCADJK"}, {"ABCDGHJL","HGBCADLJ"},
  {"ABCDGHKL","HGBCADLK"}, {"ABCDGIJK","CJBDAGIK"}, {"ABCDGIJL","CJBDAGLI"}, {"ABCDGIKL","IGBCADLK"}, {"ABCDGJKL","CJBDAGLK"},
  {"ABCDHIJK","HJBCADIK"}, {"ABCDHIJL","HJBCADLI"}, {"ABCDHIKL","HIBCADLK"}, {"ABCDHJKL","HJBCADLK"}, {"ABCDIJKL","IJBCADLK"},
  {"ABCEFGHI","HGBCAFEI"}, {"ABCEFGHJ","HGBCAFEJ"}, {"ABCEFGHK","HGBCAFEK"}, {"ABCEFGHL","HGBCAFLE"}, {"ABCEFGIJ","EGBCAFIJ"},
  {"ABCEFGIK","EGBCAFIK"}, {"ABCEFGIL","EGBCAFLI"}, {"ABCEFGJK","EGBCAFJK"}, {"ABCEFGJL","EGBCAFLJ"}, {"ABCEFGKL","EGBCAFLK"},
  {"ABCEFHIJ","HJBCAFEI"}, {"ABCEFHIK","HEBCAFIK"}, {"ABCEFHIL","HEBCAFLI"}, {"ABCEFHJK","HJBCAFEK"}, {"ABCEFHJL","HJBCAFLE"},
  {"ABCEFHKL","HEBCAFLK"}, {"ABCEFIJK","EJBCAFIK"}, {"ABCEFIJL","EJBCAFLI"}, {"ABCEFIKL","EIBCAFLK"}, {"ABCEFJKL","EJBCAFLK"},
  {"ABCEGHIJ","HJBCAGEI"}, {"ABCEGHIK","EGBCAHIK"}, {"ABCEGHIL","EGBCAHLI"}, {"ABCEGHJK","HJBCAGEK"}, {"ABCEGHJL","HJBCAGLE"},
  {"ABCEGHKL","EGBCAHLK"}, {"ABCEGIJK","EJBCAGIK"}, {"ABCEGIJL","EJBCAGLI"}, {"ABCEGIKL","EGBAICLK"}, {"ABCEGJKL","EJBCAGLK"},
  {"ABCEHIJK","EJBCAHIK"}, {"ABCEHIJL","EJBCAHLI"}, {"ABCEHIKL","EIBCAHLK"}, {"ABCEHJKL","EJBCAHLK"}, {"ABCEIJKL","EJBAICLK"},
  {"ABCFGHIJ","HGBCAFIJ"}, {"ABCFGHIK","HGBCAFIK"}, {"ABCFGHIL","HGBCAFLI"}, {"ABCFGHJK","HGBCAFJK"}, {"ABCFGHJL","HGBCAFLJ"},
  {"ABCFGHKL","HGBCAFLK"}, {"ABCFGIJK","CJBFAGIK"}, {"ABCFGIJL","CJBFAGLI"}, {"ABCFGIKL","IGBCAFLK"}, {"ABCFGJKL","CJBFAGLK"},
  {"ABCFHIJK","HJBCAFIK"}, {"ABCFHIJL","HJBCAFLI"}, {"ABCFHIKL","HIBCAFLK"}, {"ABCFHJKL","HJBCAFLK"}, {"ABCFIJKL","IJBCAFLK"},
  {"ABCGHIJK","HJBCAGIK"}, {"ABCGHIJL","HJBCAGLI"}, {"ABCGHIKL","IGBCAHLK"}, {"ABCGHJKL","HJBCAGLK"}, {"ABCGIJKL","IJBCAGLK"},
  {"ABCHIJKL","IJBCAHLK"}, {"ABDEFGHI","HGBDAFEI"}, {"ABDEFGHJ","HGBDAFEJ"}, {"ABDEFGHK","HGBDAFEK"}, {"ABDEFGHL","HGBDAFLE"},
  {"ABDEFGIJ","EGBDAFIJ"}, {"ABDEFGIK","EGBDAFIK"}, {"ABDEFGIL","EGBDAFLI"}, {"ABDEFGJK","EGBDAFJK"}, {"ABDEFGJL","EGBDAFLJ"},
  {"ABDEFGKL","EGBDAFLK"}, {"ABDEFHIJ","HJBDAFEI"}, {"ABDEFHIK","HEBDAFIK"}, {"ABDEFHIL","HEBDAFLI"}, {"ABDEFHJK","HJBDAFEK"},
  {"ABDEFHJL","HJBDAFLE"}, {"ABDEFHKL","HEBDAFLK"}, {"ABDEFIJK","EJBDAFIK"}, {"ABDEFIJL","EJBDAFLI"}, {"ABDEFIKL","EIBDAFLK"},
  {"ABDEFJKL","EJBDAFLK"}, {"ABDEGHIJ","HJBDAGEI"}, {"ABDEGHIK","EGBDAHIK"}, {"ABDEGHIL","EGBDAHLI"}, {"ABDEGHJK","HJBDAGEK"},
  {"ABDEGHJL","HJBDAGLE"}, {"ABDEGHKL","EGBDAHLK"}, {"ABDEGIJK","EJBDAGIK"}, {"ABDEGIJL","EJBDAGLI"}, {"ABDEGIKL","EGBAIDLK"},
  {"ABDEGJKL","EJBDAGLK"}, {"ABDEHIJK","EJBDAHIK"}, {"ABDEHIJL","EJBDAHLI"}, {"ABDEHIKL","EIBDAHLK"}, {"ABDEHJKL","EJBDAHLK"},
  {"ABDEIJKL","EJBAIDLK"}, {"ABDFGHIJ","HGBDAFIJ"}, {"ABDFGHIK","HGBDAFIK"}, {"ABDFGHIL","HGBDAFLI"}, {"ABDFGHJK","HGBDAFJK"},
  {"ABDFGHJL","HGBDAFLJ"}, {"ABDFGHKL","HGBDAFLK"}, {"ABDFGIJK","FJBDAGIK"}, {"ABDFGIJL","FJBDAGLI"}, {"ABDFGIKL","IGBDAFLK"},
  {"ABDFGJKL","FJBDAGLK"}, {"ABDFHIJK","HJBDAFIK"}, {"ABDFHIJL","HJBDAFLI"}, {"ABDFHIKL","HIBDAFLK"}, {"ABDFHJKL","HJBDAFLK"},
  {"ABDFIJKL","IJBDAFLK"}, {"ABDGHIJK","HJBDAGIK"}, {"ABDGHIJL","HJBDAGLI"}, {"ABDGHIKL","IGBDAHLK"}, {"ABDGHJKL","HJBDAGLK"},
  {"ABDGIJKL","IJBDAGLK"}, {"ABDHIJKL","IJBDAHLK"}, {"ABEFGHIJ","HJBFAGEI"}, {"ABEFGHIK","EGBFAHIK"}, {"ABEFGHIL","EGBFAHLI"},
  {"ABEFGHJK","HJBFAGEK"}, {"ABEFGHJL","HJBFAGLE"}, {"ABEFGHKL","EGBFAHLK"}, {"ABEFGIJK","EJBFAGIK"}, {"ABEFGIJL","EJBFAGLI"},
  {"ABEFGIKL","EGBAIFLK"}, {"ABEFGJKL","EJBFAGLK"}, {"ABEFHIJK","EJBFAHIK"}, {"ABEFHIJL","EJBFAHLI"}, {"ABEFHIKL","EIBFAHLK"},
  {"ABEFHJKL","EJBFAHLK"}, {"ABEFIJKL","EJBAIFLK"}, {"ABEGHIJK","EJBAHGIK"}, {"ABEGHIJL","EJBAHGLI"}, {"ABEGHIKL","EGBAIHLK"},
  {"ABEGHJKL","EJBAHGLK"}, {"ABEGIJKL","EJBAIGLK"}, {"ABEHIJKL","EJBAIHLK"}, {"ABFGHIJK","HJBFAGIK"}, {"ABFGHIJL","HJBFAGLI"},
  {"ABFGHIKL","HGBAIFLK"}, {"ABFGHJKL","HJBFAGLK"}, {"ABFGIJKL","IJBFAGLK"}, {"ABFHIJKL","HJBAIFLK"}, {"ABGHIJKL","HJBAIGLK"},
  {"ACDEFGHI","HGECAFDI"}, {"ACDEFGHJ","HGJCAFDE"}, {"ACDEFGHK","HGECAFDK"}, {"ACDEFGHL","HGFCADLE"}, {"ACDEFGIJ","CGJDAFEI"},
  {"ACDEFGIK","CGEDAFIK"}, {"ACDEFGIL","CGEDAFLI"}, {"ACDEFGJK","CGJDAFEK"}, {"ACDEFGJL","CGJDAFLE"}, {"ACDEFGKL","CGEDAFLK"},
  {"ACDEFHIJ","HJECAFDI"}, {"ACDEFHIK","HEFCADIK"}, {"ACDEFHIL","HEFCADLI"}, {"ACDEFHJK","HJECAFDK"}, {"ACDEFHJL","HJFCADLE"},
  {"ACDEFHKL","HEFCADLK"}, {"ACDEFIJK","CJEDAFIK"}, {"ACDEFIJL","CJEDAFLI"}, {"ACDEFIKL","CEIDAFLK"}, {"ACDEFJKL","CJEDAFLK"},
  {"ACDEGHIJ","HGJCADEI"}, {"ACDEGHIK","HGECADIK"}, {"ACDEGHIL","HGECADLI"}, {"ACDEGHJK","HGJCADEK"}, {"ACDEGHJL","HGJCADLE"},
  {"ACDEGHKL","HGECADLK"}, {"ACDEGIJK","EGJCADIK"}, {"ACDEGIJL","EGJCADLI"}, {"ACDEGIKL","EGICADLK"}, {"ACDEGJKL","EGJCADLK"},
  {"ACDEHIJK","HJECADIK"}, {"ACDEHIJL","HJECADLI"}, {"ACDEHIKL","HEICADLK"}, {"ACDEHJKL","HJECADLK"}, {"ACDEIJKL","EJICADLK"},
  {"ACDFGHIJ","HGJCAFDI"}, {"ACDFGHIK","HGFCADIK"}, {"ACDFGHIL","HGFCADLI"}, {"ACDFGHJK","HGJCAFDK"}, {"ACDFGHJL","CGJDAFLH"},
  {"ACDFGHKL","HGFCADLK"}, {"ACDFGIJK","CGJDAFIK"}, {"ACDFGIJL","CGJDAFLI"}, {"ACDFGIKL","CGIDAFLK"}, {"ACDFGJKL","CGJDAFLK"},
  {"ACDFHIJK","HJFCADIK"}, {"ACDFHIJL","HJFCADLI"}, {"ACDFHIKL","HFICADLK"}, {"ACDFHJKL","HJFCADLK"}, {"ACDFIJKL","CJIDAFLK"},
  {"ACDGHIJK","HGJCADIK"}, {"ACDGHIJL","HGJCADLI"}, {"ACDGHIKL","HGICADLK"}, {"ACDGHJKL","HGJCADLK"}, {"ACDGIJKL","IGJCADLK"},
  {"ACDHIJKL","HJICADLK"}, {"ACEFGHIJ","HGJCAFEI"}, {"ACEFGHIK","HGECAFIK"}, {"ACEFGHIL","HGECAFLI"}, {"ACEFGHJK","HGJCAFEK"},
  {"ACEFGHJL","HGJCAFLE"}, {"ACEFGHKL","HGECAFLK"}, {"ACEFGIJK","EGJCAFIK"}, {"ACEFGIJL","EGJCAFLI"}, {"ACEFGIKL","EGICAFLK"},
  {"ACEFGJKL","EGJCAFLK"}, {"ACEFHIJK","HJECAFIK"}, {"ACEFHIJL","HJECAFLI"}, {"ACEFHIKL","HEICAFLK"}, {"ACEFHJKL","HJECAFLK"},
  {"ACEFIJKL","EJICAFLK"}, {"ACEGHIJK","EGJCAHIK"}, {"ACEGHIJL","EGJCAHLI"}, {"ACEGHIKL","EGICAHLK"}, {"ACEGHJKL","EGJCAHLK"},
  {"ACEGIJKL","EJICAGLK"}, {"ACEHIJKL","EJICAHLK"}, {"ACFGHIJK","HGJCAFIK"}, {"ACFGHIJL","HGJCAFLI"}, {"ACFGHIKL","HGICAFLK"},
  {"ACFGHJKL","HGJCAFLK"}, {"ACFGIJKL","IGJCAFLK"}, {"ACFHIJKL","HJICAFLK"}, {"ACGHIJKL","HJICAGLK"}, {"ADEFGHIJ","HGJDAFEI"},
  {"ADEFGHIK","HGEDAFIK"}, {"ADEFGHIL","HGEDAFLI"}, {"ADEFGHJK","HGJDAFEK"}, {"ADEFGHJL","HGJDAFLE"}, {"ADEFGHKL","HGEDAFLK"},
  {"ADEFGIJK","EGJDAFIK"}, {"ADEFGIJL","EGJDAFLI"}, {"ADEFGIKL","EGIDAFLK"}, {"ADEFGJKL","EGJDAFLK"}, {"ADEFHIJK","HJEDAFIK"},
  {"ADEFHIJL","HJEDAFLI"}, {"ADEFHIKL","HEIDAFLK"}, {"ADEFHJKL","HJEDAFLK"}, {"ADEFIJKL","EJIDAFLK"}, {"ADEGHIJK","EGJDAHIK"},
  {"ADEGHIJL","EGJDAHLI"}, {"ADEGHIKL","EGIDAHLK"}, {"ADEGHJKL","EGJDAHLK"}, {"ADEGIJKL","EJIDAGLK"}, {"ADEHIJKL","EJIDAHLK"},
  {"ADFGHIJK","HGJDAFIK"}, {"ADFGHIJL","HGJDAFLI"}, {"ADFGHIKL","HGIDAFLK"}, {"ADFGHJKL","HGJDAFLK"}, {"ADFGIJKL","IGJDAFLK"},
  {"ADFHIJKL","HJIDAFLK"}, {"ADGHIJKL","HJIDAGLK"}, {"AEFGHIJK","EGJFAHIK"}, {"AEFGHIJL","EGJFAHLI"}, {"AEFGHIKL","EGIFAHLK"},
  {"AEFGHJKL","EGJFAHLK"}, {"AEFGIJKL","EJIFAGLK"}, {"AEFHIJKL","EJIFAHLK"}, {"AEGHIJKL","EJIAHGLK"}, {"AFGHIJKL","HJIFAGLK"},
  {"BCDEFGHI","CGBDHFEI"}, {"BCDEFGHJ","HGBCJFDE"}, {"BCDEFGHK","CGBDHFEK"}, {"BCDEFGHL","CGBDHFLE"}, {"BCDEFGIJ","CGBDJFEI"},
  {"BCDEFGIK","CGBDEFIK"}, {"BCDEFGIL","CGBDEFLI"}, {"BCDEFGJK","CGBDJFEK"}, {"BCDEFGJL","CGBDJFLE"}, {"BCDEFGKL","CGBDEFLK"},
  {"BCDEFHIJ","CJBDHFEI"}, {"BCDEFHIK","CEBDHFIK"}, {"BCDEFHIL","CEBDHFLI"}, {"BCDEFHJK","CJBDHFEK"}, {"BCDEFHJL","CJBDHFLE"},
  {"BCDEFHKL","CEBDHFLK"}, {"BCDEFIJK","CJBDEFIK"}, {"BCDEFIJL","CJBDEFLI"}, {"BCDEFIKL","CEBDIFLK"}, {"BCDEFJKL","CJBDEFLK"},
  {"BCDEGHIJ","HGBCJDEI"}, {"BCDEGHIK","EGBCHDIK"}, {"BCDEGHIL","EGBCHDLI"}, {"BCDEGHJK","HGBCJDEK"}, {"BCDEGHJL","HGBCJDLE"},
  {"BCDEGHKL","EGBCHDLK"}, {"BCDEGIJK","EGBCJDIK"}, {"BCDEGIJL","EGBCJDLI"}, {"BCDEGIKL","EGBCIDLK"}, {"BCDEGJKL","EGBCJDLK"},
  {"BCDEHIJK","EJBCHDIK"}, {"BCDEHIJL","EJBCHDLI"}, {"BCDEHIKL","EIBCHDLK"}, {"BCDEHJKL","EJBCHDLK"}, {"BCDEIJKL","EJBCIDLK"},
  {"BCDFGHIJ","HGBCJFDI"}, {"BCDFGHIK","CGBDHFIK"}, {"BCDFGHIL","CGBDHFLI"}, {"BCDFGHJK","HGBCJFDK"}, {"BCDFGHJL","CGBDHFLJ"},
  {"BCDFGHKL","CGBDHFLK"}, {"BCDFGIJK","CGBDJFIK"}, {"BCDFGIJL","CGBDJFLI"}, {"BCDFGIKL","CGBDIFLK"}, {"BCDFGJKL","CGBDJFLK"},
  {"BCDFHIJK","CJBDHFIK"}, {"BCDFHIJL","CJBDHFLI"}, {"BCDFHIKL","CIBDHFLK"}, {"BCDFHJKL","CJBDHFLK"}, {"BCDFIJKL","CJBDIFLK"},
  {"BCDGHIJK","HGBCJDIK"}, {"BCDGHIJL","HGBCJDLI"}, {"BCDGHIKL","HGBCIDLK"}, {"BCDGHJKL","HGBCJDLK"}, {"BCDGIJKL","IGBCJDLK"},
  {"BCDHIJKL","HJBCIDLK"}, {"BCEFGHIJ","HGBCJFEI"}, {"BCEFGHIK","EGBCHFIK"}, {"BCEFGHIL","EGBCHFLI"}, {"BCEFGHJK","HGBCJFEK"},
  {"BCEFGHJL","HGBCJFLE"}, {"BCEFGHKL","EGBCHFLK"}, {"BCEFGIJK","EGBCJFIK"}, {"BCEFGIJL","EGBCJFLI"}, {"BCEFGIKL","EGBCIFLK"},
  {"BCEFGJKL","EGBCJFLK"}, {"BCEFHIJK","EJBCHFIK"}, {"BCEFHIJL","EJBCHFLI"}, {"BCEFHIKL","EIBCHFLK"}, {"BCEFHJKL","EJBCHFLK"},
  {"BCEFIJKL","EJBCIFLK"}, {"BCEGHIJK","EJBCHGIK"}, {"BCEGHIJL","EJBCHGLI"}, {"BCEGHIKL","EGBCIHLK"}, {"BCEGHJKL","EJBCHGLK"},
  {"BCEGIJKL","EJBCIGLK"}, {"BCEHIJKL","EJBCIHLK"}, {"BCFGHIJK","HGBCJFIK"}, {"BCFGHIJL","HGBCJFLI"}, {"BCFGHIKL","HGBCIFLK"},
  {"BCFGHJKL","HGBCJFLK"}, {"BCFGIJKL","IGBCJFLK"}, {"BCFHIJKL","HJBCIFLK"}, {"BCGHIJKL","HJBCIGLK"}, {"BDEFGHIJ","HGBDJFEI"},
  {"BDEFGHIK","EGBDHFIK"}, {"BDEFGHIL","EGBDHFLI"}, {"BDEFGHJK","HGBDJFEK"}, {"BDEFGHJL","HGBDJFLE"}, {"BDEFGHKL","EGBDHFLK"},
  {"BDEFGIJK","EGBDJFIK"}, {"BDEFGIJL","EGBDJFLI"}, {"BDEFGIKL","EGBDIFLK"}, {"BDEFGJKL","EGBDJFLK"}, {"BDEFHIJK","EJBDHFIK"},
  {"BDEFHIJL","EJBDHFLI"}, {"BDEFHIKL","EIBDHFLK"}, {"BDEFHJKL","EJBDHFLK"}, {"BDEFIJKL","EJBDIFLK"}, {"BDEGHIJK","EJBDHGIK"},
  {"BDEGHIJL","EJBDHGLI"}, {"BDEGHIKL","EGBDIHLK"}, {"BDEGHJKL","EJBDHGLK"}, {"BDEGIJKL","EJBDIGLK"}, {"BDEHIJKL","EJBDIHLK"},
  {"BDFGHIJK","HGBDJFIK"}, {"BDFGHIJL","HGBDJFLI"}, {"BDFGHIKL","HGBDIFLK"}, {"BDFGHJKL","HGBDJFLK"}, {"BDFGIJKL","IGBDJFLK"},
  {"BDFHIJKL","HJBDIFLK"}, {"BDGHIJKL","HJBDIGLK"}, {"BEFGHIJK","EJBFHGIK"}, {"BEFGHIJL","EJBFHGLI"}, {"BEFGHIKL","EGBFIHLK"},
  {"BEFGHJKL","EJBFHGLK"}, {"BEFGIJKL","EJBFIGLK"}, {"BEFHIJKL","EJBFIHLK"}, {"BEGHIJKL","EJIBHGLK"}, {"BFGHIJKL","HJBFIGLK"},
  {"CDEFGHIJ","CGJDHFEI"}, {"CDEFGHIK","CGEDHFIK"}, {"CDEFGHIL","CGEDHFLI"}, {"CDEFGHJK","CGJDHFEK"}, {"CDEFGHJL","CGJDHFLE"},
  {"CDEFGHKL","CGEDHFLK"}, {"CDEFGIJK","CGEDJFIK"}, {"CDEFGIJL","CGEDJFLI"}, {"CDEFGIKL","CGEDIFLK"}, {"CDEFGJKL","CGEDJFLK"},
  {"CDEFHIJK","CJEDHFIK"}, {"CDEFHIJL","CJEDHFLI"}, {"CDEFHIKL","CEIDHFLK"}, {"CDEFHJKL","CJEDHFLK"}, {"CDEFIJKL","CJEDIFLK"},
  {"CDEGHIJK","EGJCHDIK"}, {"CDEGHIJL","EGJCHDLI"}, {"CDEGHIKL","EGICHDLK"}, {"CDEGHJKL","EGJCHDLK"}, {"CDEGIJKL","EGICJDLK"},
  {"CDEHIJKL","EJICHDLK"}, {"CDFGHIJK","CGJDHFIK"}, {"CDFGHIJL","CGJDHFLI"}, {"CDFGHIKL","CGIDHFLK"}, {"CDFGHJKL","CGJDHFLK"},
  {"CDFGIJKL","CGIDJFLK"}, {"CDFHIJKL","CJIDHFLK"}, {"CDGHIJKL","HGICJDLK"}, {"CEFGHIJK","EGJCHFIK"}, {"CEFGHIJL","EGJCHFLI"},
  {"CEFGHIKL","EGICHFLK"}, {"CEFGHJKL","EGJCHFLK"}, {"CEFGIJKL","EGICJFLK"}, {"CEFHIJKL","EJICHFLK"}, {"CEGHIJKL","EJICHGLK"},
  {"CFGHIJKL","HGICJFLK"}, {"DEFGHIJK","EGJDHFIK"}, {"DEFGHIJL","EGJDHFLI"}, {"DEFGHIKL","EGIDHFLK"}, {"DEFGHJKL","EGJDHFLK"},
  {"DEFGIJKL","EGIDJFLK"}, {"DEFHIJKL","EJIDHFLK"}, {"DEGHIJKL","EJIDHGLK"}, {"DFGHIJKL","HGIDJFLK"}, {"EFGHIJKL","EJIFHGLK"}
};
}

// ─────────────────────────────────────────────────────────────────────────────
// Single tournament simulation
// ─────────────────────────────────────────────────────────────────────────────

// [[Rcpp::export]]
List simulate_tournament_cpp(
    List groups_list,
    List prob_list,
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

  std::vector<int> group_winners(n_groups);
  for (int g = 0; g < n_groups; ++g)
    group_winners[g] = standings[g][0].id;

  // Elimination lists
  std::vector<int> group_out;
  std::vector<int> r32_losers, r16_losers, qf_losers, sf_losers_vec;
  int finalist = -1, champion = -1;

  // 4th place eliminated
  for (int g = 0; g < n_groups; ++g)
    group_out.push_back(standings[g][3].id);

  // Collect 3rd place teams
  std::vector<TeamStat> thirds(n_groups);
  for (int g = 0; g < n_groups; ++g)
    thirds[g] = standings[g][2];

  // Select best 8 thirds; capture their group letters in rank order
  std::vector<std::string> best8_groups;
  std::vector<TeamStat> best8_stats = select_best_thirds(thirds, grp_labels, best8_groups);

  // Non-advancing thirds → group_out
  std::unordered_set<int> best8_ids_set;
  for (auto& ts : best8_stats) best8_ids_set.insert(ts.id);
  for (int g = 0; g < n_groups; ++g) {
    int tid = standings[g][2].id;
    if (!best8_ids_set.count(tid)) group_out.push_back(tid);
  }

  // ── Annex C lookup ────────────────────────────────────────────────────────
  // combo_key = sorted 8 group letters joined, e.g. "ABCDEFGH"
  std::vector<std::string> sorted_groups = best8_groups;
  std::sort(sorted_groups.begin(), sorted_groups.end());
  std::string combo_key;
  for (auto& s : sorted_groups) combo_key += s;

  static const auto annexC = build_annexC();
  auto ac_it = annexC.find(combo_key);
  if (ac_it == annexC.end())
    Rcpp::stop("Annex-C combination not found: " + combo_key);
  const std::string& assign_str = ac_it->second; // positions: A,B,D,E,G,I,K,L

  // Position order of winners in assign_str
  const std::vector<std::string> winner_order = {"A","B","D","E","G","I","K","L"};

  // Map: group letter → third team ID (only for the 8 qualifying thirds)
  std::unordered_map<std::string, int> group_to_third_id;
  for (auto& ts : best8_stats) {
    for (int g = 0; g < n_groups; ++g) {
      if (standings[g][2].id == ts.id) {
        group_to_third_id[grp_labels[g]] = ts.id;
        break;
      }
    }
  }

  // Helper: given a winner's group letter, return the Annex-C-assigned third's team ID
  auto third_id_for_winner = [&](const std::string& winner_grp) -> int {
    for (int i = 0; i < 8; ++i) {
      if (winner_order[i] == winner_grp) {
        std::string src_group(1, assign_str[i]);
        auto it = group_to_third_id.find(src_group);
        if (it != group_to_third_id.end()) return it->second;
      }
    }
    return -1;
  };

  // Helper: get team ID by group label + 0-indexed rank
  auto gt = [&](const std::string& grp, int rank) -> int {
    int gi = grp_idx.count(grp) ? grp_idx[grp] : -1;
    if (gi < 0 || rank >= (int)standings[gi].size()) return -1;
    return standings[gi][rank].id;
  };

  // ── Round of 32 pairs ─────────────────────────────────────────────────────
  std::vector<std::pair<int,int>> r32_pairs = {
    {gt("A",1), gt("B",1)},
    {gt("C",0), gt("F",1)},
    {gt("E",0), third_id_for_winner("E")},
    {gt("F",0), gt("C",1)},
    {gt("E",1), gt("I",1)},
    {gt("I",0), third_id_for_winner("I")},
    {gt("A",0), third_id_for_winner("A")},
    {gt("L",0), third_id_for_winner("L")},
    {gt("G",0), third_id_for_winner("G")},
    {gt("D",0), third_id_for_winner("D")},
    {gt("H",0), gt("J",1)},
    {gt("K",1), gt("L",1)},
    {gt("B",0), third_id_for_winner("B")},
    {gt("D",1), gt("G",1)},
    {gt("J",0), gt("H",1)},
    {gt("K",0), third_id_for_winner("K")}
  };

  std::vector<int> r32_winners;
  for (auto& p : r32_pairs) {
    if (p.first < 0 || p.second < 0) { r32_winners.push_back(-1); continue; }
    auto [w, l] = simulate_ko_match(p.first, p.second, prob_map, rng);
    r32_winners.push_back(w);
    r32_losers.push_back(l);
  }

  // Round of 16
  std::vector<int> r16_winners;
  for (int i = 0; i < 16; i += 2) {
    if (r32_winners[i] < 0 || r32_winners[i+1] < 0) {
      r16_winners.push_back(-1); continue;
    }
    auto [w, l] = simulate_ko_match(r32_winners[i], r32_winners[i+1], prob_map, rng);
    r16_winners.push_back(w);
    r16_losers.push_back(l);
  }

  // Quarter-finals
  std::vector<int> qf_winners;
  for (int i = 0; i < 8; i += 2) {
    if (r16_winners[i] < 0 || r16_winners[i+1] < 0) {
      qf_winners.push_back(-1); continue;
    }
    auto [w, l] = simulate_ko_match(r16_winners[i], r16_winners[i+1], prob_map, rng);
    qf_winners.push_back(w);
    qf_losers.push_back(l);
  }

  // Semi-finals
  std::vector<int> sf_winners;
  for (int i = 0; i < 4; i += 2) {
    if (qf_winners[i] < 0 || qf_winners[i+1] < 0) {
      sf_winners.push_back(-1); sf_losers_vec.push_back(-1); continue;
    }
    auto [w, l] = simulate_ko_match(qf_winners[i], qf_winners[i+1], prob_map, rng);
    sf_winners.push_back(w);
    sf_losers_vec.push_back(l);
  }

  // Final
  if (sf_winners.size() >= 2 && sf_winners[0] > 0 && sf_winners[1] > 0) {
    auto [w, l] = simulate_ko_match(sf_winners[0], sf_winners[1], prob_map, rng);
    champion = w;
    finalist = l;
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
    Named("group_labels")  = wrap(grp_labels)
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
    int    seed_base = 42)
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

  std::unordered_map<int, std::array<int,7>> elim;
  for (int id : all_ids) elim[id] = {0,0,0,0,0,0,0};

  std::unordered_map<int, int> gwin;
  for (int id : all_ids) gwin[id] = 0;

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

    IntegerVector gw = r["group_winners"];
    for (int id : gw)
      if (gwin.count(id)) gwin[id]++;
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
    Named("id")            = ids_out,
    Named("Group.Stage")   = p_group,
    Named("Round.of.32")   = p_r32,
    Named("Round.of.16")   = p_r16,
    Named("Quarter.Final") = p_qf,
    Named("Semi.Final")    = p_sf,
    Named("Final")         = p_final,
    Named("Champion")      = p_champ,
    Named("GroupWinner")   = p_gwin,
    Named("n_sims")        = n_sims
  );
}

