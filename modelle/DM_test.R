library(forecast)

rf <- read.csv("dm_rf_data.csv")
poi <- read.csv("dm_poisson_data.csv")

cat(sprintf("RF-Daten: %d Spiele, Poisson-Daten: %d Spiele\n\n", nrow(rf), nrow(poi)))

dm <- function(label, se_base, se_model) {
  r <- dm.test(ts(se_base), ts(se_model), alternative="greater", h=1, power=1)
  cat(sprintf("  %-20s DM=%+.3f  p=%.4f\n", label, r$statistic, r$p.value))
}

for (thresh in c(0, 1600, 1700, 1800)) {
  lbl <- if (thresh == 0) "Alle Spiele" else sprintf("Beide >= %d", thresh)
  rf_sub <- if (thresh == 0) rf else rf[rf$min_elo >= thresh,]
  poi_sub <- if (thresh == 0) poi else poi[poi$min_elo >= thresh,]
  if (nrow(rf_sub) < 10 | nrow(poi_sub) < 10) next
  cat(sprintf("\n%s\n", lbl))
  dm("Random Forest", rf_sub$se_real, rf_sub$se_rf)
  dm("Poisson + xG", poi_sub$se_real, poi_sub$se_poisson)
}

# Robustheit >=1800 (Wilcoxon Test)
cat("\nRobustheit >=1800\n")
rs <- rf[rf$min_elo >= 1800,]
ps <- poi[poi$min_elo >= 1800,]
cat(sprintf("  Baseline:            Brier=%.4f (%d Spiele)\n", mean(rs$se_real), nrow(rs)))

w_rf <- wilcox.test(rs$se_real, rs$se_rf, alternative="greater", paired=TRUE)
cat(sprintf("  Random Forest        Brier=%.4f (%+.1f%%)  Wilcoxon p=%.4f\n",
            mean(rs$se_rf), (mean(rs$se_rf)-mean(rs$se_real))/mean(rs$se_real)*100, w_rf$p.value))

w_poi <- wilcox.test(ps$se_real, ps$se_poisson, alternative="greater", paired=TRUE)
cat(sprintf("  Poisson + xG         Brier=%.4f (%+.1f%%)  Wilcoxon p=%.4f\n",
            mean(ps$se_poisson), (mean(ps$se_poisson)-mean(ps$se_real))/mean(ps$se_real)*100, w_poi$p.value))
