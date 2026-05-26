library(forecast)

# CSV vom Random-Forest-Skript exportiert. Enthält pro Test-Spiel:
#   se_real  = quadratischer Fehler der eloratings.net-Baseline
#   se_rf    = quadratischer Fehler des Random-Forest-Modells
#   min_elo  = niedrigeres der beiden Team-Elos (für Segmentierung)
rf <- read.csv("dm_rf_data.csv")

# Diebold-Mariano-Test: vergleicht die Vorhersagefehler zweier Modelle
# auf denselben Beobachtungen. Im Gegensatz zu einem t-Test berücksichtigt
# er die Korrelation der Fehler, beide Modelle treffen ja auf dasselbe Spiel.
#
# alternative = "greater" prüft die einseitige Hypothese:
#   "Baseline-Fehler sind systematisch größer als RF-Fehler"
#   → ein signifikantes Ergebnis bedeutet, das RF ist besser
#
# power = 2: quadratische Verlustfunktion, konsistent zum Brier Score
# h = 1: Standardeinstellung für nicht-zeitreihen-strukturierte Vorhersagen
dm <- function(label, se_base, se_model) {
  r <- dm.test(ts(se_base), ts(se_model),
               alternative = "greater", h = 1, power = 2)
  cat(sprintf("  %-20s DM=%+.3f  p=%.4f\n",
              label, r$statistic, r$p.value))
}

# Auswertung über vier Segmente: alle Spiele und drei Elo-Schwellen.
# Die Segmentierung zeigt, ob der RF-Vorteil mit der Spielstärke wächst —
# bei stärkeren Teams hat Elo allein weniger Differenzierungskraft,
# weshalb zusätzliche Features dort mehr beitragen sollten.
for (thresh in c(0, 1600, 1700, 1800)) {
  lbl    <- if (thresh == 0) "Alle Spiele" else sprintf("Beide >= %d", thresh)
  rf_sub <- if (thresh == 0) rf else rf[rf$min_elo >= thresh, ]
  if (nrow(rf_sub) < 10) next   # zu kleine Stichprobe für stabilen Test
  cat(sprintf("\n%s (n = %d)\n", lbl, nrow(rf_sub)))
  dm("Random Forest", rf_sub$se_real, rf_sub$se_rf)
}