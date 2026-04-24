cache_dir <- "/srv/shiny-server/wm2026/data/cache"
dir.create(cache_dir, showWarnings=FALSE, recursive=TRUE)

tryCatch({
  download.file("https://www.eloratings.net/World.tsv",
                file.path(cache_dir, "elo.tsv"), quiet=TRUE)
  download.file("https://www.eloratings.net/en.teams.tsv",
                file.path(cache_dir, "countries.tsv"), quiet=TRUE)
  cat(format(Sys.time()), "ELO cache updated\n",
      file="/srv/shiny-server/wm2026/elo_update.log", append=TRUE)
}, error=function(e) {
  cat(format(Sys.time()), "ERROR:", conditionMessage(e), "\n",
      file="/srv/shiny-server/wm2026/elo_update.log", append=TRUE)
})
