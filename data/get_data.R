library(devtools)
devtools::install_github("jfjelstul/worldcup")
devtools::install_github("JaseZiv/worldfootballR")

#includes womens and penatlies
data <- worldcup::matches

#no penalties
world_cups <- load_match_comp_results(comp_name = "FIFA World Cup")
