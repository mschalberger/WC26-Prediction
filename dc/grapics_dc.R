library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales)

# ── CONFIG ───────────────────────────────────────────────────
BG        <- "#FFFFFF"
PANEL_BG  <- "#FFFFFF"
WHITE     <- "#FFFFFF"
BLACK     <- "#000000"
BORDER    <- "#E6E6E6"
TEXT      <- "#000000"
MUTED     <- "#808080"
LIME      <- "#CCFF00"
NAVY      <- "#004659"   # --fublue
GREEN_LT  <- "#00A4D1"
RED_LT    <- "#E57050"
DRAW_COL  <- "#58756A"

# Score matrix cell fills (pastel, coloured per outcome region)
FILL_WIN  <- "#CCFF00"   # pale green
FILL_DRAW <- "#00A4D1"   # pale lime/yellow
FILL_LOSS <- "#E57050"   # pale red


FONT <- "sans"


theme_wc <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      text              = element_text(family = FONT, colour = TEXT),
      plot.background   = element_rect(fill = BG,       colour = NA),
      panel.background  = element_rect(fill = PANEL_BG, colour = NA),
      panel.border      = element_rect(fill = NA, colour = BORDER, linewidth = 0.8),
      panel.grid.major  = element_line(colour = BORDER,  linewidth = 0.35),
      panel.grid.minor  = element_blank(),
      axis.text         = element_text(colour = MUTED,  size = rel(0.85)),
      axis.title        = element_text(colour = TEXT,   size = rel(0.95), face = "bold"),
      plot.title        = element_text(colour = TEXT,   size = rel(1.22), face = "bold",
                                       margin = margin(b = 5)),
      plot.subtitle     = element_text(colour = MUTED,  size = rel(0.82),
                                       margin = margin(b = 10)),
      plot.caption      = element_text(colour = MUTED,  size = rel(0.70),
                                       hjust = 0, margin = margin(t = 8)),
      legend.background = element_rect(fill = PANEL_BG, colour = NA),
      legend.key        = element_rect(fill = PANEL_BG, colour = NA),
      legend.text       = element_text(colour = TEXT),
      legend.title      = element_text(colour = MUTED, face = "bold"),
      # Lime header with black text — mirrors .group-header in light mode
      strip.background  = element_rect(fill = LIME, colour = NA),
      strip.text        = element_text(colour = TEXT, face = "bold", size = rel(0.88)),
      plot.margin       = margin(16, 16, 12, 16)
    )
}

plot_germany_matrix_dc <- function(dc, teams_init,
                                   germany_name = "Deutschland",
                                   max_g = 5,           # still predict up to max_g internally
                                   opps) {

  DISPLAY_MAX <- 4L   # last displayed bin  (0, 1, 2, 3, 4+)

  plots <- lapply(seq_len(nrow(opps)), function(i) {

    opp_nm <- opps$team_name[i]

    x1 <- x2 <- matrix(c(0, 0), ncol = 2)
    colnames(x1) <- colnames(x2) <- c("home", "host")

    # ── Raw grid (up to max_g goals) ──
    raw_grid <- predict_goals(dc,
                              team1 = teams_init %>% filter(team_name == germany_name) %>% pull(id),
                              team2 = teams_init %>% filter(team_name == opp_nm)       %>% pull(id),
                              x1 = x1, x2 = x2,
                              maxgoal = max_g,
                              return_df = TRUE) %>%
      mutate(prob = ifelse(is.na(probability), 0, probability))

    # ── Collapse goals >= DISPLAY_MAX into one bin ──
    grid <- raw_grid %>%
      mutate(
        g1 = pmin(goals1, DISPLAY_MAX),
        g2 = pmin(goals2, DISPLAY_MAX)
      ) %>%
      group_by(g1, g2) %>%
      summarise(prob = sum(prob), .groups = "drop") %>%
      rename(goals1 = g1, goals2 = g2) %>%
      mutate(
        result = case_when(
          goals1 > goals2 ~ "Sieg",
          goals1 < goals2 ~ "Niederlage",
          TRUE            ~ "Remis"
        ),
        label = ifelse(
          prob >= 0.005,
          sprintf("%.1f%%", prob * 100),
          "<.05%"
        )
      )

    # ── Colour gradient per region ──
    grid <- grid %>%
      mutate(
        region    = case_when(
          goals1 > goals2 ~ "win",
          goals1 < goals2 ~ "loss",
          TRUE            ~ "draw"
        ),
        prob_norm = if (max(prob) > 0) prob / max(prob) else 0
      )

    grid$fill_col <- mapply(
      function(reg, p) {
        end_col <- switch(reg,
                          win  = FILL_WIN,
                          loss = FILL_LOSS,
                          draw = FILL_DRAW)
        scales::seq_gradient_pal("white", end_col)(p)
      },
      grid$region, grid$prob_norm
    )

    # ── Axis labels: 0-3 numeric, 4 → "4+" ──
    axis_breaks  <- 0:DISPLAY_MAX
    axis_labels  <- c(as.character(0:(DISPLAY_MAX - 1)), paste0(DISPLAY_MAX, "+"))

    # ── 5×5 Heatmap ──
    p_heat <- ggplot(grid, aes(x = goals1, y = goals2)) +
      geom_tile(aes(fill = fill_col), colour = NAVY, linewidth = 0.5) +
      geom_text(aes(label = label),
                colour = TEXT, size = 2.9, fontface = "bold", family = FONT) +
      scale_fill_identity() +
      scale_x_continuous(breaks = axis_breaks, labels = axis_labels,
                         expand = expansion(add = 0.52)) +
      scale_y_continuous(breaks = axis_breaks, labels = axis_labels,
                         expand = expansion(add = 0.52)) +
      labs(
        x = "Tore Deutschland",
        y = sprintf("Tore %s", opp_nm)
      ) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background  = element_blank(),
        legend.position  = "none"
      )

    # ── W/D/L bar (unchanged) ──
    wdl_df <- predict_result(dc,
                             team1 = teams_init %>% filter(team_name == germany_name) %>% pull(id),
                             team2 = teams_init %>% filter(team_name == opp_nm)       %>% pull(id),
                             x1 = x1, x2 = x2,
                             return_df = TRUE) %>%
      pivot_longer(cols = c(p1, pd, p2),
                   names_to  = "outcome",
                   values_to = "probability") %>%
      mutate(
        prob   = ifelse(is.na(probability), 0, probability),
        result = case_when(
          outcome == "p1" ~ "Sieg",
          outcome == "p2" ~ "Niederlage",
          outcome == "pd" ~ "Remis",
          TRUE            ~ NA_character_
        ),
        result = factor(result, levels = c("Sieg", "Remis", "Niederlage"))
      )

    wdl_repel <- wdl_df %>%
      arrange(desc(result)) %>%
      mutate(
        ymax = cumsum(prob),
        ymin = ymax - prob,
        ymid = (ymin + ymax) / 2,
        lbl  = paste0(round(prob * 100), "%")
      )

    # Split into direct labels (>= 10%) and repelled labels (< 10%)
    wdl_direct <- wdl_repel %>% filter(prob >= 0.1)
    wdl_small  <- wdl_repel %>% filter(prob <  0.1)

    # ── Bar ──
    p_bar <- ggplot(wdl_df, aes(x = 1, y = prob, fill = result)) +
      geom_col(width = 0.4, linewidth = 0.5, colour = NAVY) +
      # Direct labels for large segments — centred in the bar
      geom_text(
        data = wdl_direct,
        aes(x = 1, y = ymid, label = lbl),
        hjust    = 0.5,
        colour   = TEXT,
        size     = 2.9,
        fontface = "bold",
        family   = FONT
      ) +
      ggrepel::geom_text_repel(
        data = wdl_small,
        aes(x = 1, y = ymid, label = lbl),
        direction          = "y",
        nudge_y            = -0.03,
        nudge_x            = 0.45,
        hjust              = 0,
        force              = 2.0,
        force_pull         = 0.05,
        segment.color      = NAVY,
        segment.size       = 0.3,
        segment.linetype   = "dotted",
        min.segment.length = 0,
        box.padding        = 0.15,
        colour    = TEXT,
        size      = 2.9,
        fontface  = "bold",
        family    = FONT,
        xlim      = c(NA, 2.4)
      ) +
      coord_flip() +
      scale_fill_manual(
        values = c("Sieg" = LIME, "Remis" = FILL_DRAW, "Niederlage" = RED_LT),
        name   = NULL,
        breaks = c("Niederlage", "Remis", "Sieg")
      ) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background  = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        axis.title       = element_blank(),
        legend.position  = "bottom"
      )

    # ── Patchwork ──
    (p_heat / p_bar) +
      plot_layout(heights = c(5, 1)) +
      plot_annotation(
        title = sprintf("Deutschland gegen %s", opp_nm),
        theme = theme(
          plot.title    = element_text(colour = TEXT,  size = 15, face = "bold",
                                       family = FONT,  margin = margin(b = 4)),
          plot.subtitle = element_text(colour = MUTED, size = 9,  family = FONT,
                                       margin = margin(b = 6)),
          plot.margin   = margin(20, 20, 14, 20)
        )
      )
  })

  names(plots) <- opps$team_name
  plots
}

plot_champion_prob_dc <- function(dc_results, top_n = 10) {
  df <- dc_results$reach_df %>%
    arrange(desc(Champion)) %>%
    slice(1:top_n) %>%
    mutate(
      pct       = Champion * 100,
      team_name = factor(team_name, levels = rev(team_name))
    )

  ggplot(df, aes(x = pct, y = team_name)) +
    # Main lime bar
    geom_col(fill = LIME, colour = NA, width = 0.65) +
    geom_text(aes(label = sprintf("%.1f%%", pct)),
              hjust = -0.12, colour = TEXT,
              size = 3.7, fontface = "bold", family = FONT) +
    scale_x_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.22))) +
    labs(
      title    = "Weltmeisterwahrscheinlichkeiten",
      subtitle = sprintf("Die Besten %d Nationen  ·  %s Simulationen",
                         top_n, format(dc_results$n_sims, big.mark = ",")),
      x = NULL, y = NULL
    ) +
    theme_wc(11) +
    theme(
      axis.text.y        = element_text(size = 10.5, colour = TEXT, face = "bold"),
      axis.text.x        = element_text(size = 9,    colour = MUTED),
      panel.grid.major.y = element_blank()
    )
}

plot_germany_stages_dc <- function(dc_results, germany_name = "Deutschland") {
  stage_keys <- c("Round of 32","Round of 16","Quarter-Final",
                  "Semi-Final","Final","Champion")
  stage_lbls <- c("Sechzehntel-\nfinale","Achtel-\nfinale","Viertel-\nfinale",
                  "Halb-\nfinale","Finale","Weltmeister")

  ger <- dc_results$reach_df %>% filter(team_name == germany_name)
  if (nrow(ger) == 0) stop("Deutschland not found in results.")

  df <- data.frame(
    stage = factor(stage_keys, levels = stage_keys),
    label = stage_lbls,
    pct   = as.numeric(ger[1, stage_keys]) * 100
  ) %>%
    mutate(lbl = sprintf("%.1f%%", pct))

  ggplot(df, aes(x = stage, y = pct)) +
    geom_col(fill = LIME, colour = NA, width = 0.65) +
    geom_text(aes(label = lbl), vjust = -0.45, colour = TEXT,
              size = 3.7, fontface = "bold", family = FONT) +
    scale_x_discrete(labels = stage_lbls) +
    scale_y_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.18))) +
    labs(
      title    = "Wahrscheinlichkeiten für Deutschland",
      subtitle = sprintf("%s Simulationen", format(dc_results$n_sims, big.mark = ",")),
      x = NULL, y = NULL
    ) +
    theme_wc(11) +
    theme(
      axis.text.x        = element_text(size = 9, colour = TEXT, face = "bold"),
      axis.text.y        = element_text(size = 9, colour = MUTED),
      panel.grid.major.x = element_blank()
    )
}

plot_group_winners_dc <- function(dc_results, teams_init) {
  gadv_df <- dc_results$reach_df %>%
    select(id, adv_prob = `Round of 32`)

  df <-dc_results$reach_df %>%
    mutate(adv_prob = `Round of 32`,
           win_prob =`Group Winner`) %>%
    filter(adv_prob > 0.01) %>%   # optional filter
    group_by(group_letter) %>%
    arrange(desc(adv_prob)) %>%
    ungroup() %>%
    mutate(
      group_label = paste("GROUP", group_letter),
      group_label = factor(group_label,
                           levels = paste("GROUP", sort(unique(group_letter))))
    )

  ggplot(df, aes(x = win_prob * 100,
                 y = reorder(team_name, win_prob))) +
    geom_col(aes(x = adv_prob * 100),
             fill = MUTED, alpha = 0.35, width = 0.75) +
    geom_text(
      aes(x = adv_prob * 100,
          label = sprintf("%.0f%%", adv_prob * 100)),
      hjust = -0.15,
      colour = MUTED,
      size = 2.5,
      family = FONT
    ) +
    geom_col(aes(x = win_prob * 100),
             fill = LIME, width = 0.55) +
    geom_text(
      aes(x = win_prob * 100,
          label = ifelse(win_prob > 0.01,sprintf("%.0f%%", win_prob * 100),"")),
      hjust = -0.15,
      colour = TEXT,
      size = 2.6,
      fontface = "bold",
      family = FONT
    ) +
    facet_wrap(~ group_label, scales = "free_y", ncol = 2) +
    scale_x_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0, 0.32))) +
    labs(
      title    = "Gruppensieger",
      subtitle = sprintf(
        "%s Simulationen  ·  Nur Mannschaften mit > 0.5%% Wahrscheinlichkeit",
        format(dc_results$n_sims, big.mark = ",")),
      x = NULL, y = NULL
    ) +
    theme_wc(9) +
    theme(
      strip.text         = element_text(colour = TEXT, face = "bold", size = 8),
      strip.background   = element_rect(fill = LIME, colour = NA),
      axis.text.y        = element_text(size = 7.5, colour = TEXT),
      axis.text.x        = element_text(size = 7,   colour = MUTED),
      panel.grid.major.y = element_blank(),
      panel.spacing      = unit(0.7, "lines")
    )
}


team_de <- c(
  "Mexico"                 = "Mexiko",
  "South Africa"           = "Südafrika",
  "South Korea"            = "Südkorea",
  "Czechia"                = "Tschechien",
  "Canada"                 = "Kanada",
  "Bosnia and Herzegovina" = "Bosnien und Herzegowina",
  "Qatar"                  = "Katar",
  "Switzerland"            = "Schweiz",
  "Brazil"                 = "Brasilien",
  "Morocco"                = "Marokko",
  "Haiti"                  = "Haiti",
  "Scotland"               = "Schottland",
  "USA"                    = "USA",
  "Paraguay"               = "Paraguay",
  "Australia"              = "Australien",
  "Turkey"                 = "Türkei",
  "Germany"                = "Deutschland",
  "Curaçao"                = "Curaçao",
  "Côte d'Ivoire"          = "Côte d'Ivoire",
  "Ecuador"                = "Ecuador",
  "Netherlands"            = "Niederlande",
  "Japan"                  = "Japan",
  "Sweden"                 = "Schweden",
  "Tunisia"                = "Tunesien",
  "Belgium"                = "Belgien",
  "Egypt"                  = "Ägypten",
  "IR Iran"                = "Iran",
  "New Zealand"            = "Neuseeland",
  "Spain"                  = "Spanien",
  "Cabo Verde"             = "Kap Verde",
  "Saudi Arabia"           = "Saudi-Arabien",
  "Uruguay"                = "Uruguay",
  "France"                 = "Frankreich",
  "Senegal"                = "Senegal",
  "Iraq"                   = "Irak",
  "Norway"                 = "Norwegen",
  "Argentina"              = "Argentinien",
  "Algeria"                = "Algerien",
  "Austria"                = "Österreich",
  "Jordan"                 = "Jordanien",
  "Portugal"               = "Portugal",
  "DR Congo"               = "DR Kongo",
  "Uzbekistan"             = "Usbekistan",
  "Colombia"               = "Kolumbien",
  "England"                = "England",
  "Croatia"                = "Kroatien",
  "Ghana"                  = "Ghana",
  "Panama"                 = "Panama"
)
# Hilfsfunktion: alles, was nicht in der Tabelle ist, bleibt unverändert
translate <- function(x) ifelse(x %in% names(team_de), team_de[x], x)

teams <- read.csv("data/teams.csv")
dc_results <- readRDS("dc/dc_results.rds")

teams$team_name  <- translate(teams$team_name)
dc_results$reach_df$team_name       <- translate(dc_results$reach_df$team_name)

opps_aux = teams %>%
  filter(group_letter == "E", team_name != "Deutschland")

p1 <- plot_germany_matrix_dc(dc, teams, germany_name = "Deutschland", max_g = 8, opps = opps_aux)
ggsave("Figures/GERCuracao_dc.png",
       plot   = p1[1],
       width  = 6, height = 4,
       dpi    = 300,
       bg     = "white")
ggsave("Figures/GERCoteDIvoire_dc.png",
       plot   = p1[2],
       width  = 6, height = 4,
       dpi    = 300,
       bg     = "white")

ggsave("Figures/GEREcuador_dc.png",
       plot   = p1[3],
       width  = 6, height = 4,
       dpi    = 300,
       bg     = "white")

p2 <- plot_champion_prob_dc(dc_results, top_n = 10)
ggsave("Figures/Winners_dc.png",
       plot   = p2,
       width  = 6, height = 4,
       dpi    = 300,
       bg     = "white")

p3 <- plot_germany_stages_dc(dc_results, germany_name = "Deutschland")
ggsave("Figures/GERWeiterkommen_dc.png",
       plot   = p3,
       width  = 6, height = 4,
       dpi    = 300,
       bg     = "white")

p4 <- plot_group_winners_dc(dc_results, teams)
ggsave("Figures/Gruppensieger_dc.png",
       plot   = p4,
       width  = 6, height = 12,
       dpi    = 300,
       bg     = "white")

