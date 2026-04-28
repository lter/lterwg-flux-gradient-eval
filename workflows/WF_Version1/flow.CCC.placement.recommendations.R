# ---------------------------------------------------------------
# flow.CCC.placement.recommendations.R
#
# Recommended sensor placement for Gradient Flux methods, derived from
# where CCC peaks in SITES_One2One_canopy_model.
#
# Layout: 2 (short / tall canopy) x 2 (CO2 / H2O) panels, plus a
# practical-rules text panel.
#
# Output: CCC_placement_recommendations.{png,pdf} in the FluxGradient
# project root.
#
# Convention used by the SITES_One2One_canopy_model frame:
#   MeasurementHeight_m_A < MeasurementHeight_m_B for every row,
#   so z_A is the LOWER sensor and z_B is the UPPER sensor.
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(patchwork)
  library(grid)
})

# --- Paths ------------------------------------------------------
root_dir <- "/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient"
rdata    <- file.path(root_dir, "lterwg-flux-gradient-evalSITES_One2One_canopy_model.Rdata")
fig_dir <- '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/'

out_png  <- file.path(fig_dir , "CCC_placement_recommendations.png")

# --- Load -------------------------------------------------------
load(rdata)
stopifnot(exists("SITES_One2One_canopy_model"))

approach_palette <- c(MBR = "#B8860B", AE = "#2E8B57", WP = "#6A0DAD")
methods <- c("MBR", "AE", "WP")

# Filter & derive geometry
df <- SITES_One2One_canopy_model %>%
  as_tibble() %>%
  filter(Canopy_L1 %in% c("AA", "AW")) %>%
  mutate(
    Approach = factor(Approach, levels = methods),
    RelLower = MeasurementHeight_m_A - CanopyHeight,   # lower minus h_c
    RelUpper = MeasurementHeight_m_B - CanopyHeight,   # upper minus h_c
    Sep      = MeasurementHeight_m_B - MeasurementHeight_m_A,
    CanopyClass = cut(CanopyHeight,
                      breaks = c(-0.1, 1, 5, 15, 100),
                      labels = c("Very short", "Short", "Medium", "Tall"),
                      include.lowest = TRUE)
  )

# --- Optimal ranges: top-quartile CCC rows ----------------------
optimal_ranges <- function(sub, gas_name) {
  sub %>%
    filter(gas == gas_name) %>%
    group_by(Approach) %>%
    arrange(desc(CCC), .by_group = TRUE) %>%
    slice_head(prop = 0.25) %>%
    summarise(
      n         = n(),
      ccc_med   = median(CCC, na.rm = TRUE),
      lower_med = median(RelLower, na.rm = TRUE),
      lower_q25 = quantile(RelLower, 0.25, na.rm = TRUE),
      lower_q75 = quantile(RelLower, 0.75, na.rm = TRUE),
      upper_med = median(RelUpper, na.rm = TRUE),
      upper_q25 = quantile(RelUpper, 0.25, na.rm = TRUE),
      upper_q75 = quantile(RelUpper, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
}

canopy_buckets <- list(
  list(label = "Short canopy (2 m)", h_c = 2, classes = c("Short", "Very short")),
  list(label = "Tall canopy (25 m)", h_c = 25, classes = c("Tall"))
)

# --- Single panel builder ---------------------------------------
make_panel <- function(opt, h_c, panel_title) {
  # Build a long data frame for both lanes
  bars <- bind_rows(
    opt %>% transmute(Approach,
                      lane = "z[A] (lower)",
                      lane_idx = match(Approach, methods),
                      ymin = h_c + lower_q25,
                      ymax = h_c + lower_q75,
                      ymed = h_c + lower_med,
                      label = sprintf("%+.0f", lower_med)),
    opt %>% transmute(Approach,
                      lane = "z[B] (upper)",
                      lane_idx = match(Approach, methods),
                      ymin = h_c + upper_q25,
                      ymax = h_c + upper_q75,
                      ymed = h_c + upper_med,
                      label = sprintf("%+.0f", upper_med))
  ) %>%
    mutate(lane = factor(lane, levels = c("z[A] (lower)", "z[B] (upper)")),
           x = (lane_idx - 2) * 0.6)   # 3 lanes centered on 0

  y_top <- h_c + max(c(opt$upper_q75, 20)) + 4
  y_bot <- min(-1.5, h_c + min(opt$lower_q25) - 2)

  # Canopy polygon (ellipse approx) and trunk
  theta <- seq(0, 2 * pi, length.out = 80)
  canopy_w <- if (h_c >= 5) 2.2 else 2.6
  canopy_centre <- if (h_c >= 5) h_c * 0.7 else h_c * 0.55
  canopy_h <- if (h_c >= 5) h_c * 0.65 else h_c * 1.0
  canopy_df <- tibble(
    xc = (canopy_w / 2) * cos(theta),
    yc = canopy_centre + (canopy_h / 2) * sin(theta)
  )
  trunk_df <- tibble(xmin = -0.10, xmax = 0.10, ymin = 0,
                     ymax = if (h_c >= 5) h_c * 0.45 else 0)

  # CCC summary string
  ccc_text <- paste(opt$Approach, sprintf("%.2f", opt$ccc_med),
                    sep = " ", collapse = ", ")
  ccc_text <- paste0("Top-quartile CCC: ", ccc_text)

  ggplot() +
    facet_wrap(~ lane, nrow = 1, labeller = label_parsed) +
    # Ground line
    geom_hline(yintercept = 0, linewidth = 0.5) +
    # Canopy top h_c reference
    geom_hline(yintercept = h_c, linetype = "dashed",
               color = "#1b5e20", alpha = 0.5) +
    # Trunk
    geom_rect(data = trunk_df,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "#5d4037") +
    # Canopy crown
    geom_polygon(data = canopy_df,
                 aes(x = xc, y = yc),
                 fill = "#388e3c", color = "#1b5e20", alpha = 0.85) +
    # Recommendation rectangles
    geom_rect(data = bars,
              aes(xmin = x - 0.18, xmax = x + 0.18,
                  ymin = ymin, ymax = ymax,
                  fill = Approach),
              alpha = 0.55) +
    # Median tick + diamond
    geom_segment(data = bars,
                 aes(x = x - 0.22, xend = x + 0.22,
                     y = ymed, yend = ymed),
                 linewidth = 0.6, color = "black") +
    geom_point(data = bars,
               aes(x = x, y = ymed, fill = Approach),
               shape = 23, size = 2.4, stroke = 0.5, color = "black") +
    # Method label above each bar
    geom_text(data = bars,
              aes(x = x, y = ymax + (y_top - y_bot) * 0.025,
                  label = Approach, color = Approach),
              size = 3, fontface = "bold", show.legend = FALSE) +
    # Median value next to diamond
   # geom_text(data = bars,
  #            aes(x = x + 0.30, y = ymed, label = label, color = Approach),
   #           hjust = 1, size = 2.6, show.legend = FALSE, vjust = -3) +
    # h_c label
    annotate("text", x = canopy_w / 2 + 0.15, y = h_c,
             label = "h[c]", parse = TRUE,
             color = "#1b5e20", hjust = 0, fontface = "bold", size = 3.6) +
    annotate("text", x = 0, y = canopy_centre,
             label = sprintf("h[c] %s %d~m", "%~~%", as.integer(h_c)),
             parse = TRUE, color = "white", fontface = "bold", size = 3) +
    # CCC summary box (annotation_custom is brittle in facet — use top corner text)
    coord_cartesian(xlim = c(-1.6, 1.6),
                    ylim = c(y_bot, y_top), clip = "off") +
    scale_fill_manual(values = approach_palette) +
    scale_color_manual(values = approach_palette) +
    labs(title = panel_title,
         #subtitle = ccc_text,
         x = NULL, y = "Height above ground (m)") +
    theme_bw(base_size = 10) +
    theme(
      plot.title    = element_text(face = "bold"),
      plot.subtitle = element_text(size = 8.5, color = "grey20"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold"),
      legend.position = "none"
    )
  
  
  
}

# --- Build all 4 sub-panels -------------------------------------
panels <- list()
for (cb in canopy_buckets) {
  for (g in c("CO2", "H2O")) {
    sub <- df %>% filter(CanopyClass %in% cb$classes)
    opt <- optimal_ranges(sub, g)
    title <- sprintf("%s -- %s",
                     cb$label,
                     ifelse(g == "CO2", "CO[2]", "H[2]*O"))
    p <- make_panel(opt, cb$h_c, title) +
      labs(title = parse(text = paste0("\"", cb$label, " -- \"~",
                                       ifelse(g == "CO2", "CO[2]", "H[2]*O"))))
    panels[[paste(cb$label, g, sep = "_")]] <- p
  }
}

# --- Recommendation text panel ----------------------------------
rec_text <- paste0(
  "PRACTICAL PLACEMENT RULES\n",
  "─────────────────────────\n",
  "Short canopy (h_c <= 5 m, e.g. grasslands, shrublands, croplands):\n",
  "    Put z_A at the canopy top (within +-2 m) and z_B about 5-10 m higher. ",
  "All three methods (MBR, AE, WP) agree on this geometry for both gases. ",
  "MBR will dominate; AE and WP follow with smaller gains.\n\n",
  "Tall canopy (h_c >= 15 m, e.g. forests):\n",
  "    CO2  -- Source is split (canopy uptake + soil respiration). Put z_A near or just below the canopy top ",
  "(-5 to +5 m) and z_B about 10-20 m above; both above-canopy is best. ",
  "MBR gives the highest CCC; AE/WP are weaker.\n",
  "    H2O  -- Source is the canopy itself (transpiration). For MBR keep both above-canopy. ",
  "For AE and WP, drop z_A INSIDE the canopy (about 15-20 m below the top, i.e. trunk space) so the ",
  "gradient straddles the source -- this almost doubles their CCC. Pair with z_B about 10-15 m above the canopy.\n\n",
  "Why the asymmetry: H2O has a single, strong source (transpiring leaves), so the largest gradient sits across the ",
  "canopy. CO2 has competing sources/sinks within the canopy, so the cleanest gradient is in the constant-flux ",
  "layer above. MBR normalises by a co-located heat flux, so it benefits from above-canopy clearance for both gases."
)

text_panel <- ggplot() +
  annotate("text", x = 0, y = 1, label = rec_text, hjust = 0, vjust = 1,
           size = 3.2, family = "mono") +
  xlim(0, 1) + ylim(0, 1) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#fffbe6", color = "#bbb",
                                       linewidth = 0.4))

# --- Compose ----------------------------------------------------
grid_panels <- (panels[["Short canopy (2 m)_CO2"]] | panels[["Short canopy (2 m)_H2O"]]) /
               (panels[["Tall canopy (25 m)_CO2"]]  | panels[["Tall canopy (25 m)_H2O"]])

design <- grid_panels  +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Optimum sensor placement for Gradient Flux approaches",
   #subtitle = "Bands = IQR of top-quartile-CCC pairs in NEON data; diamonds = median",
    theme = theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5, size = 10))
  )

ggsave(out_png, design, width = 7.5, height = 6, dpi = 220, bg = "white")

message("Saved: ", out_png)

