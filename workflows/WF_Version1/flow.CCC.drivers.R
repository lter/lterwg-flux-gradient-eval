# ---------------------------------------------------------------
# flow.CCC.drivers.R
#
# Reproduce the CCC drivers explanatory figure for both CO2 and H2O
# from the SITES_One2One_canopy_model object produced by
# workflows/WF_Version1/flow.evaluation.GoodDrivers.V2.R.
#
# Question answered:
#   What about wind profiles, gas profiles, and canopy structure
#   produces strong CCC between Gradient Flux (GF) and Eddy
#   Covariance (EC)?
#
# Filtering: gas in {CO2, H2O}, Canopy_L1 in {AA, AW} (WW excluded).
#
# Output: CCC_drivers_explained_CO2.{png,pdf} and
#         CCC_drivers_explained_H2O.{png,pdf} in FluxGradient root.
# ---------------------------------------------------------------

# --- Packages ---------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(ggpubr)
  library(scales)
  library(grid)
  library(patchwork)   # panel layout
  library(ggrepel)     # label every site in panel F without overlap
})

# --- Paths ------------------------------------------------------
root_dir <- "/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient"
rdata    <- file.path(root_dir, "lterwg-flux-gradient-evalSITES_One2One_canopy_model.Rdata")
fig_dir <- '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/'

# --- Load -------------------------------------------------------
# This file contains: train, test, rf_model, final.vars,
#                     SITES_One2One_canopy, SITES_One2One_canopy_model
load(rdata)
stopifnot(exists("SITES_One2One_canopy_model"))

approach_palette <- c(MBR = "#B8860B", AE = "#2E8B57", WP = "#6A0DAD")
canopy_palette   <- c(AA = "#1f77b4", AW = "#ff7f0e")

base_theme <- theme_bw(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        plot.title  = element_text(face = "bold", size = 15, hjust = 0),
        axis.title  = element_text(size = 13),
        axis.text   = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text  = element_text(size = 12),
        strip.text   = element_text(size = 12, face = "bold"),
        legend.position = "right")

# ---------------------------------------------------------------
# Helper: compute bin centers from a cut() factor
# ---------------------------------------------------------------
.bin_center <- function(bin) {
  vapply(strsplit(gsub("[^0-9.,-]", "", as.character(bin)), ","),
         function(v) mean(as.numeric(v)), numeric(1))
}

# ---------------------------------------------------------------
# Main builder: returns a patchwork that can be ggsave()'d.
# Takes:
#   df  -- the SITES_One2One_canopy_model frame (already loaded)
#   gas -- "CO2" or "H2O"
# ---------------------------------------------------------------
build_ccc_drivers_figure <- function(df, gas = c("CO2", "H2O")) {
 # gas <- match.arg(gas)
  gas_label <- if (gas == "CO2") "CO[2]" else "H[2]*O"

  co <- df %>%
    as_tibble() %>%
    filter(gas == !!gas, Canopy_L1 %in% c("AA", "AW")) %>%
    mutate(
      Approach        = factor(Approach, levels = c("MBR", "AE", "WP")),
      Canopy_L1       = factor(Canopy_L1, levels = c("AA", "AW")),
      RelDistA        = MeasurementHeight_m_A - CanopyHeight,
      RelDistB        = MeasurementHeight_m_B - CanopyHeight,
      MidAboveCH      = (MeasurementHeight_m_A + MeasurementHeight_m_B) / 2 - CanopyHeight,
      MeasurementDist = MeasurementHeight_m_A - MeasurementHeight_m_B,
      Good            = as.integer(CCC >= 0.5)
    )

  # -------- Panel A schematic -----------------------------------
  ch <- 2.4
  panel_levels <- c("AA  Above-Above", "AW  Above-Within")
  geom_df <- tibble(panel = factor(panel_levels, levels = panel_levels),
                    color = c("violetred2",  "salmon1"))
  draw_df <- function(panel, color) {
    if (grepl("AA", panel)) { ya <- ch * 1.65; yb <- ch * 1.20 }
    else                    { ya <- ch * 1.55; yb <- ch * 0.55 }
    tibble(panel = panel, color = color,
           x_tower = 0, y_tower_top = ch * 1.85,
           x_canopy = 1.6, y_canopy = ch * 0.55,
           w_canopy = 1.6, h_canopy = ch * 0.9,
           xs = 0, y_zA = ya, y_zB = yb, hc_y = ch)
  }
  d <- map2_dfr(as.character(geom_df$panel), geom_df$color, draw_df)
  d$panel <- factor(d$panel, levels = panel_levels)

  A <- ggplot(d) +
    facet_wrap(~ panel, nrow = 1) +
    geom_segment(aes(x = -2.0, xend = 2.0, y = 0, yend = 0)) +
    geom_segment(aes(x = x_tower - 0.05, xend = x_tower - 0.05,
                     y = 0, yend = y_tower_top), linewidth = 0.8) +
    geom_polygon(data = d %>%
                   crossing(theta = seq(0, 2 * pi, length.out = 60)) %>%
                   mutate(xc = x_canopy + (w_canopy / 2) * cos(theta),
                          yc = y_canopy + (h_canopy / 2) * sin(theta)),
                 aes(x = xc, y = yc, group = panel),
                 fill = "#388e3c", alpha = 0.55, color = "#1b5e20") +
    geom_segment(aes(x = -2.5, xend = 2.5, y = hc_y, yend = hc_y),
                 linetype = "dashed", color = "#388e3c") +
    annotate("text", x = 2.6, y = ch, label = "h[c]", parse = TRUE,
             color = "#1b5e20", size = 5, hjust = 0) +
    geom_point(aes(x = xs, y = y_zA, color = panel), size = 4, shape = 15) +
    geom_point(aes(x = xs, y = y_zB, color = panel), size = 4, shape = 15) +
    geom_text(aes(x = xs + 0.25, y = y_zA, label = "z[A]"), parse = TRUE, hjust = 0, size = 5) +
    geom_text(aes(x = xs + 0.25, y = y_zB, label = "z[B]"), parse = TRUE, hjust = 0, size = 5) +
    scale_color_manual(values = setNames(geom_df$color, geom_df$panel), guide = "none") +
    coord_cartesian(xlim = c(-2.6, 2.6), ylim = c(-0.5, ch * 2.1)) +
    labs(title = "A.",
         x = NULL, y = NULL) +
    base_theme +
    theme(panel.grid = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), strip.text = element_text(face = "bold"),
          strip.background = element_blank())

  fracs <- co %>% group_by(Canopy_L1) %>%
    summarise(n = n(), good = sum(Good, na.rm = TRUE), .groups = "drop") %>%
    mutate(label = sprintf("%s: %d/%d = %.0f%% reach CCC >= 0.5",
                           Canopy_L1, good, n, 100 * good / n))
  
  A_caption <- paste(fracs$label, collapse = "    ") 
  
  A <- A + labs(caption = A_caption) 
    

  # -------- Panel B titles depend on gas ------------------------
  title_B <- "B."

  B <- ggplot(co, aes(x = Canopy_L1, y = CCC, fill = Approach)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.78,
                 position = position_dodge(width = 0.78), width = 0.7) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    scale_fill_manual(values = approach_palette) +
    labs(title = title_B,
         x = "",
         y = "CCC") +
    coord_cartesian(ylim = c(-0.6, 1.0)) +
    base_theme

  # -------- Panel C ---------------------------------------------
  relA_breaks <- c(-30, -10, -5, -2, 0, 2, 5, 10, 20, 40, 80)
  C_data <- co %>%
    mutate(bin = cut(RelDistA, breaks = relA_breaks, include.lowest = TRUE)) %>%
    group_by(Approach, bin) %>%
    summarise(ccc = mean(CCC, na.rm = TRUE), n = n(), .groups = "drop") %>%
    filter(n >= 5, !is.na(bin)) %>%
    mutate(center = .bin_center(bin))

  title_C <- "C."

  C <- ggplot(C_data, aes(x = center, y = ccc, color = Approach, shape = Approach)) +
    geom_line(linewidth = 0.7) + geom_point(size = 2.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "#388e3c") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", linewidth = 0.4)  +
    scale_color_manual(values = approach_palette) +
    scale_shape_manual(values = c(MBR = 16, AE = 15, WP = 17)) +
    labs(title = title_C,
         x = expression("Upper sensor height - canopy height (z"[A] * " - h"[c] * ", m)"),
         y = "Mean CCC") +
    coord_cartesian(xlim = c(-15, 40), ylim = c(-0.05, 0.9)) +
    base_theme +
    theme(legend.position = "none")
  

  # -------- Panel D ---------------------------------------------
  chm_breaks <- c(0, 0.5, 2, 5, 10, 20, 30, 45)
  D_data <- co %>%
    mutate(bin = cut(CHM.mean, breaks = chm_breaks, include.lowest = TRUE)) %>%
    group_by(Approach, bin) %>%
    summarise(ccc = mean(CCC, na.rm = TRUE), n = n(), .groups = "drop") %>%
    filter(n >= 5, !is.na(bin)) %>%
    mutate(center = .bin_center(bin))

  D <- ggplot(D_data, aes(x = center, y = ccc, color = Approach, shape = Approach)) +
    geom_line(linewidth = 0.7) + geom_point(size = 2.2) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", linewidth = 0.4) +
    scale_color_manual(values = approach_palette) +
    scale_shape_manual(values = c(MBR = 16, AE = 15, WP = 17)) +
    labs(title = "D.",
         x = "Mean canopy height (m)", y = "Mean CCC") +
    coord_cartesian(xlim = c(0, 40), ylim = c(-0.05, 0.8)) +
    base_theme+
    theme(legend.position = "none")

  # -------- Panel E heatmap of drivers --------------------------
  driver_groups <- list(
    "Wind / gas profile geometry" = c(
      `zA - hc (RelDistA)`               = "RelDistA",
      `zB - hc (RelDistB)`               = "RelDistB",
      `( zA + zB )/2 - hc`                 = "MidAboveCH",
      `zA - zB`    = "MeasurementDist"
    ),
    "Canopy structure (NEON AOP)" = c(
      `Canopy height (hc)`                 = "CanopyHeight",
      `Mean canopy h. (CHM)`            = "CHM.mean",
      `Vegetation Area Index (VAI)`          = "Cutoff05.Grain01.VAI",
      `Foliage Height Diversity (FHD)`       = "Cutoff05.Grain01.FHD",
      `Vertical Complexity Index (VCI)`      = "Cutoff05.VCI",
      `Top Rugosity`                         = "Cutoff05.TopRugosity"
    ),
    "Canopy density / openness" = c(
      `Gap fraction profile (GFP)`           = "Cutoff05.GFP",
      `Direct gap fraction (DGF)`            = "Cutoff05.DGF",
      `LAI`                             = "LAI.mean",
      `LAI S.D`                             = "LAI.sd",
      `NDVI`                            = "NDVI.mean"
    )
  )
  E_data <- map_dfr(names(driver_groups), function(grp) {
    vars <- driver_groups[[grp]]
    imap_dfr(vars, function(col, label) {
      map_dfr(c("MBR", "AE", "WP"), function(ap) {
        sub <- co %>% filter(Approach == ap) %>%
          select(CCC, all_of(col)) %>% drop_na()
        r <- if (nrow(sub) > 10) suppressWarnings(cor(sub$CCC, sub[[col]])) else NA_real_
        tibble(group = grp, label = label, Approach = ap, r = r)
      })
    })
  }) %>% mutate(label = factor(label, levels = unique(label)),
                Approach = factor(Approach, levels = c("MBR", "AE", "WP")))

  E <- ggplot(E_data, aes(x = Approach, y = fct_rev(label), fill = r)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%+.2f", r),
                  color = abs(r) > 0.30), size = 2.8, show.legend = FALSE) +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                         midpoint = 0, limits = c(-0.6, 0.6),
                         name = "r with CCC") +
    scale_color_manual(values = c(`TRUE` = "white", `FALSE` = "black"),
                       guide = "none") +
    labs(title = "E.",
         x = NULL, y = NULL) +
    base_theme + theme(axis.text.y = element_text(size = 8))

  # -------- Panel F per-site scatter ----------------------------
  F_data <- co %>%
    group_by(Site) %>%
    summarise(maxCCC = max(CCC, na.rm = TRUE),
              CHM    = first(CHM.mean),
              VAI    = first(Cutoff05.Grain01.VAI),
              GFP    = first(Cutoff05.GFP),
              .groups = "drop") %>%
    drop_na(CHM, VAI)

  `F` <- ggplot(F_data, aes(x = CHM, y = maxCCC, fill = VAI)) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", linewidth = 0.4) +
    geom_point(size = 3, shape = 21, color = "black", stroke = 0.4) +
    ggrepel::geom_text_repel(
      aes(label = Site),
      size = 2.5, color = "#222",
      max.overlaps = Inf,
      box.padding = 0.30, point.padding = 0.20,
      min.segment.length = 0.0,
      segment.color = "#888", segment.size = 0.25,
      seed = 42
    ) +
    scale_fill_distiller(palette = "YlGn", direction = 1, limits = c(0, 2.5),
                         name = "VAI") +
    labs(title = "F.",
         x = "Mean canopy height (m)",
         y = "Best CCC at site") +
    coord_cartesian(ylim = c(-0.05, 1.10)) +
    base_theme

  # -------- Synthesis text (gas-aware) --------------------------
  pct_aa <- 100 * mean(co$CCC[co$Canopy_L1 == "AA"] >= 0.5, na.rm = TRUE)
  pct_aw <- 100 * mean(co$CCC[co$Canopy_L1 == "AW"] >= 0.5, na.rm = TRUE)
  mbr_aa <- mean(co$CCC[co$Canopy_L1 == "AA" & co$Approach == "MBR"], na.rm = TRUE)
  mbr_aw <- mean(co$CCC[co$Canopy_L1 == "AW" & co$Approach == "MBR"], na.rm = TRUE)
  ae_aw  <- mean(co$CCC[co$Canopy_L1 == "AW" & co$Approach == "AE"],  na.rm = TRUE)
  wp_aw  <- mean(co$CCC[co$Canopy_L1 == "AW" & co$Approach == "WP"],  na.rm = TRUE)

  if (gas == "CO2") {
    rank_line <- sprintf(
      "Method ranking -- MBR is most rewarded by favorable geometry (mean CCC = %.2f in AA pairs); AE & WP plateau lower (mean ~ 0.10-0.12) regardless of canopy.",
      mbr_aa)
  } else {
    rank_line <- sprintf(
      "Method ranking -- MBR peaks for AA pairs (mean CCC = %.2f); for AW, the canopy itself is the H2O source so AE (mean = %.2f) and WP (mean = %.2f) overtake MBR (mean = %.2f). AW often matches AA for water.",
      mbr_aa, ae_aw, wp_aw, mbr_aw)
  }

  synth_text <- paste0(
    sprintf("Synthesis (%s; AA + AW pairs only; WW excluded -- almost never reaches CCC >= 0.5).  Strong FG-EC agreement requires three things acting together:\n", gas),
    sprintf("(1) Sensor geometry -- both heights above the canopy (AA pairs, z_A - h_c >= 5 m). AA reaches CCC >= 0.5 in %.0f%% of cases; AW in %.0f%%.\n", pct_aa, pct_aw),
    "(2) Wind/gas profile space -- log-profile theory needs clearance above the roughness sublayer; wider sensor separation amplifies the gradient signal (esp. MBR).\n",
    "(3) Canopy structure -- short, open, vertically simple canopies (low VAI/FHD, high gap fraction) give the cleanest gradients; tall, closed canopies suppress agreement.\n",
    rank_line
  )

  title_top <- sprintf(
    "What makes Gradient Flux agree with Eddy Covariance? -- %s, NEON sites (AA + AW pairs only; WW excluded)",
    if (gas == "CO2") "CO2" else "H2O"
  )

  design <- (A|B ) /
            (C | D) /
             (E|F)

  design
}


# ---------------------------------------------------------------
# Render and save for both gases
# ---------------------------------------------------------------
for (g in c("CO2", "H2O")) {
  
  fig <- build_ccc_drivers_figure(SITES_One2One_canopy_model, gas = g)
  print(fig)
  out_png <- file.path(fig_dir, sprintf("CCC_drivers_explained_%s.png", g))
  ggsave(out_png, fig, width = 13, height = 9, dpi = 220, bg = "white")
  message("Saved: ", out_png)
}

