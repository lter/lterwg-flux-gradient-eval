# ---------------------------------------------------------------
# flow.CCC.results.summary.R
#
# Single summary figure for the Flux-Gradient evaluation paper,
# COMPUTED DIRECTLY FROM SITES_One2One_canopy_model.
#
# Panels A-E and G are derived from the data; the seasonal panel (F)
# is taken from the manuscript text because the underlying
# half-hourly flux records are not stored in the One2One file.
#
# Output: CCC_results_summary.{png,pdf} in the FluxGradient root.
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(patchwork)
  library(grid)
  library(scales)
})

root_dir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
rdata    <- file.path(root_dir, "SITES_One2One_AA_AW.Rdata")
fig_dir <- '/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/'
# Name of the output files:
out_png  <- file.path(fig_dir, "CCC_results_summary.png")
out_pdf  <- file.path(fig_dir, "CCC_results_summary.pdf")


load(rdata)
stopifnot(exists("SITES_One2One"))

SITES_One2One %>% summary
# --- Palettes ---------------------------------------------------
gas_colors      <- c(CO2 = "#2e7d32", H2O = "#1f77b4")
approach_colors <- c(MBR = "#B8860B", AE  = "#2E8B57", WP  = "#6A0DAD")
geom_colors     <- c(AA  = "violetred2", AW  = "salmon1")  # eval repo convention

base_theme <- theme_bw(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        plot.title  = element_text(face = "bold", size = 11, hjust = 0),
        legend.position = "right")

# --- Prep data --------------------------------------------------
df <- SITES_One2One%>%
  as_tibble() %>%
  filter(Canopy_L1 %in% c("AA", "AW")) %>%
  mutate(Approach = factor(Approach, levels = c("MBR", "AE", "WP")),
         Canopy_L1 = factor(Canopy_L1, levels = c("AA", "AW")),
         Good = as.integer(CCC >= 0.5))

total_sites <- df %>% pull(Site) %>% n_distinct()

compute_stats <- function(d, gas_name) {
  sub <- d %>% filter(gas == gas_name)
  rel <- sub %>% filter(Good == 1)
  sites_with <- sub %>% group_by(Site) %>% summarise(n_good = sum(Good), .groups = "drop")
  rsites     <- sites_with %>% filter(n_good >= 1) %>% pull(n_good)

  list(
    n_pairs           = nrow(sub),
    n_good            = sum(sub$Good),
    pct_good          = 100 * mean(sub$Good == 1),
    n_sites_total     = n_distinct(sub$Site),
    n_sites_reliable  = sum(sites_with$n_good >= 1),
    rsites_mean       = mean(rsites),
    rsites_sd         = sd(rsites),
    method_pct        = 100 * prop.table(table(rel$Approach)),
    geom_pct          = 100 * prop.table(table(rel$Canopy_L1)),
    by_geom = rel %>%
      group_by(Canopy_L1) %>%
      summarise(CCC = mean(CCC, na.rm = TRUE),
                R2  = mean(R2,  na.rm = TRUE),
                n   = n(),
                .groups = "drop") %>%
      column_to_rownames("Canopy_L1"),
    method_size = sub %>%
      group_by(Site, Approach) %>%
      summarise(best = max(count, na.rm = TRUE), .groups = "drop_last") %>%
      group_by(Approach) %>%
      summarise(mean = mean(best), sd = sd(best), .groups = "drop") %>%
      column_to_rownames("Approach")
  )
}

stats <- list(CO2 = compute_stats(df, "CO2"),
              H2O = compute_stats(df, "H2O"))

co2_sites_rel <- df %>% filter(gas == "CO2", Good == 1) %>% pull(Site) %>% unique()
h2o_sites_rel <- df %>% filter(gas == "H2O", Good == 1) %>% pull(Site) %>% unique()
both_sites    <- intersect(co2_sites_rel, h2o_sites_rel)

# ---------------------------------------------------------------
# Panel A -- Site-coverage Venn
# ---------------------------------------------------------------
make_panelA <- function() {
  co2 <- stats$CO2$n_sites_reliable
  h2o <- stats$H2O$n_sites_reliable
  both <- length(both_sites)
  co2_only <- co2 - both
  h2o_only <- h2o - both
  neither  <- total_sites - co2_only - h2o_only - both

  theta <- seq(0, 2 * pi, length.out = 100)
  c1 <- tibble(x = 1.2 + 1.4 * cos(theta), y = 1.4 * sin(theta))
  c2 <- tibble(x = 2.6 + 1.4 * cos(theta), y = 1.4 * sin(theta))

  ggplot() +
    geom_polygon(data = c1, aes(x, y), fill = gas_colors["CO2"], alpha = 0.4) +
    geom_polygon(data = c2, aes(x, y), fill = gas_colors["H2O"], alpha = 0.4) +
    annotate("text", x = 0.7, y = 0,
             label =  co2_only,
             color = "#1b4d1f", fontface = "bold", size = 3.6) +
    annotate("text", x = 3.1, y = 0,
             label =  h2o_only,
             color = "#0d3a6b", fontface = "bold", size = 3.6) +
    annotate("text", x = 1.9, y = 0, label = paste0("both\n", both),
             color = "black", fontface = "bold", size = 4) +
    annotate("text", x = 1.2, y = 1.85, label = "CO[2]", parse = TRUE,
             color = gas_colors["CO2"], fontface = "bold", size = 4.5) +
    annotate("text", x = 2.6, y = 1.85, label = "H[2]*O", parse = TRUE,
             color = gas_colors["H2O"], fontface = "bold", size = 4.5) +
    coord_fixed(xlim = c(-1, 5), ylim = c(-2.4, 2.4)) +
    labs(title = "A.", x = NULL, y = NULL) +
    theme_void() +
    theme(plot.title = element_text(face = "bold", size = 11, hjust = 0))
}

# ---------------------------------------------------------------
# Panel B -- % of pairs reliable + per-site mean
# ---------------------------------------------------------------
make_panelB <- function() {
  d <- tibble(
    gas = factor(c("CO2", "H2O"), levels = c("CO2", "H2O")),
    pct = c(stats$CO2$pct_good, stats$H2O$pct_good)
  ) %>% mutate(label = sprintf("%.1f%% of all pairs", pct))
  bg <- tibble(gas = d$gas, pct = 100)

  subtitle <- sprintf(
    "Mean reliable height pairs per site:  CO2 = %.1f +/- %.1f   |   H2O = %.1f +/- %.1f",
    stats$CO2$rsites_mean, stats$CO2$rsites_sd,
    stats$H2O$rsites_mean, stats$H2O$rsites_sd)

  ggplot(d, aes(y = fct_rev(gas))) +
    geom_col(data = bg, aes(x = pct), fill = "#eeeeee", color = "#bbb",
             width = 0.55) +
    geom_col(aes(x = pct, fill = gas), alpha = 0.85, width = 0.55) +
    geom_text(aes(x = pct + 2, label = label, color = gas),
              hjust = 0, fontface = "bold", size = 3.5,
              show.legend = FALSE) +
    scale_y_discrete(labels = c(CO2 = expression(CO[2]),
                                H2O = expression(H[2]*O))) +
    scale_fill_manual(values = gas_colors, guide = "none") +
    scale_color_manual(values = gas_colors, guide = "none") +
    coord_cartesian(xlim = c(0, 110)) +
    labs(title = "B.",
         #subtitle = subtitle,
         x = "% of sampling pairs with CCC ≥ 0.5",
         y = NULL) +
    base_theme +
    theme(plot.subtitle = element_text(size = 8.5, color = "#444"),
          axis.text.y = element_text(face = "bold", size = 11,
                                     color = c(gas_colors["H2O"], gas_colors["CO2"])))
}

# ---------------------------------------------------------------
# Panel C -- Method breakdown of reliable pairs
# ---------------------------------------------------------------
make_panelC <- function() {
  d <- bind_rows(
    tibble(gas = "CO2", method = names(stats$CO2$method_pct),
           pct = as.numeric(stats$CO2$method_pct)),
    tibble(gas = "H2O", method = names(stats$H2O$method_pct),
           pct = as.numeric(stats$H2O$method_pct))
  ) %>% mutate(gas = factor(gas, levels = c("CO2", "H2O")),
               method = factor(method, levels = c("MBR", "AE", "WP")))

  ggplot(d, aes(y = fct_rev(gas), x = pct, fill = method)) +
    geom_col(position = "stack", width = 0.55, alpha = 0.85,
             color = "white", linewidth = 0.6) +
    geom_text(aes(label = ifelse(pct >= 6,
                                 paste0(method, "\n", round(pct), "%"), "")),
              position = position_stack(vjust = 0.5),
              color = "white", fontface = "bold", size = 3) +
    scale_y_discrete(labels = c(CO2 = expression(CO[2]),
                                H2O = expression(H[2]*O))) +
    scale_fill_manual(values = approach_colors) +
    coord_cartesian(xlim = c(0, 100)) +
    labs(title = "C.",
         x = "% of sampling pairs with CCC ≥ 0.5", y = NULL, fill = NULL) +
    base_theme +
    theme(legend.position = "none",
          axis.text.y = element_text(face = "bold", size = 11,
                                     color = c(gas_colors["H2O"], gas_colors["CO2"])))
}

# ---------------------------------------------------------------
# Panel D -- Canopy geometry of reliable pairs
# ---------------------------------------------------------------
make_panelD <- function() {
  d <- bind_rows(
    tibble(gas = "CO2", geom = names(stats$CO2$geom_pct),
           pct = as.numeric(stats$CO2$geom_pct)),
    tibble(gas = "H2O", geom = names(stats$H2O$geom_pct),
           pct = as.numeric(stats$H2O$geom_pct))
  ) %>% mutate(gas = factor(gas, levels = c("CO2", "H2O")),
               geom = factor(geom, levels = c("AA", "AW")))

  ggplot(d, aes(y = fct_rev(gas), x = pct, fill = geom)) +
    geom_col(position = "stack", width = 0.55, alpha = 0.85,
             color = "white", linewidth = 0.6) +
    geom_text(aes(label = paste0(geom, "\n", round(pct), "%")),
              position = position_stack(vjust = 0.5),
              color = "white", fontface = "bold", size = 3.2) +
    scale_y_discrete(labels = c(CO2 = expression(CO[2]),
                                H2O = expression(H[2]*O))) +
    scale_fill_manual(values = geom_colors) +
    coord_cartesian(xlim = c(0, 100)) +
    labs(title = "D.",
         x = "% of sampling pairs with CCC ≥ 0.5", y = NULL) +
    base_theme +
    theme(legend.position = "none",
          axis.text.y = element_text(face = "bold", size = 11,
                                     color = c(gas_colors["H2O"], gas_colors["CO2"])))
}

# ---------------------------------------------------------------
# Panel E -- CCC distribution: gas (edge) x Canopy_L1 (fill)
# Boxplot of per-pair CCC for reliable pairs only.
# ---------------------------------------------------------------
make_panelE <- function() {
  d <- df %>%
    filter(Good == 1) %>%
    mutate(group = factor(paste0(gas, "_", Canopy_L1),
                          levels = c("CO2_AA", "CO2_AW", "H2O_AA", "H2O_AW")),
           gas = factor(gas, levels = c("CO2", "H2O")),
           Canopy_L1 = factor(Canopy_L1, levels = c("AA", "AW")))

  # R^2 + n annotations
  ann <- d %>% group_by(gas, Canopy_L1) %>%
    summarise(n = n(), R2 = mean(R2, na.rm = TRUE), .groups = "drop") %>%
    mutate(group = factor(paste0(gas, "_", Canopy_L1),
                          levels = c("CO2_AA", "CO2_AW", "H2O_AA", "H2O_AW")),
           label = sprintf("R^2==%.2f~~n==%d", R2, n))

  group_labels <- c(CO2_AA = "CO2 AA", CO2_AW = "CO2 AW",
                    H2O_AA = "H2O AA", H2O_AW = "H2O AW")

  ggplot(d, aes(x = group, y = CCC,
                fill = Canopy_L1, color = gas)) +
    geom_boxplot(outlier.shape = NA, width = 0.62, linewidth = 0.9,
                 alpha = 0.78) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red",
               linewidth = 0.4) +
   # geom_text(data = ann, aes(x = group, y = 1.0, label = label),
   #           parse = TRUE, inherit.aes = FALSE,
   #           vjust = 1, size = 2.8, color = "#333") +
    scale_fill_manual(values = geom_colors,
                      name = "Canopy_L1 (fill)") +
    scale_color_manual(values = gas_colors,
                       labels = c(CO2 = expression(CO[2]),
                                  H2O = expression(H[2]*O)),
                       name = "Gas (edge)") +
    scale_x_discrete(labels = group_labels) +
    coord_cartesian(ylim = c(0.4, 1.02)) +
    labs(title = "E.",
         x = NULL, y = "CCC") +
    base_theme +
    theme(legend.position = "none",
          legend.box = "vertical",
          legend.spacing.y = unit(0, "pt"),
          legend.margin = margin(0, 0, 0, 0),
          axis.text.x = element_text(
            face = "bold",
            color = c(gas_colors["CO2"], gas_colors["CO2"],
                      gas_colors["H2O"], gas_colors["H2O"]))) +
    guides(color = guide_legend(override.aes = list(fill = "white"),
                                ncol = 2),
           fill  = guide_legend(override.aes = list(color = "black"),
                                ncol = 2))
}

# ---------------------------------------------------------------
# Panel F -- Seasonal coverage (from manuscript text -- not in Rdata)
# ---------------------------------------------------------------
make_panelF <- function() {
  d <- tribble(
    ~gas,  ~season,  ~pct, ~sd,
    "CO2", "Winter", 17,    8,
    "CO2", "Spring", 24,    9,
    "CO2", "Summer", 34,   11,
    "CO2", "Autumn", 25,    7,
    "H2O", "Winter", 23,    9,
    "H2O", "Spring", 31,   12,
    "H2O", "Summer", 24,    7,
    "H2O", "Autumn", 21,    6
  ) %>% mutate(gas = factor(gas, levels = c("CO2", "H2O")),
               season = factor(season,
                               levels = c("Winter", "Spring", "Summer", "Autumn")))

  ggplot(d, aes(x = season, y = pct, fill = gas)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75,
             alpha = 0.85, color = "black", linewidth = 0.3) +
    geom_errorbar(aes(ymin = pct - sd, ymax = pct + sd),
                  position = position_dodge(width = 0.8),
                  width = 0.25, linewidth = 0.4) +
    geom_text(aes(label = paste0(pct, "%")),
              position = position_dodge(width = 0.8),
              vjust = -0.6, size = 3, color = "#222") +
    scale_fill_manual(values = gas_colors,
                      labels = c(CO2 = expression(CO[2]),
                                 H2O = expression(H[2]*O))) +
    coord_cartesian(ylim = c(0, 50)) +
    labs(title = "F.",
         caption = "* From filtered 1/2-hourly fluxes",
         x = NULL, y = "% of reliable flux records",
         fill = NULL) +
    base_theme +
    theme(legend.position = "none",
          legend.background = element_blank(),
          plot.caption = element_text(size = 7.5, color = "#666", hjust = 1))
}

source(fs::path(DirRepo.eval,'workflows/WF_Version1/flow.flux.counts.R'))
p_season <- ggplot(season.summary %>% na.omit, aes(x = percent.mean, y = gas, fill = season))  + geom_col() +  scale_fill_discrete_qualitative(palette = "Harmonic")+ geom_text(aes(label = sprintf("%.0f%%", percent.mean)), position = position_stack(vjust = 0.5), size = 3) +
  labs(
    y = NULL,
    x = "% sampling pairs",
    title = "F. ",
    fill = "Season"
  )  + theme_bw()  + theme(legend.position = "top") 


# ---------------------------------------------------------------
# Panel G -- Average dataset size by approach (best pair per site)
# ---------------------------------------------------------------
make_panelG <- function() {
  d <- bind_rows(
    tibble(gas = "CO2", method = c("MBR", "AE", "WP"),
           mean = stats$CO2$method_size[c("MBR", "AE", "WP"), "mean"],
           sd   = stats$CO2$method_size[c("MBR", "AE", "WP"), "sd"]),
    tibble(gas = "H2O", method = c("MBR", "AE", "WP"),
           mean = stats$H2O$method_size[c("MBR", "AE", "WP"), "mean"],
           sd   = stats$H2O$method_size[c("MBR", "AE", "WP"), "sd"])
  ) %>% mutate(gas = factor(gas, levels = c("CO2", "H2O")),
               method = factor(method, levels = c("MBR", "AE", "WP")))

  ymax <- max(d$mean + d$sd) + 1500

  ggplot(d, aes(x = method, y = mean, fill = gas)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7,
             alpha = 0.85, color = "black", linewidth = 0.3) +
    geom_errorbar(aes(ymin = pmax(mean - sd, 0), ymax = mean + sd),
                  position = position_dodge(width = 0.8),
                  width = 0.25, linewidth = 0.4) +
    geom_text(aes(label = scales::comma(round(mean))),
              position = position_dodge(width = 0.8),
              vjust = -5, size = 3, color = "#222") +
    scale_fill_manual(values = gas_colors,
                      labels = c(CO2 = expression(CO[2]),
                                 H2O = expression(H[2]*O)))  +
    coord_cartesian(ylim = c(0, ymax)) +
    labs(title = "F. ",
         x = NULL, y = "Mean half-hours per year", fill = NULL) +
    base_theme +
    theme(legend.position = 'none',
          legend.background = element_blank(),
          axis.text.x = element_text(face = "bold"))
}

plot.h <- plot.overlap.2 + labs(
  y = NULL,
  x = "% overlapping sampling pairs",
  title = "G. ",
  fill = "")  + 
  base_theme + theme(legend.position = 'top')

# ---------------------------------------------------------------
# Panel H -- Synthesis text
# ---------------------------------------------------------------
make_panelH <- function() {
  s <- stats
  pct_co2 <- s$CO2$pct_good; pct_h2o <- s$H2O$pct_good
  mbr_co2 <- s$CO2$method_pct["MBR"]; mbr_h2o <- s$H2O$method_pct["MBR"]
  wp_h2o  <- s$H2O$method_pct["WP"]
  aa_co2  <- s$CO2$geom_pct["AA"];  aa_h2o  <- s$H2O$geom_pct["AA"]
  aw_h2o  <- s$H2O$geom_pct["AW"]
  ccc_aa_co2 <- s$CO2$by_geom["AA", "CCC"]; ccc_aw_co2 <- s$CO2$by_geom["AW", "CCC"]
  ccc_aa_h2o <- s$H2O$by_geom["AA", "CCC"]; ccc_aw_h2o <- s$H2O$by_geom["AW", "CCC"]
  ae_h2o_size <- s$H2O$method_size["AE", "mean"]
  wp_h2o_size <- s$H2O$method_size["WP", "mean"]
  co2 <- s$CO2$n_sites_reliable; h2o <- s$H2O$n_sites_reliable
  both <- length(both_sites)

  txt <- paste0(
    "KEY FINDINGS  (computed from SITES_One2One_canopy_model)\n",
    "------------------------------------------------------\n",
    sprintf("* Coverage:  %d of %d sites yield reliable CO2 GFs; %d yield reliable H2O; %d sites yield both.\n",
            co2, total_sites, h2o, both),
    sprintf("* Reliability:  %.1f%% of all sampling-height pairs are reliable for CO2, vs %.1f%% for H2O.\n",
            pct_co2, pct_h2o),
    sprintf("* Method:  MBR dominates (%.0f%% of CO2, %.0f%% of H2O reliable pairs).\n",
            mbr_co2, mbr_h2o),
    sprintf("  WP fills more of the H2O reliability gap (%.0f%%) than the CO2 gap.\n", wp_h2o),
    sprintf("* Geometry:  AA dominates CO2 reliability (%.0f%%); H2O accepts cross-canopy AW\n",
            aa_co2),
    sprintf("  more readily (%.0f%% AW vs %.0f%% AA).\n", aw_h2o, aa_h2o),
    "* Concordance:  CCC for reliable pairs is similar across geometry --\n",
    sprintf("  CO2: AA = %.2f, AW = %.2f;  H2O: AA = %.2f, AW = %.2f.\n",
            ccc_aa_co2, ccc_aw_co2, ccc_aa_h2o, ccc_aw_h2o),
    "* Seasons (from filtered 1/2-hourly fluxes):  CO2 skewed to summer/autumn (winter\n",
    "  undersampled, 17%); H2O more even, with spring slightly elevated (31%).\n",
    sprintf("* Dataset size:  AE & WP produce the largest H2O datasets (~%s and %s 1/2-hrs/yr) --\n",
            scales::comma(round(ae_h2o_size)), scales::comma(round(wp_h2o_size))),
    "  they unlock cross-canopy fluxes in tall stands.\n",
    "* Take-away:  MBR is the workhorse across all ecosystems; AE and WP are\n",
    "  essential in forests."
  )

  ggplot() +
    #annotate("text", x = 0, y = 1, label = txt, hjust = 0, vjust = 1,
    #         size = 3.1, family = "mono") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    theme_void() +
    theme(plot.background = element_rect(fill = "#f5f5f5", color = "#bbb",
                                         linewidth = 0.4))
}

# ---------------------------------------------------------------
# Compose
# ---------------------------------------------------------------
A <- make_panelA(); B <- make_panelB(); C <- make_panelC(); D <- make_panelD()
E <- make_panelE() 
F_p <-  make_panelF() 
G <- make_panelG(); H <-plot.h

design <- (A | B | C) /
          (D | E) /
          (G |H)
  plot_layout(widths = c(1, 1, 1)) 

ggsave(out_png, design, width = 10, height = 7, dpi = 220, bg = "white")
ggsave(out_pdf, design, width = 10, height = 7, bg = "white")
message("Saved: ", out_png)
message("Saved: ", out_pdf)

