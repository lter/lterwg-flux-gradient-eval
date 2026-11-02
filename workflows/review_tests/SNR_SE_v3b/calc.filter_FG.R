# Although we trach cross gradient flags we dont filter by it!

# Signal-to-noise (SNR) of the concentration difference ------------------------
# snr.type = "SE" (default): SNR = |dConc| / SE of the difference, where
#   SE = sqrt(vari_A/numSamp_A + vari_B/numSamp_B) (vari = variance of the samples at each level).
#   This is a detection test for the mean difference and does not depend on flux magnitude.
#   MBR also tests the tracer difference with its own SE. Because the MBR flux scales with the
#   ratio of the target and tracer differences, the noise in both adds up and MBR needs a higher
#   SNR than AE/WP (where K does not depend on the gradient). Thresholds are set by approach:
snr.min.default <- c(MBR = 30, AE = 3, WP = 3)
#   Chosen on CO2 and H2O against EC (all NEON sites, Aug 2021 - Aug 2026; sweep of MBR 3-50):
#   AE/WP SE >= 3 keeps 64-86% of windows and removes the selection toward large/calm-period
#   fluxes; MBR SE >= 30 keeps sampling-height-pair CCC at the level of the previous rule
#   (lower MBR thresholds make K noisy and degrade agreement with EC).
#   Analysis: /Volumes/MaloneLab/Research/FluxGradient/METHANE/SNR_SE_test/full_2021_2026/
# snr.type = "SD": previous (published) rule, SNR = |dConc| / dConc_sd, where dConc_sd is the
#   spread of the samples (sqrt(vari_A + vari_B)), one threshold for all approaches (3).

# Pick the threshold for an approach: a named vector (c(MBR = 30, AE = 3, WP = 3)) or one number.
snr_threshold <- function(dConcSNR.min, approach) {
  if (length(dConcSNR.min) > 1) {
    if (is.null(names(dConcSNR.min)) || !approach %in% names(dConcSNR.min))
      stop("dConcSNR.min must be one number or a vector named by approach (MBR, AE, WP)")
    return(unname(dConcSNR.min[[approach]]))
  }
  dConcSNR.min
}

# Add the SE of the concentration difference (dConc_se) and, for MBR, of the tracer difference
# (dConc.tracer_se). The tracer SE comes from the tracer gas's own row for the same sampling
# window and height pair (timeEnd_A, timeEnd_B, dLevelsAminusB) in the same data frame, checked
# against dConc.tracer. Where no row matches, the tracer SD is scaled by the SE/SD ratio of the
# target row (same window, same sample counts). Run on the full MBR data frame (all gases) before
# subsetting by gas, so that the tracer rows are present.
add_snr_se <- function(df) {
  df <- as.data.frame(df)
  df$dConc_se <- sqrt(df$vari_A / df$numSamp_A + df$vari_B / df$numSamp_B)
  if (all(c("tracer", "dConc.tracer", "dConc.tracer_sd") %in% names(df)) && !"dConc.tracer_se" %in% names(df)) {
    key <- intersect(c("timeEnd_A", "timeEnd_B", "dLevelsAminusB"), names(df))
    lk <- df[, c(key, "gas", "dConc", "dConc_se")]
    names(lk)[names(lk) == "gas"] <- "tracer"
    names(lk)[names(lk) == "dConc"] <- "dConc.tracer.lookup"
    names(lk)[names(lk) == "dConc_se"] <- "dConc.tracer_se"
    lk <- lk[!duplicated(lk[, c(key, "tracer")]), ]
    df <- dplyr::left_join(df, lk, by = c(key, "tracer"))
    ok <- !is.na(df$dConc.tracer.lookup) &
      abs(df$dConc.tracer.lookup - df$dConc.tracer) <= 1e-6 * pmax(1, abs(df$dConc.tracer))
    df$dConc.tracer_se[!ok] <- NA
    fill <- is.na(df$dConc.tracer_se)
    df$dConc.tracer_se[fill] <- (df$dConc.tracer_sd * df$dConc_se / sqrt(df$vari_A + df$vari_B))[fill]
    df$dConc.tracer.lookup <- NULL
    attr(df, "tracer_se_exact") <- mean(ok)
  }
  df
}

# Add dConcSNR (target) and dConcTSNR (tracer; MBR only) for snr.type "SE" or "SD".
# For AE/WP the tracer test is switched off as before (dConc.tracer = threshold, dConc.tracer_sd = 1).
calc_snr <- function(df, approach, thr, snr.type = "SE") {
  if (approach != "MBR") {
    df$dConc.tracer <- thr
    df$dConc.tracer_sd <- 1
    if (snr.type == "SE") df$dConc.tracer_se <- 1
  }
  if (snr.type == "SE") {
    if (!"dConc_se" %in% names(df) || !"dConc.tracer_se" %in% names(df)) df <- add_snr_se(df)
    df$dConcSNR <- abs(df$dConc) / df$dConc_se
    df$dConcTSNR <- abs(df$dConc.tracer) / df$dConc.tracer_se
  } else if (snr.type == "SD") {
    df$dConcSNR <- abs(df$dConc) / df$dConc_sd
    df$dConcTSNR <- abs(df$dConc.tracer) / df$dConc.tracer_sd
  } else stop("snr.type must be 'SE' or 'SD'")
  df
}

filter_fluxes <- function( df,
                           dConcSNR.min = snr.min.default,
                           approach,
                           snr.type = "SE"){
  df <- as.data.frame(df)
  thr <- snr_threshold(dConcSNR.min, approach)
  if (snr.type == "SE" && approach == "MBR" && length(dConcSNR.min) == 1 && thr < 10)
    warning("SE-based SNR threshold of ", thr, " applied to MBR; the validated MBR threshold is ", snr.min.default[["MBR"]])

  H.filter.list = df$dLevelsAminusB %>% unique

  df.new <- calc_snr(df, approach, thr, snr.type) %>%
    filter(dLevelsAminusB  %in% H.filter.list,
           TowerPosition_A != TowerPosition_B,
           dConcSNR >= thr,
           dConcTSNR >= thr,
           ustar_interp >= ustar_threshold)
  return(df.new)
}


Apply.filter <- function( site.tibble,
                          dConcSNR.min = snr.min.default,
                          approach,
                          snr.type = "SE"){

  sites <- names(site.tibble)
  site.tibble_FILTER <- list()

  for ( i in sites){
    print(i)
    filtered.data <- filter_fluxes( df = site.tibble[i],
                                    dConcSNR.min = dConcSNR.min,
                                    approach = approach,
                                    snr.type = snr.type)
    
    filtered.data.new <- filtered.data %>% mutate(time='timeEnd_A', dLevelsAminusB.colname = 'dLevelsAminusB' )
    site.tibble_FILTER[i] <- list(filtered.data.new )
    
  }
  
  return(site.tibble_FILTER )  
}

# Filter Report:
filter_report <- function( df,
                           dConcSNR.min = snr.min.default,
                           approach,
                           snr.type = "SE"){
  df <- as.data.frame(df)
  thr <- snr_threshold(dConcSNR.min, approach)

  H.filter.list = df$dLevelsAminusB %>% unique

  df.new <- calc_snr(df, approach, thr, snr.type) %>% mutate(diff.flux = abs(FG_mean - FC_turb_interp)) %>%
    filter(dLevelsAminusB  %in% H.filter.list,
           TowerPosition_A != TowerPosition_B) %>%
    mutate(
      flag.dConcSNR = case_when( dConcSNR >= thr ~ 0, dConcSNR < thr ~ 1, is.na(dConcSNR ) ~ 0),
      flag.dConcTSNR = case_when( dConcTSNR >= thr ~ 0, dConcTSNR < thr ~ 1, is.na(dConcTSNR ) ~ 0),
           flag.ustar_interp = case_when( ustar_interp >=  ustar_threshold ~ 0, ustar_interp <  ustar_threshold ~ 1, is.na(ustar_interp) ~ 0),   
      interaction.ALL= flag.ustar_interp + flag.dConcTSNR + flag.dConcSNR,
      
           flag.interaction.ALL = case_when( interaction.ALL > 0 ~1 ) )  %>% 
    reframe( .by = dLevelsAminusB,
             total =  length(timeEnd_A), 
             flag.ustar_interp = sum(flag.ustar_interp, na.rm=T) /total *100,
             flag.dConcTSNR =  sum(flag.dConcTSNR, na.rm=T) /total *100,
             flag.dConcSNR =  sum(flag.dConcSNR, na.rm=T) /total *100,
             flag.cross_grad =  sum(cross_grad_flag, na.rm=T) /total *100,
             flag.interaction.ALL = sum(flag.interaction.ALL, na.rm=T)/total *100)
  
  return(df.new)
}

# Stability filter report:
filter_report_stability <- function( df,
                           dConcSNR.min = snr.min.default,
                           approach,
                           snr.type = "SE"){
  df <- as.data.frame(df)
  thr <- snr_threshold(dConcSNR.min, approach)

  H.filter.list = df$dLevelsAminusB %>% unique

  df.new <- calc_snr(df, approach, thr, snr.type) %>% mutate(diff.flux = abs(FG_mean - FC_turb_interp)) %>%
    filter(dLevelsAminusB  %in% H.filter.list,
           TowerPosition_A != TowerPosition_B) %>%
    mutate(
      flag.dConcSNR = case_when( dConcSNR >= thr ~ 0, dConcSNR < thr ~ 1, is.na(dConcSNR ) ~ 0),
      flag.dConcTSNR = case_when( dConcTSNR >= thr ~ 0, dConcTSNR < thr ~ 1, is.na(dConcTSNR ) ~ 0),
      flag.ustar_interp = case_when( ustar_interp >=  ustar_threshold ~ 0, ustar_interp <  ustar_threshold ~ 1, is.na(ustar_interp) ~ 0),
      interaction.ALL= flag.ustar_interp + flag.dConcTSNR + flag.dConcSNR  , 
      
      
      flag.interaction.ALL = case_when( interaction.ALL > 0 ~1 ) )  %>% 
    reframe( .by = c(dLevelsAminusB,Stability_500),
             total =  length(timeEnd_A), 
             flag.ustar_interp = sum(flag.ustar_interp, na.rm=T) /total *100,
             flag.dConcTSNR =  sum(flag.dConcTSNR, na.rm=T) /total *100,
             flag.dConcSNR =  sum(flag.dConcSNR, na.rm=T) /total *100,
             flag.cross_grad =  sum(cross_grad_flag, na.rm=T)/total *100,
             flag.interaction.ALL = sum(flag.interaction.ALL, na.rm=T)/total *100)
  
  return(df.new)
}


Generate.filter.report <- function( site.tibble,
                                    dConcSNR.min = snr.min.default,
                                    approach,
                                    snr.type = "SE"){

  sites <- names(site.tibble)

  REPORT <- data.frame()

  for ( i in sites){
    print(i)

    filtered.data <- filter_report ( df = site.tibble[i],
                                     dConcSNR.min =  dConcSNR.min,
                                     approach = approach,
                                     snr.type = snr.type) %>% mutate(site = i)
    
    REPORT <-  REPORT %>% rbind(filtered.data )
    
  }
  
  
  return(REPORT  )  
}


# Old Version of functions
filter_fluxes_old <- function( df, 
                           flux.limit, 
                           ustar.filter, 
                           diff.limit, 
                           FG_sd.limit,
                           dConcSNR.min,
                           rmvCrossGrad, # Set to TRUE to remove data where cross_grad_flag == 1. Applies only to AE and WP methods
                           rmvEddyOutlier, # Set to TRUE to remove data where Eddy_outlier == 1. Applies only to AE and WP methods
                           approach){
  df <- as.data.frame(df) 
  #names(df) <- substring( names(df), 6)
  
  H.filter.list = df$dLevelsAminusB %>% unique
  
  if( approach =="MBR"){
    
    df$Eddy_outlier <- 0 
    df$cross_grad_flag <- 0 
  } else {
    df$dConc.tracer <- dConcSNR.min
    df$dConc.tracer_sd <- 1
  }
  if(rmvCrossGrad == FALSE){
    df$cross_grad_flag <- 0
  }
  if(rmvEddyOutlier == FALSE){
    df$Eddy_outlier <- 0
  }
  
  
  
  df.new <- df %>% mutate(diff.flux = abs(FG_mean - FC_turb_interp),
                          dConcSNR = abs(dConc)/dConc_sd,
                          dConcTSNR = abs(dConc.tracer)/dConc.tracer_sd) %>%  
    filter(dLevelsAminusB  %in% H.filter.list,
           TowerPosition_A != TowerPosition_B,
           dConcSNR <= dConcSNR.min,
           dConcTSNR <= dConcSNR.min,
           #dConc_pvalue <= 0.1, 
           #FG_mean < flux.limit & FG_mean > -flux.limit,
           ustar_interp >=  ustar_threshold )
  #Stability_100 == 'unstable',
  #Eddy_outlier != 1,
  #cross_grad_flag != 1,
  #diff.flux <  diff.limit,
  #abs(FG_sd) < FG_sd.limit,)
  return(df.new)
}

Apply.filter <- function( site.tibble,
                          flux.limit, 
                          ustar.filter, 
                          FG_sd.limit,
                          diff.limit,
                          rmvCrossGrad, # Set to TRUE to remove data where cross gradient flag == 1. Applies only to AE and WP methods
                          rmvEddyOutlier, # Set to TRUE to remove data where Eddy_outlier == 1. Applies only to AE and WP methods
                          dConcSNR.min, approach){
  
  sites <- names(site.tibble)
  site.tibble_FILTER <- list()
  
  for ( i in sites){
    print(i)
    filtered.data <- filter_fluxes( df = site.tibble[i],
                                    flux.limit = flux.limit, 
                                    ustar.filter = ustar.filter, 
                                    FG_sd.limit = FG_sd.limit,
                                    diff.limit = diff.limit,
                                    dConcSNR.min = dConcSNR.min,
                                    rmvCrossGrad = rmvCrossGrad, 
                                    rmvEddyOutlier = rmvEddyOutlier, 
                                    approach = approach) 
    
    filtered.data.new <- filtered.data %>% mutate(time='timeEnd_A', dLevelsAminusB.colname = 'dLevelsAminusB' )
    site.tibble_FILTER[i] <- list(filtered.data.new )
    
  }
  
  return(site.tibble_FILTER )  
}

# Filter Report:

filter_report_old <- function( df, 
                           dConcSNR.min, 
                           rmvCrossGrad, 
                           rmvEddyOutlier,
                           approach){
  df <- as.data.frame(df) 
  
  #names(df) <- substring( names(df), 6)
  
  H.filter.list = df$dLevelsAminusB %>% unique
  
  if( approach =="MBR"){
    df$Eddy_outlier <- 0 
    df$cross_grad_flag <- 0 
  } else {
    df$dConc.tracer <- dConcSNR.min
    df$dConc.tracer_sd <- 1
  }
  if(rmvCrossGrad == FALSE){
    df$cross_grad_flag <- 0
  }
  if(rmvEddyOutlier == FALSE){
    df$Eddy_outlier <- 0
  }
  
  df.new <- df %>% mutate(diff.flux = abs(FG_mean - FC_turb_interp),
                          dConcSNR = abs(dConc)/dConc_sd,
                          dConcTSNR = abs(dConc.tracer)/dConc.tracer_sd) %>%  
    filter(dLevelsAminusB  %in% H.filter.list,
           TowerPosition_A != TowerPosition_B) %>% 
    mutate(
      #flag.dConc_pvalue = case_when( dConc_pvalue <= 0.1 ~ 0, dConc_pvalue > 0.1 ~ 1, is.na(dConc_pvalue ) ~ 0),
      # flag.FG_mean = case_when(abs(FG_mean) <= flux.limit ~ 0 , abs(FG_mean) > flux.limit ~ 1, is.na(FG_mean) ~ 0),
      flag.dConcSNR = case_when( dConcSNR <= dConcSNR.min ~ 0, dConcSNR > dConcSNR.min ~ 1, is.na(dConcSNR ) ~ 0),
      flag.dConcTSNR = case_when( dConcTSNR <= dConcSNR.min ~ 0, dConcTSNR > dConcSNR.min ~ 1, is.na(dConcTSNR ) ~ 0),
      flag.ustar_interp = case_when( ustar_interp >=  ustar_threshold ~ 0, ustar_interp <  ustar_threshold ~ 1, is.na(ustar_interp) ~ 0), 
      #flag.cross_grad_flag = case_when(cross_grad_flag != 1 ~ 0, cross_grad_flag == 1 ~ 1, is.na(cross_grad_flag) ~ 0),
      # flag.Eddy_outlier = case_when(Eddy_outlier !=1 ~ 0, Eddy_outlier ==1 ~ 1, is.na( Eddy_outlier) ~ 0),
      #flag.FG_sd = case_when( abs(FG_sd) <= FG_sd.limit ~ 0 ,abs(FG_sd) > FG_sd.limit ~ 1, is.na( FG_sd) ~ 0),
      #flag.dConc = case_when( (dConcSNR.filter >= dConcSNR.min) & (dConcTSNR.filter >= dConcSNR.min) ~ 0 , (dConcSNR.filter < dConcSNR.min) | (dConcTSNR.filter < dConcSNR.min) ~ 1, is.na( FG_sd) ~ 0),
      
      # interaction.ALL= flag.dConc_pvalue + flag.FG_mean + flag.ustar_interp +flag.cross_grad_flag + flag.Eddy_outlier+flag.FG_sd + flag.dConc  ,
      interaction.ALL= flag.ustar_interp + flag.dConcTSNR + flag.dConcSNR ,
      
      flag.interaction.ALL = case_when( interaction.ALL > 0 ~1 ) )  %>% 
    reframe( .by = dLevelsAminusB,
             total =  length(timeEnd_A), 
             #flag.dConc_pvalue = sum(flag.dConc_pvalue, na.rm=T)/ total *100,
             #flag.FG_mean  = sum(flag.FG_mean, na.rm=T)/ total *100,
             flag.ustar_interp = sum(flag.ustar_interp, na.rm=T) /total *100,
             flag.dConcTSNR =  sum(flag.dConcTSNR, na.rm=T) /total *100,
             flag.dConcSNR =  sum(flag.dConcSNR, na.rm=T) /total *100,
             #flag.cross_grad_flag  = sum(flag.cross_grad_flag, na.rm=T) /total *100,
             #flag.Eddy_outlier  = sum( flag.Eddy_outlier, na.rm=T)/ total *100,
             #flag.FG_sd  = sum(flag.FG_sd, na.rm=T)/ total *100,
             #flag.dConc  = sum(flag.dConc, na.rm=T) /total *100,
             flag.interaction.ALL = sum(flag.interaction.ALL, na.rm=T)/total *100)
  
  return(df.new)
}

Generate.filter.report_old <- function( site.tibble,
                                    flux.limit, 
                                    ustar.filter, 
                                    FG_sd.limit,
                                    diff.limit,
                                    dConcSNR.min,
                                    rmvCrossGrad, 
                                    rmvEddyOutlier,
                                    approach){
  
  sites <- names(site.tibble)
  
  REPORT <- data.frame()
  
  for ( i in sites){
    print(i)
    
    filtered.data <- filter_report ( df = site.tibble[i],
                                     flux.limit = flux.limit, 
                                     ustar.filter = ustar.filter, 
                                     FG_sd.limit = FG_sd.limit,
                                     diff.limit = diff.limit,
                                     dConcSNR.min =  dConcSNR.min,
                                     rmvCrossGrad = rmvCrossGrad, 
                                     rmvEddyOutlier = rmvEddyOutlier,
                                     approach = approach) %>% mutate(site = i)
    
    REPORT <-  REPORT %>% rbind(filtered.data )
    
  }
  
  
  return(REPORT  )  
}


# Gradient-flux spike removal ------------------------------------------------
# Applied after filter_fluxes(). Drops gradient fluxes (FG_mean) farther than
# k * MAD from the median, computed within gas x sampling-height pair x
# day/night x season strata. Stratifying matters: pooled across the diel cycle,
# the MAD is set by small night-time fluxes and real midday H2O peaks get removed
# (~25% of midday MBR H2O). Uses the GF only (no EC information).
# With k = 6, removes ~2-3% of filtered GF by day and by night (NEON 2021-2026).
# Strata with fewer than min.n values are left untouched.
despike_fluxes <- function(df, k = 6, flux.col = "FG_mean",
                           time.col = "timeEndA.local",
                           group.cols = c("gas", "dLevelsAminusB"),
                           day.hours = 7:18, by.season = TRUE, min.n = 20) {
  df <- as.data.frame(df)
  if (nrow(df) == 0) return(df)
  hr <- as.numeric(format(df[[time.col]], "%H"))
  mo <- as.numeric(format(df[[time.col]], "%m"))
  df$.daynight <- ifelse(hr %in% day.hours, "day", "night")
  df$.season <- if (by.season) {
    dplyr::case_when(mo %in% c(12, 1, 2) ~ "Winter", mo %in% 3:5 ~ "Spring",
                     mo %in% 6:8 ~ "Summer", TRUE ~ "Autumn")
  } else "all"
  df %>%
    group_by(across(all_of(c(group.cols, ".daynight", ".season")))) %>%
    mutate(.med = median(.data[[flux.col]], na.rm = TRUE),
           .mad = mad(.data[[flux.col]], na.rm = TRUE),
           .n = sum(!is.na(.data[[flux.col]]))) %>%
    filter(is.na(.data[[flux.col]]) | .n < min.n |
             abs(.data[[flux.col]] - .med) <= k * pmax(.mad, 1e-6)) %>%
    ungroup() %>%
    select(-.med, -.mad, -.n, -.daynight, -.season) %>%
    as.data.frame()
}
