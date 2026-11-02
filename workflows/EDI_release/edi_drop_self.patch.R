  # Drop same-inlet rows: dLevelsAminusB "n_n" (e.g. 4_4) pairs two consecutive samples of
  # one inlet, so it is not a gradient (AE/WP give no flux; MBR gives a non-physical flux).
  # These rows never enter the evaluation (not in the sampling-height pair table).
  drop.self <- function(d) {
    lv <- strsplit(as.character(d$dLevelsAminusB), "_", fixed = TRUE)
    self <- vapply(lv, function(v) length(v) == 2 && v[1] == v[2], TRUE)
    message(site, ": dropping ", sum(self), " same-inlet rows")
    d[!self, , drop = FALSE]
  }
  MBR_9min.df.final <- drop.self(MBR_9min.df.final)
  AE_9min.df.final  <- drop.self(AE_9min.df.final)
  WP_9min.df.final  <- drop.self(WP_9min.df.final)

