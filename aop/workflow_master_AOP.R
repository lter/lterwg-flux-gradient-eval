#!/usr/bin/env Rscript

# Master workflow for the AOP processing scripts.
#
# Usage examples:
#   Rscript aop/workflow_master_AOP.R --dry-run
#   Rscript aop/workflow_master_AOP.R
#   Rscript aop/workflow_master_AOP.R --skip-download
#   Rscript aop/workflow_master_AOP.R --start flow_AOP_FormatLayers --stop flow.canopy
#   Rscript aop/workflow_master_AOP.R --include-spatial-homo

args <- commandArgs(trailingOnly = FALSE)
trailing_args <- commandArgs(trailingOnly = TRUE)

script_path <- sub("^--file=", "", args[grep("^--file=", args)])
if (length(script_path) == 0) {
  script_path <- normalizePath("aop/workflow_master_AOP.R", mustWork = TRUE)
} else {
  script_path <- normalizePath(script_path, mustWork = TRUE)
}

aop_dir <- dirname(script_path)
project_root <- normalizePath(file.path(aop_dir, ".."), mustWork = TRUE)

has_flag <- function(flag) flag %in% trailing_args

arg_value <- function(flag, default = NA_character_) {
  idx <- match(flag, trailing_args)
  if (is.na(idx) || idx == length(trailing_args)) {
    return(default)
  }
  trailing_args[[idx + 1]]
}

dry_run <- has_flag("--dry-run")
continue_on_error <- has_flag("--continue-on-error")
skip_download <- has_flag("--skip-download")
include_spatial_homo <- has_flag("--include-spatial-homo")
start_step <- arg_value("--start")
stop_step <- arg_value("--stop")

steps <- data.frame(
  id = c(
    "flow.NEONAOP.EVI.Download",
    "flow.neon.site.squarebuffers",
    "flow.neon.site.simplefeatures",
    "flow_AOP_FormatLayers",
    "flow.StructuralDiversity",
    "flow.canopy",
    "flow.AOP.viz"
  ),
  file = file.path(
    aop_dir,
    c(
      "flow.NEONAOP.EVI.Download.R",
      "flow.neon.site.squarebuffers.R",
      "flow.neon.site.simplefeatures.R",
      "flow_AOP_FormatLayers.R",
      "flow.StructuralDiversity.R",
      "flow.canopy.R",
      "flow.AOP.viz.R"
    )
  ),
  purpose = c(
    "Download and mosaic NEON AOP LAI, canopy height, and vegetation-index rasters.",
    "Create square site buffers and save NEONLTERsiteBuffers.Rdata.",
    "Create site wedge/simple-feature intersections and save FG_Site_Wdges.RDATA.",
    "Extract AOP raster summaries by site/wedge and save Sites_AOP_Summary.Rdata.",
    "Extract structural-diversity raster summaries and save Structral_Diversity.Rdata.",
    "Join tower attributes, AOP summaries, and structural diversity into CanopyInformation.Rdata.",
    "Create AOP summary figures from Sites_AOP_Summary.Rdata."
  ),
  stringsAsFactors = FALSE
)

if (include_spatial_homo) {
  steps <- rbind(
    steps,
    data.frame(
      id = "Site.Spatial.Homo",
      file = file.path(aop_dir, "Site.Spatial.Homo.R"),
      purpose = paste(
        "Optional legacy spatial-homogeneity analysis.",
        "This script uses older Dropbox paths and hard-coded site rasters."
      ),
      stringsAsFactors = FALSE
    )
  )
}

if (skip_download) {
  steps <- steps[steps$id != "flow.NEONAOP.EVI.Download", ]
}

match_step <- function(value, choices) {
  if (is.na(value)) {
    return(NA_integer_)
  }

  matches <- which(choices == value | basename(choices) == value |
                     sub("\\.R$", "", basename(choices)) == value)
  if (length(matches) != 1) {
    stop(
      sprintf(
        "Could not uniquely match step '%s'. Valid step ids are:\n  %s",
        value,
        paste(steps$id, collapse = "\n  ")
      ),
      call. = FALSE
    )
  }

  matches
}

start_idx <- match_step(start_step, steps$id)
stop_idx <- match_step(stop_step, steps$id)

if (is.na(start_idx)) {
  start_idx <- 1
}

if (is.na(stop_idx)) {
  stop_idx <- nrow(steps)
}

if (stop_idx < start_idx) {
  stop("The --stop step occurs before the selected --start step.", call. = FALSE)
}

steps <- steps[seq.int(start_idx, stop_idx), ]

if (nrow(steps) == 0) {
  stop("No workflow steps selected.", call. = FALSE)
}

missing_files <- steps$file[!file.exists(steps$file)]
if (length(missing_files) > 0) {
  stop(
    sprintf("Missing workflow script(s):\n  %s", paste(missing_files, collapse = "\n  ")),
    call. = FALSE
  )
}

cat("AOP workflow master\n")
cat(sprintf("Project root: %s\n", project_root))
cat(sprintf("AOP script dir: %s\n", aop_dir))
cat(sprintf("Dry run: %s\n", dry_run))
cat(sprintf("Continue on error: %s\n\n", continue_on_error))

cat("Selected order:\n")
for (i in seq_len(nrow(steps))) {
  cat(sprintf("  %d. %s - %s\n", i, steps$id[[i]], steps$purpose[[i]]))
}
cat("\n")

if (dry_run) {
  quit(status = 0)
}

run_step <- function(step_id, step_file) {
  cat(sprintf("\n[%s] START %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), step_id))
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

  step_env <- new.env(parent = globalenv())
  step_env$PROJECT_ROOT <- project_root
  step_env$AOP_DIR <- aop_dir

  source(step_file, local = step_env, echo = FALSE, chdir = TRUE)
  cat(sprintf("[%s] DONE  %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), step_id))
}

failures <- character()

for (i in seq_len(nrow(steps))) {
  result <- tryCatch(
    {
      run_step(steps$id[[i]], steps$file[[i]])
      TRUE
    },
    error = function(e) {
      message(sprintf(
        "[%s] FAILED %s: %s",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        steps$id[[i]],
        conditionMessage(e)
      ))
      FALSE
    }
  )

  if (!isTRUE(result)) {
    failures <- c(failures, steps$id[[i]])
    if (!continue_on_error) {
      break
    }
  }
}

if (length(failures) > 0) {
  stop(
    sprintf("AOP workflow failed at: %s", paste(failures, collapse = ", ")),
    call. = FALSE
  )
}

cat("\nAOP workflow complete.\n")
