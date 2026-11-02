root <- "/nfs/roberts/project/pi_sm3466/sm3466/FluxGradient"
source(file.path(root,"runtime_v3b/lterwg-flux-gradient/functions/workflow.runtime.R"))
sites <- commandArgs(trailingOnly=TRUE)
if (!length(sites)) sites <- unique(read.csv(file.path(root,"Ameriflux_NEON field-sites.csv"))$Site_Id.NEON)
for (s in sites) {
  validate_flux_checkpoint(file.path(root,"NEON_Aligned_Concentrations_2021_2026_v3b",s,paste0(s,"_complete.RData")))
  f <- file.path(root,"NEON_GradientFlux_Data_2021_2026_v3b",s,paste0(s,"_Evaluation.RData"))
  objects <- c("MBR_9min.df.final","AE_9min.df.final","WP_9min.df.final")
  validate_rdata(f,objects)
  e <- new.env(); load(f,envir=e)
  for (obj in objects) stopifnot(is.data.frame(e[[obj]]), nrow(e[[obj]])>0)
  csv <- file.path(root,"NEON_GradientFlux_EDI_2021_2026_v3b",s,paste0(s,"_",objects,".csv"))
  stopifnot(all(file.exists(csv)),all(file.info(csv)$size>100))
  cat(s,"validated; evaluation rows:",sapply(objects,function(x)nrow(e[[x]])),"\n")
  rm(e); gc()
}
