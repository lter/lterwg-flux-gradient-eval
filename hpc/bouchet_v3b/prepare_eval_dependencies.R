p <- strsplit("RColorBrewer VSURF caret colorspace corrplot data.table dplyr epiR fs ggdendro ggforce ggplot2 ggpmisc ggpubr ggrepel ggridges googledrive rnaturalearth gridGraphics gtools lubridate lutz parallelly patchwork purrr randomForest readr rpart rpart.plot rstatix scales sf statip tibble tidyr tidyverse", " ")[[1]]
missing <- p[!vapply(p,requireNamespace,logical(1),quietly=TRUE)]
print(missing)
if(length(missing)) install.packages(missing, lib="R-library-eval-v3b", repos="https://cloud.r-project.org")
stopifnot(all(vapply(p,requireNamespace,logical(1),quietly=TRUE)))
write.csv(as.data.frame(installed.packages()[,c("Package","Version","LibPath")]),"logs_v3b/evaluation_packages.csv",row.names=FALSE)
cat("Evaluation dependencies verified\n")
