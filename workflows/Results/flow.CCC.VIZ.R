# CCC Viz and Evaluation:
rm(list=ls())

library(dplyr)
library(ggplot2)
library(ggridges)
library(colorspace)
library(ggpubr)

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
load(fs::path(localdir,paste0("SITES_One2One.Rdata")))

# Build the dataset: ####
canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct

SITES_One2One$R2 %>% round(2)

Highest.CCC <- SITES_One2One %>% reframe(.by= c(Site, gas, Approach), CCC.max = max(CCC, na.rm=T), R2.max = max(R2, na.rm=T) )

Highest.CCC %>% ggplot( aes( x= CCC.max, y= R2.max)) + geom_point()

SITES_One2One_canopy <- SITES_One2One %>% full_join(canopy, by=c("Site", "dLevelsAminusB" ) ) %>% 
  full_join( Highest.CCC,by = c("Site", "gas", "Approach")) %>% mutate(Approach = factor(Approach, levels = c("MBR", "AE", "WP") ),
                                                                       Good.CCC = case_when(  CCC >= 0.5 ~ 1, CCC < 0.5 ~ 0) %>% as.factor,
                                                                       RelativeDistB = MeasurementHeight_m_B - CanopyHeight ) %>% distinct

SITES_One2One_canopy.Highest <- SITES_One2One_canopy %>% filter( CCC== CCC.max)

SITES_One2One_canopy.Highest %>% ggplot( aes( x= CCC, y= R2, col=Site)) + geom_point()

SITES_One2One_canopy.Highest.100 <- SITES_One2One_canopy %>% filter( count > 100) %>% filter( CCC== CCC.max) 

# Non-parametric test for differences between groups: ####

# Approach:

kruskal.test(CCC ~ Approach, data = SITES_One2One_canopy %>% filter(gas=="CO2")) 
kruskal.test(CCC ~ Approach, data = SITES_One2One_canopy %>% filter(gas=="H2O")) 
# Canopy Levels:
kruskal.test(CCC ~ Canopy_L1, data = SITES_One2One_canopy %>% filter(gas=="CO2")) 
kruskal.test(CCC ~ Canopy_L1, data = SITES_One2One_canopy %>% filter(gas=="H2O")) 

SITES_One2One_canopy.subCO2 <- SITES_One2One_canopy %>% filter(gas=="CO2")
SITES_One2One_canopy.subH2O <- SITES_One2One_canopy %>% filter(gas=="H2O")

 pairwise.matrix.plot <- function(df, y, x ){
   library("ggpubr")
   library(ggplot2)
   library(ggpubr)
   library(rstatix)
   
   pairwise_results <- pairwise.wilcox.test( df[, y] , df[,x], p.adjust.method = "BH")
   
   p_values_df <- as.data.frame(as.table(pairwise_results$p.value))
   colnames(p_values_df) <- c("Group1", "Group2", "p_value")
   p_values_df <- na.omit(p_values_df) # Remove NA values from the upper triangle
   
   library(ggplot2)
   
   plot <- ggplot(p_values_df, aes(x = Group1, y = Group2, fill = p_value)) +
     geom_tile(color = "white") +
     geom_text(aes(label = round(p_value, 4)), color = "black", size = 3) +
     scale_fill_gradient2(low = "goldenrod",
                          high = "grey",
                          midpoint = 0.06, name = "P-value",
                          limits = c(0, 0.1)) +
     labs(title = "",
          x = "", y = "") +
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))
   
   return(plot )
 }
   
 matrix.approach.plot.co2 <- pairwise.matrix.plot(df = SITES_One2One_canopy.subCO2, y = "CCC", x = "Approach"  )
 matrix.canopyl1.plot.co2 <-pairwise.matrix.plot(df = SITES_One2One_canopy.subCO2, y = "CCC", x = "Canopy_L1"  )
 matrix.approach.plot.h2o <- pairwise.matrix.plot(df = SITES_One2One_canopy.subH2O, y = "CCC", x = "Approach"  )
 matrix.canopyl1.plot.h2o <-pairwise.matrix.plot(df = SITES_One2One_canopy.subH2O, y = "CCC", x = "Canopy_L1"  )
 
 final.plot.matrix <- ggarrange( matrix.approach.plot.co2,  matrix.canopyl1.plot.co2,
            matrix.approach.plot.h2o,  matrix.canopyl1.plot.h2o,
            ncol=2 , nrow=2, labels= c("A", "B", "C", "D"), common.legend = T, legend = "right")
 
 
 ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/final.plot.matrix.png", plot =  final.plot.matrix, width = 5, height = 5, units = "in")
 
 # Figure 3 ####
 
 plot.pairwise.co2.canopy <- SITES_One2One_canopy %>% filter(gas == "CO2") %>% ggplot( aes( x= Canopy_L1, y = CCC , col= Canopy_L1)) +
   geom_boxplot() + scale_color_manual(values=c("black", "black", "black"),
                                       name= "Canopy") +theme_bw() + xlab("Measurement Level") + theme(legend.position="none")
 
 plot.pairwise.co2.approach <- SITES_One2One_canopy %>% filter(gas == "CO2") %>% 
   ggplot( aes( x= Approach, y = CCC , col= Approach)) +
   geom_boxplot() + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta"),
                                       name= "Approach") +theme_bw() + theme(legend.position="none")
 
 
 plot.pairwise.h2o.canopy <- SITES_One2One_canopy %>% filter(gas == "H2O") %>% ggplot( aes( x= Canopy_L1, y = CCC , col= Canopy_L1)) +
   geom_boxplot() + scale_color_manual(values=c("black", "black", "black"),
                                       name= "Canopy") +theme_bw()+ xlab("Measurement Level")+ theme(legend.position="none")
 
 plot.pairwise.h2o.approach <- SITES_One2One_canopy %>% filter(gas == "H2O") %>% 
   ggplot( aes( x= Approach, y = CCC , col= Approach)) +
   geom_boxplot() + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta"),
                                       name= "Approach") +theme_bw()+ theme(legend.position="none")
 

evaluation.ccc.plot <-  ggarrange(  plot.pairwise.co2.approach ,
                                    plot.pairwise.h2o.approach ,
                                    plot.pairwise.co2.canopy ,
                                    plot.pairwise.h2o.canopy ,  
                          
                                    
                                    ncol=2, nrow=2, common.legend = FALSE,
                                    labels = c("A", "B", "C", "D"))
 
 
evaluation.ccc.plot

library(ggridges)

ggridges.co2 <- SITES_One2One_canopy %>% filter(gas=="CO2") %>% ggplot( aes(x = CCC, y = Canopy_L1, fill = Approach)) + geom_density_ridges(alpha=0.5) + theme_ridges() +
  theme(legend.position = "none") +
  scale_fill_manual(name = "Approach",
                    values=c("goldenrod", "aquamarine4", "darkmagenta"),
                    labels = c("MBR", "AE", "WP")) + ylab("") +xlab("CCC")

ggridges.h2o <- SITES_One2One_canopy %>% filter(gas=="H2O") %>% ggplot( aes(x = CCC, y = Canopy_L1, fill = Approach)) + geom_density_ridges(alpha=0.5) + theme_ridges() +
  theme(legend.position = "none") +
  scale_fill_manual(name = "Approach",
                    values=c("goldenrod", "aquamarine4", "darkmagenta"),
                    labels = c("MBR", "AE", "WP")) + ylab("") +xlab("CCC")

evaluation.ggridges <-  ggarrange( ggridges.co2, ggridges.h2o,ncol=2,
                                    labels = c("E", "F"))


final.eval.ccc.plot <- ggarrange( evaluation.ccc.plot, evaluation.ggridges , ncol=1)

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/final.eval.ccc.plot.png", plot = final.eval.ccc.plot, width = 5, height = 6, units = "in")


 # CCC  Evaluation Threshold ####
plot.CCC.CO2.all <- SITES_One2One_canopy %>% filter(gas=="CO2") %>%  ggplot() + geom_point( aes(x= CCC, y=Site, col=Approach ), alpha= 0.7, size = 3) + facet_grid(cols = vars(Canopy_L1)) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + theme_bw() + ylab("") + geom_vline(xintercept= 0.5, linetype="dashed", color ="red")


plot.CCC.H2O.all <- SITES_One2One_canopy %>% filter(gas=="H2O") %>%  ggplot() + geom_point( aes(x= CCC, y=Site, col=Approach ), alpha= 0.7, size = 3) + facet_grid(cols = vars(Canopy_L1)) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + theme_bw() + ylab("") + geom_vline(xintercept= 0.5, linetype="dashed", color ="red")

ggsave("Figures/CCC_plot_co2.png", plot = plot.CCC.CO2.all, width = 8, height = 9, units = "in")
ggsave("Figures/CCC_plot_h2o.png", plot = plot.CCC.H2O.all, width = 8, height = 9, units = "in")


plot.CCC.all <- SITES_One2One_canopy %>%  ggplot() + geom_point( aes(x= CCC, y=Site, col=Approach ), alpha= 0.7, size = 3) + facet_grid(cols = vars(gas)) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + theme_bw() + ylab("") + geom_vline(xintercept= 0.5, linetype="dashed", color = "red")


ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/CCC_plot_all.png", plot = plot.CCC.all, width = 6, height = 6, units = "in")

# Determine the distribution of sampling pair heights:
library(tidyverse)
sitepairs.total <- SITES_One2One_canopy %>% filter(gas == "H2O") %>% select(dLevelsAminusB, Site) 

sitepairs.h2o <- SITES_One2One_canopy %>% filter(gas == "H2O", CCC >= 0.5) %>% select(dLevelsAminusB, Site)
sitepairs.co2 <- SITES_One2One_canopy %>% filter(gas == "CO2", CCC >= 0.5) %>% select(dLevelsAminusB, Site)

sitepairs.same <-intersect(sitepairs.h2o, sitepairs.co2) 


df_overall <- tibble::tribble(
  ~category, ~n_pairs, ~total_pairs,
  "CO2", sitepairs.co2$dLevelsAminusB %>% length, sitepairs.total$dLevelsAminusB %>% length,
  "H2O", sitepairs.h2o$dLevelsAminusB %>% length, sitepairs.total$dLevelsAminusB %>% length,
  "Both", sitepairs.same$Site %>% length, sitepairs.total$dLevelsAminusB %>% length
) |>
  mutate(percent = 100 * n_pairs / total_pairs)

p_overall <- ggplot(df_overall, aes(x = category, y = percent)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%% (n=%d)", percent, n_pairs)),
            vjust = -0.3, size = 3) +
  labs(
    x = NULL,
    y = "Sampling Height Pairs (%)",
    title = "" ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_bw() +
  theme( axis.text.x = element_text(angle = 20, hjust = 1))
 
# Approach Summary
df_method <- SITES_One2One_canopy %>% filter( CCC >= 0.5) %>%  mutate( count = 1) %>% reframe(.by= c(gas, Approach), n_pairs = sum(count)) |>
  group_by(gas) |>
  mutate(percent = 100 * n_pairs / sum(n_pairs)) |>
  ungroup()

## ======================

p_method <- ggplot(df_method, aes(x = gas, y = percent, fill = Approach),alpha=0.05) +
  geom_col() +
  geom_text(aes(label = sprintf("%.0f%%", percent)),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(x = NULL,
    y = "Sampling Height Pairs (%)",
    title = "",
    fill = "Approach")  +
  theme_bw()  + scale_fill_manual(values=c("goldenrod", "aquamarine4", "darkmagenta"))

## ======================

# Canopy:
df_canopy <- SITES_One2One_canopy %>% filter( CCC >= 0.5) %>%  mutate( count.sh = 1) %>% reframe(.by= c(gas, Canopy_L1), n_pairs = sum(count.sh)) |>
  group_by(gas) |>
  mutate(percent = 100 * n_pairs / sum(n_pairs)) |>
  ungroup()

# NOTE: if you revise the counts, you only need to edit df_canopy above.

p_canopy <- ggplot(df_canopy, aes(x = gas, y = percent, fill = Canopy_L1)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.0f%%", percent)),
            position = position_stack(vjust = 0.5), size = 3) +
  labs(
    x = NULL,
    y = "Sampling Height Pairs (%)",
    title = "",
    fill = "Canopy Level"
  )  +
  theme_bw() +  scale_fill_discrete_sequential(palette = "OrYel")

plots.samplingHeights.counts <- ggarrange( p_overall , p_method , p_canopy,
           nrow = 1, labels=c('A', 'B', 'C'))

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/SamplingHeightPairs.png", plot = plots.samplingHeights.counts, width = 8.5, height = 3, units = "in")


SITES_One2One_canopy %>% filter( CCC >= 0.5) %>% mutate( count.sh = 1) %>% 
  reframe(.by= c(Canopy_L1, gas), total.05 = sum(count.sh ), R2 = mean(R2), CCC = mean(CCC))

# Number of levels across sites:
mean.counts <- SITES_One2One_canopy %>% filter(gas == "CO2", CCC >= 0.5) %>% mutate( count.sh = 1) %>% reframe(.by= c(Site), Mean.count = sum(count.sh)) 
mean.counts$Mean.count %>% mean
mean.counts$Mean.count %>% sd       

localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
setwd(localdir)

save(plot.CCC.CO2.all ,
     plot.CCC.H2O.all, plots.samplingHeights.counts, file = "flow.CCC_VIZ.R" )

drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/14Ga9sLRMlQVvorZdHBiYxCbUybiGwPNp")
fileSave <- file.path(paste(localdir, "flow.CCC_VIZ.R", sep="/"))
googledrive::drive_upload(media = fileSave, overwrite = T, path = drive_url)

