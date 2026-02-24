# Pre-Ensemble Checks: ####
localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
DirRepo.eval <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"

# Pre-validation: ####.  START HERE.. ####
load(file= paste(localdir, "SITES_One2One_canopy_model.Rdata", sep="" ))
load( fs::path(localdir,paste0("SITE_DATA_FILTERED_CCC.Rdata")))
source(fs::path(DirRepo.eval,"./functions/calc_validation.R"))
source(fs::path(DirRepo.eval,"./functions/calc.linear.terms.R"))
source(fs::path(DirRepo.eval,"./functions/calc.lins.ccc.R"))
load(  file= paste(localdir, "SITE_RSHP_MODEL.Rdata", sep="") )

# Select information needed to ID the reliable levels:

canopy.adj <- val.SHP.total.canopy.summary %>% select( Site, Approach, gas, dLevelsAminusB, Good.CCC) %>% rename( Good.CCC.EC = Good.CCC)

canopy.adj$Good.CCC.EC %>% as.factor %>% summary

canopy.adj.reliable <- canopy.adj %>% filter(Good.CCC.EC == "1" ) 
canopy.adj.reliable$Site %>% unique %>% length # 41 Sites


# Run-pre.validation analysis for sampling height pairs:  #####

pre.val.SHP <- validation.SamplingHeightPairs_V1(index.df =  canopy.adj.reliable) %>% mutate(combination = paste(x, y, sep="-")) %>% rename( CCC.GF = CCC)

plot.preval.SHP.sites <- pre.val.SHP %>% ggplot() + 
  geom_boxplot( aes( x= CCC.GF,  y = Site)) +
  facet_wrap(~gas)+ theme_bw()+ xlab(" CCC (Sampling Height Pairs)") +
  ylab("") + geom_vline(xintercept= 0, col="red") + xlim(-1,1) +
  theme(strip.background =element_rect(fill="transparent"))

plot.preval.SHP.density <- pre.val.SHP %>% ggplot() + 
  geom_density( aes( x= CCC.GF)) +  xlim(-1,1) + 
  facet_wrap(~gas)+ theme_bw()+ xlab(" CCC (Sampling Height Pairs)") +
  ylab("") + geom_vline(xintercept= 0, col="red") +
  theme(strip.background =element_rect(fill="transparent"))

final.plots.preval.SHP <- ggarrange( plot.preval.SHP.sites,
                                     plot.preval.SHP.density, 
                                     ncol=1, labels = c("A", "B"),
                                     heights=c(3, 1))

pre.val.approach <- validation.approach_V1(index.df =  canopy.adj) %>% mutate(combination = paste(x, y, sep="-")) %>% rename( CCC.GF = CCC)

plot.preval.approach <- pre.val.approach %>% ggplot() + 
  geom_boxplot( aes( x= CCC.GF,  y = combination)) +
  geom_point( aes( x= CCC.GF,  y = combination)) +
  facet_wrap(~gas) + theme_bw() + xlim(-1,1) +
  theme(strip.background =element_rect(fill="transparent"))+ 
  xlab(" CCC (Approach)") + ylab( "Approach") +
  geom_vline( xintercept = 0, col="red") 

plot.preval.approach.sites <- pre.val.approach %>% ggplot() + 
  geom_boxplot( aes( x= CCC.GF,  y = Site)) +
  geom_point( aes( x= CCC.GF,  y = Site))+xlim(-1,1) +
  facet_wrap(~gas)+ theme_bw()+ xlab(" CCC (Approach)") + ylab("") +
  theme(strip.background =element_rect(fill="transparent")) +
  geom_vline( xintercept = 0, col="red") 

finalplot.preval.approach <- ggarrange( plot.preval.approach.sites, plot.preval.approach, labels=c("A", "B"),
                                        ncol=1, heights=c(3, 1))

pre.val.canopy <- validation.canopy.l1_V1( index.df = canopy.adj) %>% mutate(combination = paste(x, y, sep="-")) %>% rename( CCC.GF = CCC) %>% mutate(
  combination.adj = case_when( combination == "AW-WW" | combination =="WW-AW" ~ "AW-WW",
                               combination == "AW-AA" | combination =="AA-AW" ~ "AA-AW",
                               combination == "AA-WW" | combination =="WW-AA" ~ "AA-WW") )

plot.preval.canopy <- pre.val.canopy  %>% 
  ggplot() + geom_boxplot( aes( x= CCC.GF,  y = combination.adj)) +
  facet_wrap(~gas) + theme_bw() +
  theme(strip.background =element_rect(fill="transparent"))+ 
  xlab(" CCC (Canopy Level)") + ylab("") +   xlim(-1,1)  +
  geom_vline( xintercept = 0, col="red") 

plot.preval.canopy.sites <- pre.val.canopy %>% ggplot() + 
  geom_boxplot( aes( x= CCC.GF,  y = Site)) +
  facet_wrap(~gas)+ theme_bw()+ xlab(" CCC (Canopy Level)")+
  theme(strip.background =element_rect(fill="transparent"))+
  xlim(-1,1) +
  geom_vline( xintercept = 0, col="red") 

finalplot.preval.canopy <- ggarrange(plot.preval.canopy.sites,plot.preval.canopy, ncol=1,
                                     labels=c("A", "B"), heights = c(3,1))


final.pre.validation.plot <- ggarrange(plot.preval.SHP.density,
                                       plot.preval.approach, plot.preval.canopy , ncol=1,
                                       labels=c("A", "B", "C"), heights = c(1,1,1))

# Save the plots:

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/PreValidation.SHP.APPROACH.CL1_V1.png",
       plot =  final.pre.validation.plot, width = 6, height = 7, units = "in")

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/PreValidation.APPROACH_V1.png",
       plot =  finalplot.preval.approach, width = 6, height = 7, units = "in")

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/PreValidation_V1.CL1.png",
       plot =  finalplot.preval.canopy, width = 6, height = 7, units = "in")

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/PreValidation_V1.SHP.png",
       plot =  final.plots.preval.SHP, width = 6, height = 7, units = "in")

# Post-validation: ####
post.validation <- validation_V1( index.df = canopy.adj) %>% rename( CCC.EC = CCC)

plot.post.val <- post.validation %>% ggplot() + 
  geom_point( aes( x= CCC.EC,  y = Site)) + xlim(-1,1) +
  facet_wrap(~gas) + theme_bw() + xlab(" CCC (EC)") +
  geom_vline( xintercept = 0, col="red") +
  theme(strip.background =element_rect(fill="transparent")) + ylab("")

plot.post.val.density <- post.validation %>% ggplot() + 
  geom_density( aes( x= CCC.EC)) + xlim(-1,1) +
  facet_wrap(~gas) + theme_bw() + xlab(" CCC (EC)") +
  geom_vline( xintercept = 0, col="red") +
  theme(strip.background =element_rect(fill="transparent")) + ylab("")

final.post.validation.plot <- ggarrange(plot.post.val,
                                        plot.post.val.density, ncol=1,
                                        labels=c("A", "B"),
                                        heights=c(3,1))

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/Validation.PLOTS_V1.png",
       plot =  final.post.validation.plot, width = 6, height = 7, units = "in")

final.post.validation.plot <- ggarrange(plot.post.val,
                                        plot.post.val.density, ncol=1,
                                        labels=c("D", "E"),
                                        heights=c(3,1))

SUper.plot <- ggarrange( final.pre.validation.plot,final.post.validation.plot , ncol=2 )

ggsave("/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval/Figures/WF_Version1/Validation.Harmonization_V1.png",
       plot =  SUper.plot, width = 8, height = 7, units = "in")



canopy.adj2 <- SITES_One2One_canopy_model %>%
  reframe( .by= c(Site, gas), 
           canopyHeight_m = mean(canopyHeight_m),
           LAI.mean = mean(LAI.mean),
           LAI.sd = mean(LAI.sd),
           count = mean(count),
           predicted = max(predicted %>% as.numeric),
           CCC.GF = mean(CCC.max, na.rm = T))


validation <- post.validation %>% left_join(canopy.adj2, by=c("Site", "gas") ) %>% left_join( pre.val.SHP %>% reframe( .by=c("Site", "gas"), CCC.SHP=mean( CCC.GF, na.rm=T)), by=c("Site", "gas") )

validation %>% ggplot() + geom_point( aes(y=CCC.EC , x=CCC.SHP)) + ylim(-1,1) + xlim(-1,1)

cor.val <- cor(validation[, c( 6:7, 9:13) ], method = "pearson", use = "complete.obs")

library(corrplot)
corrplot(cor.val , type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

lm( data=validation,CCC.EC ~ count  ) %>% summary
lm( data=validation,CCC.EC ~ CCC.SHP  ) %>% summary

