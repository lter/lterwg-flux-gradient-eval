# flow.DIEL.VIZ:
rm(list=ls())
DirRepo <-"/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient-eval"

setwd(DirRepo)

load('/Volumes/MaloneLab/Research/FluxGradient/DIEL_SUMMARY.RDATA')


ggplot(data= diel.summary.year.gas.cl2.typ %>%filter(gas=="CO2")) + geom_point(aes(x= ccc, y=DIFF, col=TYP), alpha=0.01)+
  geom_smooth(aes(x= ccc, y=DIFF, col=TYP), method='loess', se=T) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + 
  facet_wrap(~Canopy_L2) + theme_bw() + ylab('Diel Difference') 

ggplot(data= diel.summary.mean.line.co2.cl2.typ) + 
  geom_point(aes(x= ccc, y=DIFF.total, col=TYP), alpha= 0.3, size = 3) +
  geom_smooth(aes(x= ccc, y=DIFF.total, col=TYP), method='loess', se=F) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) +facet_wrap(~Canopy_L2) + theme_bw() + ylab('Diel Total Difference') 

ggplot(data= diel.summary.mean.line.co2.cl2.typ) + 
  geom_point(aes(x= ccc, y=DIFF.max, col=TYP), alpha= 0.3, size = 3) +
  geom_smooth(aes(x= ccc, y=DIFF.max, col=TYP), method='loess', se=F) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) +facet_wrap(~Canopy_L2) + theme_bw() + ylab('Diel Maximum Difference') 

ggplot(data= diel.summary.mean.line.co2.cl2.typ) + 
  geom_point(aes(x= ccc, y=DIFF.min, col=TYP), alpha= 0.3, size = 3) +
  geom_smooth(aes(x= ccc, y=DIFF.min, col=TYP), method='loess', se=F) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) +facet_wrap(~Canopy_L2) + theme_bw() + ylab('Diel Minimum Difference') 


## H2O:
ggplot(data= diel.summary.year.gas.cl2.typ %>%filter(gas=="H2O")) + geom_point(aes(x= ccc, y=DIFF, col=TYP), alpha=0.01)+
  geom_smooth(aes(x= ccc, y=DIFF, col=TYP), method='loess', se=T) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) + 
  facet_wrap(~Canopy_L2) + theme_bw() + ylab('Diel Difference') 

ggplot(data= diel.summary.mean.line.h2o.cl2.typ) + 
  geom_point(aes(x= ccc, y=DIFF.total, col=TYP), alpha= 0.3, size = 3) +
  geom_smooth(aes(x= ccc, y=DIFF.total, col=TYP), method='loess', se=F) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) +facet_wrap(~Canopy_L2) + theme_bw() + ylab('Diel Total Difference') 

ggplot(data= diel.summary.mean.line.h2o.cl2.typ) + 
  geom_point(aes(x= ccc, y=DIFF.max, col=TYP), alpha= 0.3, size = 3) +
  geom_smooth(aes(x= ccc, y=DIFF.max, col=TYP), method='loess', se=F) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) +facet_wrap(~Canopy_L2) + theme_bw() + ylab('Diel Maximum Difference') 

ggplot(data= diel.summary.mean.line.h2o.cl2.typ) + 
  geom_point(aes(x= ccc, y=DIFF.min, col=TYP), alpha= 0.3, size = 3) +
  geom_smooth(aes(x= ccc, y=DIFF.min, col=TYP), method='loess', se=F) + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta")) +facet_wrap(~Canopy_L2) + theme_bw() + ylab('Diel Minimum Difference') 



# Summarized Plots by CCC ####

diel.summary.mean.line.co2 <- diel.summary.year.gas %>% filter(gas=="CO2") %>% reframe(.by=ccc,DIFF.total = mean(DIFF),
                                                                                       DIFF.max = mean(FG.max - EC.max),
                                                                                       DIFF.min = mean(FG.min - EC.min)) %>% fortify() %>% mutate( DIFF.total.gC = (DIFF.total/1000000)*44.01*1800, DIFF.max.gC = (DIFF.max/1000000)*44.01*1800,DIFF.min.gC = (DIFF.min/1000000)*44.01*1800)
                                                                                         
                                                                                         
diel.summary.mean.line.h2o <- diel.summary.year.gas %>% filter(gas=="H2O") %>%  reframe(.by=ccc,DIFF.total = mean(DIFF),
                                                                                        DIFF.max = mean(FG.max - EC.max),
                                                                                        DIFF.min = mean(FG.min - EC.min)) %>% fortify() %>% mutate( DIFF.total.gC = (DIFF.total/1000000)*18.05*1800, DIFF.max.gC = (DIFF.max/1000000)*18.05*1800,DIFF.min.gC = (DIFF.min/1000000)*18.05*1800)


# How does the diel change with CCC threshold


co2.diel.plot <- ggplot(data= diel.summary.mean.line.co2) + geom_point(aes(x= ccc, y=DIFF.total.gC), alpha= 0.3, size = 3) + geom_smooth(aes(x= ccc, y=DIFF.total.gC,), se=F, col="black") +
  geom_point(aes(x= ccc, y=DIFF.max.gC), alpha= 0.3, size = 3) + 
  geom_smooth(aes(x= ccc, y=DIFF.max.gC), col="grey", se=F) +
  geom_point(aes(x= ccc, y=DIFF.min.gC), alpha= 0.3, size = 3) + 
  geom_smooth(aes(x= ccc, y=DIFF.min.gC), col="grey40", se=F) + 
  theme_bw() + ylab("Absolute Difference") + 
  annotate(geom='text', x=0.53, y=1,label="Total", col="black", size=6) +
  annotate(geom='text', x=0.55, y=2,label="Maximum", col="grey", size=6) +
  annotate(geom='text', x=0.55, y=0.1,label="Minimum", col="grey40", size=6)



h2o.diel.plot <- ggplot(data= diel.summary.mean.line.h2o) +
  geom_point(aes(x= ccc, y=DIFF.total.gC), alpha= 0.3, size = 3) + 
  geom_smooth(aes(x= ccc, y=DIFF.total.gC), se=F, col="black") +
  geom_point(aes(x= ccc, y=DIFF.max.gC), alpha= 0.3, size = 3) + 
  geom_smooth(aes(x= ccc, y=DIFF.max.gC), col="grey", se=F) +
  geom_point(aes(x= ccc, y=DIFF.min.gC), alpha= 0.3, size = 3) + 
  geom_smooth(aes(x= ccc, y=DIFF.min.gC), col="grey40", se=F) + theme_bw()+ 
  ylab("Absolute Difference") + ylim(-0.5, 0.5)


diel.summary.mean.line.typ.co2 <- diel.summary.year.gas.typ %>% filter(gas=="CO2") %>% reframe(.by=c(ccc, TYP),DIFF.total = mean(DIFF),
                                                                                               DIFF.max = mean(FG.max - EC.max),
                                                                                               DIFF.min = mean(FG.min - EC.min)) %>% fortify() %>% mutate( DIFF.total.gC = (DIFF.total/1000000)*44.01*1800, DIFF.max.gC = (DIFF.max/1000000)*44.01*1800,DIFF.min.gC = (DIFF.min/1000000)*44.01*1800)

diel.summary.mean.line.typ.h2o <- diel.summary.year.gas.typ %>% filter(gas=="H2O") %>%  reframe(.by=c(ccc, TYP),DIFF.total = mean(DIFF),
                                                                                                DIFF.max = mean(FG.max - EC.max),
                                                                                                DIFF.min = mean(FG.min - EC.min)) %>% fortify() %>% mutate( DIFF.total.gC = (DIFF.total/1000000)*18.05*1800, DIFF.max.gC = (DIFF.max/1000000)*18.05*1800,DIFF.min.gC = (DIFF.min/1000000)*18.05*1800)

# How does the diel change with CCC threshold


plot.approach.daily.max.co2 <- ggplot(data= diel.summary.mean.line.typ.co2) + geom_point(aes(x= ccc, y=DIFF.total.gC, col=TYP), alpha= 0.4, size = 3) + geom_smooth( aes(x= ccc, y=DIFF.total.gC), alpha=0.1, col="black") + 
  geom_point( aes(x= ccc, y=DIFF.max.gC, col=TYP), alpha= 0.4, size = 3) + 
  geom_smooth( aes(x= ccc, y=DIFF.max.gC), alpha=0.1, col="grey40") +
  geom_point( aes(x= ccc, y=DIFF.min.gC, col=TYP), alpha= 0.4, size = 3) + 
  geom_smooth( aes(x= ccc, y=DIFF.min.gC), alpha=0.1, col="grey") + 
  ylab("Absolute Difference") + 
  #annotate(geom='text', x=0.65, y=-1,label="Daily Total", col="black", size=6) +
  theme_bw() + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta"), name="Approach") 



# H2O:

plot.approach.daily.max.h2o <- ggplot(data= diel.summary.mean.line.typ.h2o) + geom_point(aes(x= ccc, y=DIFF.total.gC, col=TYP), alpha= 0.4, size = 3) + geom_smooth( aes(x= ccc, y=DIFF.total.gC), alpha=0.1, col="black") + 
geom_point( aes(x= ccc, y=DIFF.max.gC, col=TYP), alpha= 0.4, size = 3) + 
  geom_smooth( aes(x= ccc, y=DIFF.max.gC), alpha=0.1, col="grey40") +
  geom_point( aes(x= ccc, y=DIFF.min.gC, col=TYP), alpha= 0.4, size = 3) + 
  geom_smooth( aes(x= ccc, y=DIFF.min.gC), alpha=0.1, col="grey") + 
  ylab("Absolute Difference") + 
  #annotate(geom='text', x=0.65, y=-1,label="Daily Total", col="black", size=6) +
  theme_bw() + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta"), name="Approach") 


diel.plot.a <- ggarrange(co2.diel.plot, h2o.diel.plot, nrow=1, labels=c("A", "B"))

diel.plot.b <- ggarrange( plot.approach.daily.max.co2,
                          plot.approach.daily.max.h2o, nrow=1,
                          common.legend = TRUE, labels=c("C", "D"))

diel.plot.final <- ggarrange( diel.plot.a, diel.plot.b,nrow=2)

ggsave("Figures/DIEL_plot_H2O_CO2.png", plot = diel.plot.final, width = 6, height =6, units = "in")
