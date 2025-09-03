# flow.DIEL.VIZ:
rm(list=ls())

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


co2.diel.plot <- ggplot(data= diel.summary.mean.line.co2) + geom_point(aes(x= ccc, y=DIFF.total.gC), alpha= 0.3, size = 3) + geom_line(aes(x= ccc, y=DIFF.total.gC)) +
  geom_point(aes(x= ccc, y=DIFF.max.gC), alpha= 0.3, size = 3) + geom_line(aes(x= ccc, y=DIFF.max.gC), col="blue") +
  geom_point(aes(x= ccc, y=DIFF.min.gC), alpha= 0.3, size = 3) + geom_line(aes(x= ccc, y=DIFF.min.gC), col="darkgreen") + theme_bw() + ylab("Absolute Difference") + 
  annotate(geom='text', x=0.55, y=3,label="Total", col="black", size=6) +
  annotate(geom='text', x=0.55, y=2.7,label="Maximum", col="blue", size=6) +
  annotate(geom='text', x=0.55, y=2.4,label="Minimum", col="darkgreen", size=6)



h2o.diel.plot <- ggplot(data= diel.summary.mean.line.h2o) + geom_point(aes(x= ccc, y=DIFF.total.gC), alpha= 0.3, size = 3) + geom_line(aes(x= ccc, y=DIFF.total.gC)) +
  geom_point(aes(x= ccc, y=DIFF.max.gC), alpha= 0.3, size = 3) + geom_line(aes(x= ccc, y=DIFF.max.gC), col="blue") +
  geom_point(aes(x= ccc, y=DIFF.min.gC), alpha= 0.3, size = 3) + geom_line(aes(x= ccc, y=DIFF.min.gC), col="darkgreen") + theme_bw()+ 
  ylab("Absolute Difference") + ylim(-0.5, 0.5)


diel.summary.mean.line.typ.co2 <- diel.summary.year.gas.typ %>% filter(gas=="CO2") %>% reframe(.by=c(ccc, TYP),DIFF.total = mean(DIFF),
                                                                                               DIFF.max = mean(FG.max - EC.max),
                                                                                               DIFF.min = mean(FG.min - EC.min)) %>% fortify() %>% mutate( DIFF.total.gC = (DIFF.total/1000000)*44.01*1800, DIFF.max.gC = (DIFF.max/1000000)*44.01*1800,DIFF.min.gC = (DIFF.min/1000000)*44.01*1800)

diel.summary.mean.line.typ.h2o <- diel.summary.year.gas.typ %>% filter(gas=="H2O") %>%  reframe(.by=c(ccc, TYP),DIFF.total = mean(DIFF),
                                                                                                DIFF.max = mean(FG.max - EC.max),
                                                                                                DIFF.min = mean(FG.min - EC.min)) %>% fortify() %>% mutate( DIFF.total.gC = (DIFF.total/1000000)*18.05*1800, DIFF.max.gC = (DIFF.max/1000000)*18.05*1800,DIFF.min.gC = (DIFF.min/1000000)*18.05*1800)

# How does the diel change with CCC threshold
plot.approach.daily.total.co2 <- ggplot(data= diel.summary.mean.line.typ.co2) + geom_point(aes(x= ccc, y=DIFF.total.gC, col=TYP), alpha= 0.4, size = 3) + 
  geom_smooth(aes(x= ccc, y=DIFF.total.gC), alpha=0.1, linetype="dotted", col="grey") + 
  ylab("Absolute Difference") + 
  #annotate(geom='text', x=0.65, y=-1,label="Daily Total", col="black", size=6) +
  theme_bw() + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta"), name="Approach") 


plot.approach.daily.max.co2 <- ggplot(data= diel.summary.mean.line.typ.co2) + geom_point(aes(x= ccc, y=DIFF.max.gC, col=TYP), alpha= 0.3, size = 3) + 
  ylab("Difference") + 
  #annotate(geom='text', x=0.65, y=-1,label="Daily Maximum", col="black", size=6) +
  geom_smooth(aes(x= ccc, y=DIFF.max.gC), linetype="dotted", col="grey") + 
  theme_bw() + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta"))

plot.approach.daily.min.co2 <- ggplot(data= diel.summary.mean.line.typ.co2) + geom_point(aes(x= ccc, y=DIFF.min.gC, col=TYP), alpha= 0.3, size = 3) + 
  geom_smooth(aes(x= ccc, y=DIFF.min.gC), linetype="dotted", col="grey") + 
  ylab("Difference") + 
  #annotate(geom='text', x=0.65, y=-1,label="Daily Minimum", col="black", size=6) +
  theme_bw() + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta"))


# H2O:
plot.approach.daily.total.h2o <- ggplot(data= diel.summary.mean.line.typ.h2o) + geom_point(aes(x= ccc, y=DIFF.total, col=TYP), alpha= 0.4, size = 3) + 
  geom_smooth(aes(x= ccc, y=DIFF.total), alpha=0.1, linetype="dotted", col="grey") + 
  ylab("Absolute Difference") + 
  #annotate(geom='text', x=0.65, y=1,label="Daily Total", col="black", size=6) +
  theme_bw() + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta"), name="Approach") 


plot.approach.daily.max.h2o <- ggplot(data= diel.summary.mean.line.typ.h2o) + geom_point(aes(x= ccc, y=DIFF.max, col=TYP), alpha= 0.3, size = 3) + 
  ylab("Difference") + 
  #annotate(geom='text', x=0.65, y=-1,label="Daily Maximum", col="black", size=6) +
  geom_smooth(aes(x= ccc, y=DIFF.max), linetype="dotted", col="grey") + 
  theme_bw() + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta"))

plot.approach.daily.min.h2o <- ggplot(data= diel.summary.mean.line.typ.h2o) + geom_point(aes(x= ccc, y=DIFF.min, col=TYP), alpha= 0.3, size = 3) + 
  geom_smooth(aes(x= ccc, y=DIFF.min), linetype="dotted", col="grey") + 
  ylab("Difference") + 
  #annotate(geom='text', x=0.65, y=5,label="Daily Minimum", col="black", size=6) +
  theme_bw() + scale_color_manual(values=c("goldenrod", "aquamarine4", "darkmagenta"))



diel.plot.a <- ggarrange(co2.diel.plot, h2o.diel.plot, nrow=1, labels=c("A", "B"))

diel.plot.b <- ggarrange( plot.approach.daily.total.co2,
                          plot.approach.daily.max.co2,
                          plot.approach.daily.min.co2, nrow=1,
                          common.legend = TRUE, labels=c("C", "D", "E"))

diel.plot.c <- ggarrange( plot.approach.daily.total.h2o,
                          plot.approach.daily.max.h2o,
                          plot.approach.daily.min.h2o, nrow=1,
                          common.legend = TRUE, labels=c("F", "G", "H"))

diel.plot.final <- ggarrange( diel.plot.a, diel.plot.b, diel.plot.c,nrow=3)

ggsave("Figures/DIEL_plot_H2O.png", plot = diel.plot.final, width = 9, height = 9, units = "in")
