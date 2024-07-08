


library(plotly)
library(readxl)

install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")

library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
library(mgcv)
library(mgcViz)
install.packages("readxl")
install.packages("tidyverse", dependencies = TRUE)
library(forcats)
install.packages("RColorBrewer")
library(RColorBrewer)

install.packages("cowplot")
library(cowplot)
library(gridExtra)
library(grid) 
install.packages("magrittr")
library(magrittr)
install.packages("ggtext")
library(ggtext)

install.packages("vioplot")
library(vioplot)





Bio18 <- with(scotland_new_biolayers,(Bio18))

Bio16 <- with(scotland_new_biolayers,(Bio16))

###### WITH Lobaria!!!!!!!!!! ##########################################################

scotland_new_biolayers$Species <- factor(scotland_new_biolayers$Species, levels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Sticta sylvatica", "Ricasolia virens", "Hypotrachyna laevigata", "Pectenia atlantica"))



bio18 <- scotland_new_biolayers %>% 
  group_by(Species) %>% 
  summarise (
    mean = mean(Bio18, na.rm = TRUE),
    median = median(Bio18))
bio18

bio16 <- scotland_new_biolayers %>% 
  group_by(Species) %>% 
  summarise (
    mean = mean(Bio16, na.rm = TRUE),
    median = median(Bio16))
bio16




bio.16 <- ggplot(data = scotland_new_biolayers, aes(x = reorder(Species,Bio16, median), y = Bio16, colour = Species, fill = Species)) +
  geom_violin() +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.9, outlier.shape = NA, show.legend = FALSE) + 
  scale_fill_manual(values = c( "slategrey","darkorange3", "purple4", "goldenrod4", "forestgreen", "deeppink4", "darkcyan"))+  
  scale_colour_manual(values = c("slategrey","darkorange3", "purple4", "goldenrod4", "forestgreen", "deeppink4", "darkcyan")) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.y = element_markdown(size = 14,)) +
  stat_summary(fun="mean", geom="point", size=2, color="black", show.legend = FALSE) +
  ylab("Mean precipitation of the wettest quarter <br>(kg m<sup>-2</sup>)")    +
  scale_y_continuous(breaks=seq(0,1200, by=200), expand= c(0,0)) +
  coord_cartesian(ylim=c(0,1200)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin=unit(c(1,-0.05,0.25,1), "cm")) +
  theme(axis.text.y = element_text(size = 12,))


bio.16 <- bio.16  + geom_violin(trim = FALSE) + 
  stat_summary(
    fun.data = "mean_sdl",  fun.args = list(mult = 1), 
    geom = "pointrange", color = "black"
  )



bio.18 <- ggplot(data = Climate_Data_UK_All_Species_new_only, aes(x = reorder(Species,Bio_18, median), y = Bio_18, colour = Species, fill = Species)) +
  geom_violin() +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.9, outlier.shape = NA, show.legend = FALSE) + 
  scale_fill_manual(values = c( "slategrey","darkorange3", "purple4", "goldenrod4", "forestgreen", "deeppink4", "darkcyan"))+  
  scale_colour_manual(values = c("slategrey","darkorange3", "purple4", "goldenrod4", "forestgreen", "deeppink4", "darkcyan")) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.y = element_markdown(size = 14,)) +
  stat_summary(fun="mean", geom="point", size=2, color="black", show.legend = FALSE) +
  ylab("Mean precipitation of the warmest quarter <br>(kg m<sup>-2</sup>)")    +
  scale_y_continuous(breaks=seq(0,800, by=100), expand= c(0,0)) +
  coord_cartesian(ylim=c(0,800)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin=unit(c(1,-0.05,0.25,1), "cm")) +
  theme(axis.text.y = element_text(size = 12,)) +
  theme(legend.text = element_text(face = "italic", size =12)) +
  theme(legend.key.size = unit(0.6, "cm")) +
  theme(legend.position = c(0.7, 0.9)) +
  theme(plot.margin=unit(c(1,-0.05,0.25,1), "cm")) 


bio.18 <- bio.18  + geom_violin(trim = FALSE) + 
  stat_summary(
    fun.data = "mean_sdl",  fun.args = list(mult = 1), 
    geom = "pointrange", color = "black"
  )




boxplots.with.Lp <- rbind(c(1,2))


grid.arrange(bio.16, bio.18, ncol = 2, nrow = 1, layout_matrix = boxplots.with.Lp)






boxplots.summary <- scotland_new_biolayers %>%
  group_by(Species) %>%
  summarise(
    mean= mean(Bio16))
boxplots.summary



boxplots.summary.Bio18 <- scotland_new_biolayers %>%
  group_by(Species) %>%
  summarise(
    mean= mean(Bio18))
boxplots.summary.Bio18











######################################################################
###############################################################################################################################################
###############################################################################################
#################### ordered in specialism ##############################

bio.18.new <- ggplot(data = scotland_new_biolayers, aes(x =Species, y = Bio18, colour = Species, fill = Species)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.9, outlier.shape = NA, show.legend = FALSE) + 
  scale_fill_manual(values = c( "slategrey","darkorange3", "purple4", "goldenrod4", "forestgreen", "deeppink4", "darkcyan"))+  
  scale_colour_manual(values = c("slategrey","darkorange3", "purple4", "goldenrod4", "forestgreen", "deeppink4", "darkcyan")) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.y = element_markdown(size = 28,)) +
  stat_summary(fun="mean", geom="point", size=2, color="black", show.legend = FALSE) +
  ylab("Mean precipitation of the warmest quarter <br>(kg m<sup>-2</sup> )")    +
  scale_y_continuous(breaks=seq(0,800, by=100), expand= c(0,0)) +
  coord_cartesian(ylim=c(0,800)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin=unit(c(1,-0.05,0.25,1), "cm")) +
  theme(legend.title = element_text( size = 28))+
  theme(legend.text = element_text(face = "italic", size =22)) +
  theme(legend.key.size = unit(0.7, "cm")) +
  theme(legend.position = c(0.7, 0.95)) +
  theme(plot.margin=unit(c(1,-0.05,0.25,1), "cm")) +
  theme(axis.text.y = element_text(size =20)) +
  guides(colour = guide_legend(override.aes = list(shape = NA))) +
  theme(legend.spacing.y = unit(0.1, "cm")) +
  theme(legend.spacing.x = unit(0.5, "cm")) +
  guides(fill = guide_legend(byrow = TRUE, ncol =2)) 

#####################################################################################
########################################################################

bio.16.new <- ggplot(data = scotland_new_biolayers, aes(x = Species, y = Bio16, colour = Species, fill = Species)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.9, outlier.shape = NA, show.legend = FALSE) + 
  scale_fill_manual(values = c( "slategrey","darkorange3", "purple4", "goldenrod4", "forestgreen", "deeppink4", "darkcyan"))+  
  scale_colour_manual(values = c("slategrey","darkorange3", "purple4", "goldenrod4", "forestgreen", "deeppink4", "darkcyan")) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.y = element_markdown(size = 28,)) +
  stat_summary(fun="mean", geom="point", size=2, color="black", show.legend = FALSE) +
  ylab("Mean precipitation of the wettest quarter <br>(kg m<sup>-2</sup> )")    +
  scale_y_continuous(breaks=seq(0,1200, by=200), expand= c(0,0)) +
  coord_cartesian(ylim=c(0,1200)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin=unit(c(1,-0.05,0.25,1), "cm")) +
  theme(axis.text.y = element_text(size =18)) +
  guides(colour = guide_legend(override.aes = list(shape = NA)))





boxplots.no.Lp <- rbind(c(1,2))

grid.arrange(bio.16.new, bio.18.new, ncol = 2, nrow = 1, layout_matrix = boxplots.no.Lp)




boxplots.no.Lp1 <- rbind(c(1,1))

boxplots.no.Lp2 <- rbind(c(1,1))


grid.arrange(bio.16.new, ncol = 1, nrow = 1, layout_matrix = boxplots.no.Lp1)

grid.arrange(bio.18.new, ncol = 2, nrow = 1, layout_matrix = boxplots.no.Lp2)
















########### me checking data #########

Climate_Data_UK_All_Species_new_only$Species <- factor(Climate_Data_UK_All_Species_new_only$Species, levels = c("Lobaria pulmonaria", "Ramalina calicaris", "Sticta limbata", "Sticta sylvatica", "Ricasolia virens", "Hypotrachyna laevigata", "Pectenia atlantica"))


boxplots.summary.Bio18 <- Climate_Data_UK_All_Species_new_only %>%
  group_by(Species) %>%
  summarise(
    mean= mean(Bio_18))
boxplots.summary.Bio18


boxplots.summary_bio_16 <- Climate_Data_UK_All_Species_new_only %>%
  group_by(Species) %>%
  summarise(
    mean= mean(Bio_16))
boxplots.summary_bio_16







bio.18.new <- ggplot(data = Climate_Data_UK_All_Species_new_only, aes(x =Species, y = Bio_18, colour = Species, fill = Species)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.9, outlier.shape = NA, show.legend = FALSE) + 
  scale_fill_manual(values = c( "slategrey","darkorange3", "purple4", "goldenrod4", "forestgreen", "deeppink4", "darkcyan"))+  
  scale_colour_manual(values = c("slategrey","darkorange3", "purple4", "goldenrod4", "forestgreen", "deeppink4", "darkcyan")) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.y = element_markdown(size = 28,)) +
  stat_summary(fun="mean", geom="point", size=2, color="black", show.legend = FALSE) +
  ylab("Mean precipitation of the warmest quarter <br>(kg m<sup>-2</sup> )")    +
  scale_y_continuous(breaks=seq(0,800, by=100), expand= c(0,0)) +
  coord_cartesian(ylim=c(0,800)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin=unit(c(1,-0.05,0.25,1), "cm")) +
  theme(legend.title = element_text( size = 28))+
  theme(legend.text = element_text(face = "italic", size =22)) +
  theme(legend.key.size = unit(0.7, "cm")) +
  theme(legend.position = c(0.7, 0.95)) +
  theme(plot.margin=unit(c(1,-0.05,0.25,1), "cm")) +
  theme(axis.text.y = element_text(size =20)) +
  guides(colour = guide_legend(override.aes = list(shape = NA))) +
  theme(legend.spacing.y = unit(0.1, "cm")) +
  theme(legend.spacing.x = unit(0.5, "cm")) +
  guides(fill = guide_legend(byrow = TRUE, ncol =2)) 

#####################################################################################
########################################################################

bio.16.new <- ggplot(data = Climate_Data_UK_All_Species_new_only, aes(x = Species, y = Bio_16, colour = Species, fill = Species)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.9, outlier.shape = NA, show.legend = FALSE) + 
  scale_fill_manual(values = c( "slategrey","darkorange3", "purple4", "goldenrod4", "forestgreen", "deeppink4", "darkcyan"))+  
  scale_colour_manual(values = c("slategrey","darkorange3", "purple4", "goldenrod4", "forestgreen", "deeppink4", "darkcyan")) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.text = element_text(face = "italic")) +
  theme(axis.title.y = element_markdown(size = 28,)) +
  stat_summary(fun="mean", geom="point", size=2, color="black", show.legend = FALSE) +
  ylab("Mean precipitation of the wettest quarter <br>(kg m<sup>-2</sup> )")    +
  scale_y_continuous(breaks=seq(0,1200, by=200), expand= c(0,0)) +
  coord_cartesian(ylim=c(0,1200)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = "none") +
  theme(plot.margin=unit(c(1,-0.05,0.25,1), "cm")) +
  theme(axis.text.y = element_text(size =18)) +
  guides(colour = guide_legend(override.aes = list(shape = NA)))


















