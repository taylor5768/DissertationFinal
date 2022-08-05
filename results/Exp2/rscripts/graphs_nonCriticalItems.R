#graphs for non-critical items in Exp 1
#based on Judith Tonhauser's code


# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load required packages
require(tidyverse)
library(ggrepel)
library(dichromat)
library(forcats)
library(ggrepel)
library(RColorBrewer)
library(Hmisc)



# load helper functions
source('../../helpers.R')

cd = read.csv("../data/data_preprocessed-test.csv")
cd$orientation<-as.factor(cd$orientation)
cd$socialInfo<-as.factor(cd$socialInfo)
cd$socialInfo<-relevel(cd$socialInfo,ref="N")

length(unique(cd$workerid))


#########################################
###Block 3-response to It Is T/F items###
########################################
d.ItIsT <- cd %>%
  filter(true_false == "TRUE" & block3 == "block3")%>%
  droplevels()

d.ItIsF <- cd %>%
  filter(true_false == "FALSE" & block3 == "block3") %>%
  droplevels()


## group mean for "it is true" item
round(mean(d.ItIsT$projection),2) #.88

ItIsT_scatterplot <- ggplot(d.ItIsT, aes(x=workerid,y=projection)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("responses to 'it is true' item in block 3")

ItIsT_scatterplot
#ggsave(ItIsT_scatterplot,filename="../graphs/Non-critical_items/Block3_itIsTrue_scatterplot.png")



ItIsT_bargraph<-ggplot(subset(cd,(true_false == "TRUE" & block3 == "block3")), aes(x=orientation,y=projection,fill=socialInfo),xpd=FALSE) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Democrat speaker","Republican speaker"),values=c("#56B4E9", "tomato1"))+
  xlab("CC orientation") +
  ylab("Mean certainty rating") + 
  scale_x_discrete(labels=c("conservative", "liberal","neutral")) +
  ylim(0,1)+
  theme(legend.position = "right",aspect.ratio=1,axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18),legend.margin = margin(t=-.1,l=.1,r=.1,b=.1,unit='cm'))+
  ggtitle("Responses to 'It is true' item")


ItIsT_bargraph
ggsave(ItIsT_bargraph,filename="../graphs/Non-critical_items/Block3_itIsTrue_bargraph.png")



#it is False
ItIsF_scatterplot <-ggplot(d.ItIsF, aes(x=workerid,y=projection)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("responses to 'it is false' item in block 3")


ItIsF_scatterplot
#ggsave(ItIsF_scatterplot,filename="../graphs/Non-critical_items/Block3_itIsFalse_scatterplot.png")

ItIsF_bargraph<-ggplot(subset(cd,(true_false == "FALSE" & block3 == "block3")), aes(x=orientation,y=projection,fill=socialInfo),xpd=FALSE) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Democrat speaker","Republican speaker"),values=c("#56B4E9", "tomato1"))+
  xlab("CC orientation") +
  ylab("Mean certainty rating") + 
  scale_x_discrete(labels=c("conservative", "liberal","neutral")) +
  ylim(0,1)+
  theme(legend.position = c(.5,.85),aspect.ratio=1,axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18),legend.margin = margin(t=-.1,l=.1,r=.1,b=.1,unit='cm'))+
  ggtitle("Responses to 'It is false' item")

ItIsF_bargraph
#ggsave(ItIsF_bargraph,filename="../graphs/Non-critical_items/Block3_itIsFalse_bargraph.png")


##look at relationship b/t responses to the true/false CCs in block 2 ('speaker beliefs') and block 3 projection ratings)

#welfare, liberal
d.ItIsT.welfare <- cd %>%
  filter(true_false == "TRUE" & block3 == "block3" & topic =="welfare")%>%
  droplevels()

welfare_liberal <- ggplot(subset(cd,(topic=="welfare" & orientation=="liberal")), aes(x=spBelief_rating, y=projection)) +
  geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=.3) +
  xlab("Speaker belief rating") +
  ylab("Certainty rating") +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1)+
  ggtitle("It is True vs. False/welfare/liberal")+
  facet_wrap(~true_false+socialInfo)

welfare_liberal
#ggsave(welfare_liberal,filename="../graphs/Non-critical_items/Projection~Beliefs_ItIsTrueFalse_Welfare_Liberal.png")


#welfare, conservative
welfare_conservative <- ggplot(subset(cd,(topic=="welfare" & orientation=="conservative")), aes(x=spBelief_rating, y=projection)) +
  geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=.3) +
  xlab("Speaker belief rating") +
  ylab("Certainty rating") +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1)+
  ggtitle("It is True vs. False/welfare/conservative")+
  facet_wrap(~true_false+socialInfo)

welfare_conservative
#ggsave(welfare_conservative,filename="../graphs/Non-critical_items/Projection~Beliefs_ItIsTrueFalse_Welfare_Conservative.png")


environment_conservative <- ggplot(subset(cd,(topic=="environment" & orientation=="conservative")), aes(x=spBelief_rating, y=projection)) +
  geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=.3) +
  xlab("Speaker belief rating") +
  ylab("Certainty rating") +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1)+
  ggtitle("It is True vs. False/environment/conservative")+
  facet_wrap(~true_false+socialInfo)

environment_conservative
#ggsave(environment_conservative,filename="../graphs/Non-critical_items/Projection~Beliefs_ItIsTrueFalse_Environment_Conservative.png")




environment_liberal <- ggplot(subset(cd,(topic=="environment" & orientation=="liberal")), aes(x=spBelief_rating, y=projection)) +
  geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=.3) +
  xlab("Speaker belief rating") +
  ylab("Certainty rating") +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1)+
  ggtitle("It is True vs. False/environment/liberal")+
  facet_wrap(~true_false+socialInfo)

environment_liberal
#ggsave(environment_liberal,filename="../graphs/Non-critical_items/Projection~Beliefs_ItIsTrueFalse_Environment_Liberal.png")

