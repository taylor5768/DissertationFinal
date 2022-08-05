#primary graphs for Exp. 1
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
library(ggpubr)



# load helper functions
source('../../helpers.R')

cd = read.csv("../data/data_preprocessed.csv")
cd$orientation<-as.factor(cd$orientation)

length(unique(cd$workerid))




########################################################################
###get an overall sense of the projection responses for all critical items
projection_socialInfo_criticalItems<-ggplot(subset(cd,item_type=="critical"), aes(x=orientation,y=projection,fill=socialInfo),xpd=FALSE) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Democrat speaker","Republican speaker"),values=c("#56B4E9", "tomato1"))+
  xlab("CC orientation") +
  ylab("Mean certainty rating") + 
  scale_x_discrete(labels=c("conservative", "liberal","neutral")) +
  ylim(0,1)+
  theme(legend.position = c(.5,.85),aspect.ratio=1,axis.text=element_text(size=14),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18),legend.margin = margin(t=-.1,l=.1,r=.1,b=.1,unit='cm'))

projection_socialInfo_criticalItems

#ggsave(projection_socialInfo_criticalItems,filename="../graphs/projection~SocInfo_CriticalItems.png")


################################################################################
####H.1.1a: is projection of neutral CCs uninfluenced by speaker political orientation?################################################
##############################################################################

#overall
projection_socialInfo_neutrals<-ggplot(subset(cd,(item_type=="critical"& orientation=="N")), aes(x=orientation,y=projection,fill=socialInfo),xpd=FALSE) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  #stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", size=4, vjust = -0.5)+
  scale_fill_manual(labels=c("Democrat speaker","Republican speaker"),values=c("#56B4E9", "tomato1"))+
  xlab("Orientation of CC") +
  ylab("Mean certainty rating") + 
  ylim(0,1)+
  theme(aspect.ratio=1,axis.text.x=element_blank(),axis.title.x=element_blank(),legend.title=element_blank())
  theme(legend.position = "bottom",aspect.ratio=1,axis.text.x=element_blank(),axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"),legend.title=element_blank(),legend.text=element_text(size=12),
        #legend.margin = margin(t=-.05,l=.05,r=.05,b=.05,unit='cm')
        )+
  xlab("")



projection_socialInfo_neutrals
ggsave(projection_socialInfo_neutrals,filename="../graphs/Neutrals/projection~socialInfo_neutrals.png")

#by topic
projection_socialInfo_neutral_byTopic<-projection_socialInfo_neutrals+
  facet_wrap(~topic,nrow=3)+
  xlab("CC orientation")+
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0","0.5","1"))+
  theme(legend.position = "bottom",aspect.ratio=1,axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=12),axis.text.x=element_blank())+ xlab("Neutral items")
  
  
projection_socialInfo_neutral_byTopic
#ggsave(projection_socialInfo_neutral_byTopic,filename="../graphs/Neutrals/projection~socialInfo_byTopic_neutrals.png")



#by predicate
projection_socialInfo_neutral_byPredicate <- projection_socialInfo_neutral_byTopic+facet_wrap(~predicate,nrow=3)

projection_socialInfo_neutral_byPredicate
#ggsave(projection_socialInfo_neutral_byPredicate,filename="../graphs/Neutrals/projection~socialInfo_byPredicate_neutrals.png")

#by predicate, but organized all together
projection_socialInfo_neutral_byPredicate_together<-ggplot(subset(cd,(orientation=="N")), aes(x=reorder(predicate,projection),y=projection,fill=socialInfo),xpd=FALSE) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Democrat","Republican"),values=c("#56B4E9","#D55E00"))+
  xlab("Predicate")+
  ylab("Mean certainty rating")+
  ylim(0,1)+
  labs(fill="Social info")+
  theme(axis.text.x = element_text(size=12,angle = 45, hjust=1))+
  ggtitle("Neutral items")

projection_socialInfo_neutral_byPredicate_together
#ggsave(projection_socialInfo_neutral_byPredicate_together,filename="../graphs/Neutrals/projection~socialInfo_byPredicate_together_neutrals.png")


projection_neutral_byPredicate <-ggplot(subset(cd,(orientation=="N" & item_type=="critical")), aes(x=reorder(predicate,projection),y=projection),xpd=FALSE) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Democrat","Republican"),values=c("#56B4E9","#D55E00"))+
  xlab("Predicate")+
  ylab("Mean certainty rating")+
  ylim(0,1)+
  labs(fill="Social info")+
  theme(axis.text.x = element_text(size=12,angle = 45, hjust=1))+
  ggtitle("Neutral items")

projection_neutral_byPredicate
ggsave(projection_neutral_byPredicate,filename="../graphs/Neutrals/projection~byPredicate_neutrals.png")

#like Judith & Judith's graph

##Like Degen & Tonhauser's graphs in prior paper####
#add another variable just for plotting purposes
cd$Party[cd$socialInfo == "D"] <- "Democrat"
cd$Party[cd$socialInfo == "R"] <- "Republican"
cd$Party<-as.factor(cd$Party)
d.neutralCCs <- subset(cd,orientation == "N" & item_type=="critical")

proj.means = d.neutralCCs %>%
  group_by(predicate,Party) %>%
  summarise(Mean = mean(projection), CILow = ci.low(projection), CIHigh = ci.high(projection)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(predicate),Mean))  
proj.means

# order predicates by socialInfo
high = proj.means %>%
  filter(Party != "Democrat") %>%
  mutate(predicate = fct_reorder(predicate,Mean))
high
levels(high$predicate)

proj.means = proj.means %>%
  mutate(predicate = fct_relevel(predicate,levels(high$predicate)))
levels(proj.means$predicate)

# change factor levels for Party for plotting
proj.means = proj.means %>%
  mutate(Party = fct_relevel(Party, "Democrat", "Republican"))
levels(proj.means$Party)

# to add participants' ratings
subjmeans = d.neutralCCs %>%
  group_by(workerid,predicate,Party) %>%
  summarise(Mean = mean(projection))
subjmeans$predicate <- factor(subjmeans$predicate, levels = unique(levels(proj.means$predicate)))
levels(subjmeans$predicate)
subjmeans
subjmeans$Party<- as.factor(subjmeans$Party)

# change factor levels for prior_type for plotting
subjmeans = subjmeans %>%
  mutate(Party = fct_relevel(Party,"Democrat", "Republican"))
levels(subjmeans$Party)

levels(proj.means$Party)
#[1] "D" "R"

ggplot(proj.means, aes(x=predicate, y=Mean, color=Party,fill=Party,shape=Party)) + 
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  geom_point(data=subjmeans,aes(fill=Party,color=Party),shape=21,alpha=.08) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_point(size = 3,color="black") +
  scale_shape_manual(values=rev(c(24, 25))) +
  scale_fill_manual(values=rev(c("tomato","#56B4E9"))) +
  scale_color_manual(values=rev(c("tomato","#56B4E9"))) +  
  scale_alpha(range = c(.3,1)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,.2,.4,.6,.8,1), labels=c("0", ".2", ".4", ".6", ".8", "1")) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  ylab("Mean certainty rating") +
  xlab("Predicate")+
  theme(legend.title = element_text(face="bold"))

ggsave("../graphs/neutralCCs-projectivity-by-predicate-and-party.png",height=5,width=7)





################################################################################
###H.1.1b: Are projection ratings higher for conservative CCs w/Republican speakers vs. Democrat speakers and vice versa for liberal CCs#####
###############################################################################




#overall
projection_socialInfo_political<-
  ggplot(subset(cd,(orientation!="N" & item_type=="critical")), aes(x=orientation,y=projection,fill=socialInfo),xpd=FALSE) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  #stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", size=4, vjust = -0.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Democrat speaker","Republican speaker"),values=c("#56B4E9","#D55E00"))+
  xlab("CC orientation")+
  ylab("Mean certainty rating")+
  theme(legend.position="left")+
  labs(fill="Social Info")+
  ylim(0,1)+
  theme(legend.position = c(.5,.85),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18),aspect.ratio=1)#+
  #annotate("text", x = 1:1.8, y = .68, label = "*",size=10)+
  #annotate("text", x = 2.05:2.8, y = .68, label = "*",size=10)


projection_socialInfo_political
ggsave(projection_socialInfo_political,filename="../graphs/projection~socialInfo_Political.png",width=5,height=5)


#by topic/individual CC
projection_socialInfo_political_byTopic<-projection_socialInfo_political+
  facet_wrap(~topic,nrow=3)+
  scale_x_discrete(labels=c("C", "L"))+
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0","0.5","1"))+
  theme(legend.position = "bottom",aspect.ratio=1,axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18),axis.text=element_text(size=12))

projection_socialInfo_political_byTopic
#ggsave(projection_socialInfo_political_byTopic,filename="../graphs/projection~socialInfo_Political_byTopic.png")


#by predicate
projection_socialInfo_political_byPredicate<-projection_socialInfo_political_byTopic+
  facet_wrap(~predicate,nrow=3)

projection_socialInfo_political_byPredicate
#ggsave(projection_socialInfo_political_byTopic,filename="../graphs/projection~socialInfo_Political_byPredicate.png")

#by predicate - all together (this one still needs labels for conservative/liberal contents)
projection_socialInfo_political_byPredicate_together<-ggplot(subset(cd,orientation!="N" & item_type=="critical"), aes(x=reorder(predicate,projection),y=projection,group=interaction(socialInfo,orientation),fill=socialInfo) )+
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Democrat speaker","Republican speaker"),values=c("#56B4E9","#D55E00"))+
  xlab("Predicate")+
  ylab("Mean certainty rating")+
  labs(fill="Social info")+
  theme(legend.position = c(.23,.87),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18),axis.text.x = element_text(size=12,angle = 45, hjust=1))+
  scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1))
  
projection_socialInfo_political_byPredicate_together
#ggsave(projection_socialInfo_political_byPredicate_together,filename="../graphs/projection~socialInfo_Political_byPredicate_together.png")

projection_socialInfo_political_byPredicate_bare<-ggplot(subset(cd,orientation!="N" & item_type=="critical"), aes(x=reorder(predicate,projection),y=projection))+
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Democrat speaker","Republican speaker"),values=c("#56B4E9","#D55E00"))+
  xlab("Predicate")+
  ylab("Mean certainty rating")+
  labs(fill="Social info")+
  theme(legend.position = c(.23,.87),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18),axis.text.x = element_text(size=12,angle = 45, hjust=1))+
  scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1))

projection_socialInfo_political_byPredicate_bare

ggsave(projection_socialInfo_political_byPredicate_together,filename="../graphs/projection~socialInfo_Political_byPredicate_bare.png",width=7,height=5)




#conservative CCs
d.conservativeCCs <- subset(cd,orientation == "conservative" & item_type=="critical")

proj.means = d.conservativeCCs %>%
  group_by(predicate,Party) %>%
  summarise(Mean = mean(projection), CILow = ci.low(projection), CIHigh = ci.high(projection)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(predicate),Mean))  
proj.means

# order predicates by socialInfo
high = proj.means %>%
  filter(Party != "Democrat") %>%
  mutate(predicate = fct_reorder(predicate,Mean))
high
levels(high$predicate)

proj.means = proj.means %>%
  mutate(predicate = fct_relevel(predicate,levels(high$predicate)))
levels(proj.means$predicate)

# change factor levels for Party for plotting
proj.means = proj.means %>%
  mutate(Party = fct_relevel(Party, "Democrat", "Republican"))
levels(proj.means$Party)

# to add participants' ratings
subjmeans = d.conservativeCCs %>%
  group_by(workerid,predicate,Party) %>%
  summarise(Mean = mean(projection))
subjmeans$predicate <- factor(subjmeans$predicate, levels = unique(levels(proj.means$predicate)))
levels(subjmeans$predicate)
subjmeans
subjmeans$Party<- as.factor(subjmeans$Party)

# change factor levels for prior_type for plotting
subjmeans = subjmeans %>%
  mutate(Party = fct_relevel(Party,"Democrat", "Republican"))
levels(subjmeans$Party)

levels(proj.means$Party)
#[1] "D" "R"

ggplot(proj.means, aes(x=predicate, y=Mean, color=Party,fill=Party,shape=Party)) + 
  theme(legend.position = "bottom", legend.text=element_text(size=12)) +
  geom_point(data=subjmeans,aes(fill=Party,color=Party),shape=21,alpha=.08) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_point(size = 3,color="black") +
  scale_shape_manual(values=rev(c(24, 25))) +
  scale_fill_manual(values=rev(c("tomato","#56B4E9"))) +
  scale_color_manual(values=rev(c("tomato","#56B4E9"))) +  
  scale_alpha(range = c(.3,1)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,.2,.4,.6,.8,1), labels=c("0", ".2", ".4", ".6", ".8", "1")) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  ylab("Mean certainty rating") +
  xlab("Predicate")+
  theme(legend.title = element_text(face="bold"))+
  ggtitle("conservative CCs")

ggsave("../graphs/conservativeCCs-projectivity-by-predicate-and-party.png",height=5,width=7)


#liberal CCs
d.liberalCCs <- subset(cd,orientation == "liberal" & item_type=="critical")

proj.means = d.liberalCCs %>%
  group_by(predicate,Party) %>%
  summarise(Mean = mean(projection), CILow = ci.low(projection), CIHigh = ci.high(projection)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(predicate),Mean))  
proj.means

# order predicates by socialInfo
high = proj.means %>%
  filter(Party != "Democrat") %>%
  mutate(predicate = fct_reorder(predicate,Mean))
high
levels(high$predicate)

proj.means = proj.means %>%
  mutate(predicate = fct_relevel(predicate,levels(high$predicate)))
levels(proj.means$predicate)

# change factor levels for Party for plotting
proj.means = proj.means %>%
  mutate(Party = fct_relevel(Party, "Democrat", "Republican"))
levels(proj.means$Party)

# to add participants' ratings
subjmeans = d.liberalCCs %>%
  group_by(workerid,predicate,Party) %>%
  summarise(Mean = mean(projection))
subjmeans$predicate <- factor(subjmeans$predicate, levels = unique(levels(proj.means$predicate)))
levels(subjmeans$predicate)
subjmeans
subjmeans$Party<- as.factor(subjmeans$Party)

# change factor levels for prior_type for plotting
subjmeans = subjmeans %>%
  mutate(Party = fct_relevel(Party,"Democrat", "Republican"))
levels(subjmeans$Party)

levels(proj.means$Party)
#[1] "D" "R"

ggplot(proj.means, aes(x=predicate, y=Mean, color=Party,fill=Party,shape=Party)) + 
  theme(legend.position = "none", legend.text=element_text(size=12)) +
  geom_point(data=subjmeans,aes(fill=Party,color=Party),shape=21,alpha=.08) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_point(size = 3,color="black") +
  scale_shape_manual(values=rev(c(24, 25))) +
  scale_fill_manual(values=rev(c("tomato","#56B4E9"))) +
  scale_color_manual(values=rev(c("tomato","#56B4E9"))) +  
  scale_alpha(range = c(.3,1)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,.2,.4,.6,.8,1), labels=c("0", ".2", ".4", ".6", ".8", "1")) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  ylab("Mean certainty rating") +
  xlab("Predicate")+
  theme(legend.title = element_text(face="bold"))+
  ggtitle("liberal CCs")

ggsave("../graphs/liberalCCs-projectivity-by-predicate-and-party.png",height=5,width=7)

#all CCs same graph
allCCs = cd %>% 
  filter(item_type == "critical" &orientation!="N") %>% 
  droplevels()
nrow(allCCs) 


proj.means = allCCs %>%
  group_by(predicate,socialInfo,orientation) %>%
  summarise(Mean = mean(projection), CILow = ci.low(projection), CIHigh = ci.high(projection)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, predicate = fct_reorder(as.factor(predicate),Mean))  
proj.means
#View(proj.means)

nrow(proj.means) #24

high = proj.means %>%
  filter(socialInfo == "S") %>%
  mutate(predicate = fct_reorder(predicate,Mean))


allCCs = allCCs %>%
  mutate(predicate = fct_relevel(predicate,levels(high$predicate)))
table(allCCs$predicate)


# change factor levels for prior_type for plotting
allCCs = allCCs %>%
  mutate(socialInfo= fct_relevel(socialInfo, "D", "R"))
levels(allCCs$socialInfo)

ggplot(allCCs) +
  geom_smooth(method="lm",aes(x=spBelief_rating,y=projection,linetype=orientation),color="black") +
  scale_linetype_manual(values=c("twodash", "solid"))+
  
  # geom_smooth(data=subset(allCCs,orientation=="conservative"),method="lm",aes(x=spBelief_rating,y=projection)) +
  # geom_smooth(data=subset(allCCs,orientation=="liberal"),method="lm",aes(x=spBelief_rating,y=projection),color="chartreuse4") +
  geom_point(shape=20, size=1, alpha=.3,aes(x=spBelief_rating,y=projection,color=socialInfo)) +
  scale_color_manual(values=c("#56B4E9","tomato"),labels=c("Democrat","Republican"),name=c("Speaker affiliation")) +
  xlab("Perceived speaker belief rating") +
  ylab("Certainty rating") +
  theme(legend.position = "bottom", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  #xlim(0,1) +
  #ylim(0,1) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  theme(panel.spacing.x = unit(4, "mm")) +
  #coord_fixed(ratio = 1) +
  facet_wrap(~predicate)
ggsave(f="../graphs/projection-by-prior-all.png",height=7,width=8)



##collpased over socialInfo & orientation
d.allCriticalCCs <- subset(cd,orientation != "neutral" & item_type=="critical")

proj.means = d.allCriticalCCs %>%
  group_by(predicate) %>%
  summarise(Mean = mean(projection), CILow = ci.low(projection), CIHigh = ci.high(projection)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, verb = fct_reorder(as.factor(predicate),Mean))  
proj.means

# order predicates by socialInfo
high = proj.means %>%
  #filter(Party != "Democrat") %>%
  mutate(predicate = fct_reorder(predicate,Mean))
high
levels(high$predicate)

proj.means = proj.means %>%
  mutate(predicate = fct_relevel(predicate,levels(high$predicate)))
levels(proj.means$predicate)

# change factor levels for Party for plotting
#proj.means = proj.means %>%
#  mutate(Party = fct_relevel(Party, "Democrat", "Republican"))
#levels(proj.means$Party)

# to add participants' ratings
subjmeans = d.allCriticalCCs %>%
  group_by(workerid,predicate) %>%
  summarise(Mean = mean(projection))
subjmeans$predicate <- factor(subjmeans$predicate, levels = unique(levels(proj.means$predicate)))
levels(subjmeans$predicate)
subjmeans
#subjmeans$Party<- as.factor(subjmeans$Party)

# change factor levels for prior_type for plotting
#subjmeans = subjmeans %>%
#  mutate(Party = fct_relevel(Party,"Democrat", "Republican"))
#levels(subjmeans$Party)

#levels(proj.means$Party)
#[1] "D" "R"

ggplot(proj.means, aes(x=predicate, y=Mean)) + 
  theme(legend.position = "none", legend.text=element_text(size=12)) +
  geom_point(data=subjmeans,shape=21,alpha=.08) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_point(size = 3,color="black") +
  scale_shape_manual(values=rev(c(24, 25))) +
  scale_fill_manual(values=rev(c("tomato","#56B4E9"))) +
  scale_color_manual(values=rev(c("tomato","#56B4E9"))) +  
  scale_alpha(range = c(.3,1)) +
  scale_y_continuous(limits = c(0,1),breaks = c(0,.2,.4,.6,.8,1), labels=c("0", ".2", ".4", ".6", ".8", "1")) +
  theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 45, hjust = 1)) +
  ylab("Mean certainty rating") +
  xlab("Predicate")+
  theme(legend.title = element_text(face="bold"))+
  ggtitle("political CCs")

ggsave("../graphs/allCriticalCCs-projectivity-by-predicate.png",height=5,width=7)



#########################################################################
#####H.1.2.a is there a correlation b/t participant beliefs & projection?
#########################################################################

#plot projection by participant beliefs
projection_partBeliefs_overall<-projection_partBeliefs_overall <- ggplot(subset(cd,(participant_beliefs!="NA" & projection!="NA" & item_type=="critical")), aes(x=participant_beliefs, y=projection)) +
  geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=.3) +
  xlab("Participant CC belief rating") +
  ylab("Certainty rating") +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) 

projection_partBeliefs_overall
#ggsave(projection_partBeliefs_overall,filename="../graphs/ParticipantBeliefRatings/projection~partBeliefs_overall.png")

#plot projection by participant beliefs, grouped by predicate
projection_partBeliefs_byPredicate<-projection_partBeliefs_overall+
  facet_wrap(~predicate,nrow=3)

#ggsave(projection_partBeliefs_byPredicate,filename="../graphs/ParticipantBeliefRatings/projection~partBeliefs_byPredicate.png")



#by topic
projection_partBeliefs_byTopic<-projection_partBeliefs_byPredicate+
  facet_wrap(~topic,nrow=3)

projection_partBeliefs_byTopic

#ggsave(projection_partBeliefs_byTopic,filename="../graphs/participantBeliefRatings/projection~partBeliefs_byTopic.png")




#####################################################################################
####H.1.2.b: are certainty ratings higher for conserv. CCs w/Rep. participants etc.?
#####################################################################################
unique(cd$party)

RepublicanParticipants = subset(cd,party=="Rep")
length(unique(RepublicanParticipants$workerid)) #36

DemocratParticipants = subset(cd,party=="Dem")
length(unique(DemocratParticipants$workerid)) #137

Projection_socialInfo_partParty<-ggplot(subset(cd,(item_type=="critical" & orientation!="N" & (party=="Dem"|party=="Rep"))), aes(x=orientation,y=projection,fill=party),xpd=FALSE) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Democrat participant","Republican participant"),values=c("#56B4E9","#D55E00"))+
  xlab("CC orientation")+
  ylab("Mean certainty rating")+
  theme(legend.position = c(.25,.85),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1))+
  coord_fixed(ratio=1)

Projection_socialInfo_partParty
ggsave(Projection_socialInfo_partParty,filename="../graphs/ParticipantParty_Ideology/projection~socialInfo+participantParty.png")



#############################H.1.2.b:##############################################
##are certainty ratings higher w/conserv.CCs w/more right-leaning participants etc?##
#################################################################################

Projection_socialInfo_partPolitics <- ggplot(subset(cd,(projection!="NA" & item_type=="critical"& orientation!="N")), aes(x=politics, y=projection)) +
  geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=.3) +
  xlab("Participant politics (1=most right-leaning)") +
  ylab("Certainty rating") +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1)+
  facet_wrap(~orientation)

Projection_socialInfo_partPolitics

Projection_socialInfo_partPolitics
#ggsave(Projection_socialInfo_partPolitics,filename="../graphs/ParticipantParty_Ideology/projection~socialInfo+participantPolitics.png")


#############################H.1.3a#####################################
###are speaker belief ratings higher for conservative CCs w/Republican sps;###
#########vice versa for liberal?#######################################

spBelief_socialInfo_overall <- ggplot(subset(cd, (orientation!="N" & item_type=="critical")), aes(x=orientation,y=spBelief_rating,fill=socialInfo)) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Democrat speaker","Republican speaker"),values=c("#56B4E9","#D55E00"))+
  xlab("CC orientation")+
  ylab("Mean speaker Belief rating")+
  scale_x_discrete(labels=c("C", "L")) +
  theme(legend.position = "bottom",axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1))+
  coord_fixed(ratio = 1)

spBelief_socialInfo_overall 
#ggsave(spBelief_socialInfo_overall ,filename="../graphs/speakerBeliefRatings/spBelief~socialInfo.png")

#facet by topic
spBelief_socialInfo_byTopic<-spBelief_socialInfo_overall+
  facet_wrap(~topic, nrow=3)

spBelief_socialInfo_byTopic
#ggsave(spBelief_socialInfo_byTopic ,filename="../graphs/speakerBeliefRatings/spBelief~socialInfo_byTopic.png")


#like Judith & Judith figure 2
#convervative CCs
conservativeCCs = cd %>% 
  filter(item_type == "critical" & orientation =="conservative") %>% 
  droplevels()
nrow(conservativeCCs) 


means = conservativeCCs %>%
  group_by(socialInfo,topic) %>%
  summarise(Mean=mean(spBelief_rating),CILow=ci.low(spBelief_rating),CIHigh=ci.high(spBelief_rating)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
means


high = means %>%
  filter(socialInfo == "R") %>%
  mutate(eventItem = fct_reorder(topic,Mean))

means = means %>%
  mutate(topic = fct_relevel(topic,levels(high$topic))) %>% 
  mutate(socialInfo = fct_relevel(socialInfo,"D"))
means

subjmeans = conservativeCCs %>%
  group_by(topic,workerid,socialInfo) %>%
  summarise(Mean = mean(spBelief_rating)) %>% 
  ungroup() %>% 
  mutate(socialInfo = fct_relevel(as.factor(as.character(socialInfo)),"D"))
subjmeans$topic <- factor(subjmeans$topic, levels = unique(levels(means$topic)))
levels(subjmeans$topic)
names(subjmeans)

ggplot(means, aes(x=topic, y=Mean, color=socialInfo,shape=socialInfo,fill=socialInfo)) + 
  geom_point(data=subjmeans,aes(fill=socialInfo,color=socialInfo),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_shape_manual(values=c(25, 24),labels=c("Democrat","Republican"),name="Speaker affiliation:") +
  scale_fill_manual(values=c("#56B4E9","tomato"),labels=c("Democrat","Republican"),name="Speaker affiliation:") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,.2,.4,.6,.8,1), labels=c("0", ".2", ".4", ".6", ".8", "1")) +
  scale_color_manual(name="Speaker affiliation:",labels=c("Democrat", "Republican"), values=c("#56B4E9","tomato")) +
  coord_flip() +
  theme(legend.position = "bottom", legend.text=element_text(size=12)) +
  # theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  ylab("Mean speaker belief rating") +
  xlab("Topic")+
  ggtitle("conservative CCs")
ggsave(f="../graphs/conservativeCC-speakerBelief-ratings-byTopic.png",height=5,width=8)


#liberal CCs
liberalCCs = cd %>% 
  filter(item_type == "critical" & orientation =="liberal") %>% 
  droplevels()
nrow(liberalCCs) 


means = liberalCCs %>%
  group_by(socialInfo,topic) %>%
  summarise(Mean=mean(spBelief_rating),CILow=ci.low(spBelief_rating),CIHigh=ci.high(spBelief_rating)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
means


high = means %>%
  filter(socialInfo == "D") %>%
  mutate(eventItem = fct_reorder(topic,Mean))

means = means %>%
  mutate(topic = fct_relevel(topic,levels(high$topic))) %>% 
  mutate(socialInfo = fct_relevel(socialInfo,"R"))
means

subjmeans = liberalCCs %>%
  group_by(topic,workerid,socialInfo) %>%
  summarise(Mean = mean(spBelief_rating)) %>% 
  ungroup() %>% 
  mutate(socialInfo = fct_relevel(as.factor(as.character(socialInfo)),"D"))
subjmeans$topic <- factor(subjmeans$topic, levels = unique(levels(means$topic)))
levels(subjmeans$topic)
names(subjmeans)

ggplot(means, aes(x=topic, y=Mean, color=socialInfo,shape=socialInfo,fill=socialInfo)) + 
  geom_point(data=subjmeans,aes(fill=socialInfo,color=socialInfo),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_shape_manual(values=c(25, 24),labels=c("Democrat","Republican"),name="Speaker affiliation:") +
  scale_fill_manual(values=c("#56B4E9","tomato"),labels=c("Democrat","Republican"),name="Speaker affiliation:") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,.2,.4,.6,.8,1), labels=c("0", ".2", ".4", ".6", ".8", "1")) +
  scale_color_manual(name="Speaker affiliation:",labels=c("Democrat", "Republican"), values=c("#56B4E9","tomato")) +
  coord_flip() +
  theme(legend.position = "none", legend.text=element_text(size=12)) +
  # theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  ylab("Mean speaker belief rating") +
  xlab("Topic")+
  ggtitle("liberal CCs")
ggsave(f="../graphs/liberalCC-speakerBelief-ratings-byTopic.png",height=5,width=8)


#main clauses
MCs = cd %>% 
  filter(item_type == "MC" & orientation !="N") %>% 
  droplevels()
nrow(MCs) 


means = MCs %>%
  group_by(socialInfo,topic,orientation) %>%
  summarise(Mean=mean(spBelief_rating),CILow=ci.low(spBelief_rating),CIHigh=ci.high(spBelief_rating)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
means


high = means %>%
  filter(socialInfo == "D") %>%
  mutate(eventItem = fct_reorder(topic,Mean))

means = means %>%
  mutate(topic = fct_relevel(topic,levels(high$topic))) %>% 
  mutate(socialInfo = fct_relevel(socialInfo,"R"))
means

subjmeans = MCs %>%
  group_by(topic,workerid,socialInfo,orientation) %>%
  summarise(Mean = mean(spBelief_rating)) %>% 
  ungroup() %>% 
  mutate(socialInfo = fct_relevel(as.factor(as.character(socialInfo)),"D"))
subjmeans$topic <- factor(subjmeans$topic, levels = unique(levels(means$topic)))
levels(subjmeans$topic)
names(subjmeans)

ggplot(means, aes(x=topic, y=Mean, color=socialInfo,shape=socialInfo,fill=socialInfo)) + 
  geom_point(data=subjmeans,aes(fill=socialInfo,color=socialInfo),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_shape_manual(values=c(25, 24),labels=c("Democrat","Republican"),name="Speaker affiliation:") +
  scale_fill_manual(values=c("#56B4E9","tomato"),labels=c("Democrat","Republican"),name="Speaker affiliation:") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,.2,.4,.6,.8,1), labels=c("0", ".2", ".4", ".6", ".8", "1")) +
  scale_color_manual(name="Speaker affiliation:",labels=c("Democrat", "Republican"), values=c("#56B4E9","tomato")) +
  coord_flip() +
  theme(legend.position = "none", legend.text=element_text(size=12)) +
  # theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  ylab("Mean speaker belief rating") +
  xlab("Topic")+
  ggtitle("Main clause contents")+
  facet_wrap(~orientation)
ggsave(f="../graphs/MCs-speakerBelief-ratings-byTopic.png",height=5,width=8)

################################################################################
###H.1.3b: are projection ratings positively correlated with speaker belief ratings? CCs#########################################################

#overall
#overall
projection_spBelief_overall<-ggplot(subset(cd,(spBelief_rating!="NA" & projection!="NA" & item_type=="critical")), aes(x=spBelief_rating, y=projection)) +
  geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=.3) +
  xlab("speaker belief rating") +
  ylab("Certainty rating") +
  #theme(legend.position = "top", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  #xlim(0,1) +
  #ylim(0,1) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) 

projection_spBelief_overall
ggsave(projection_spBelief_overall ,filename="../graphs/speakerBeliefRatings/projection~spBeliefs_overall.png")


#topic
projection_spBelief_byTopic<-projection_spBelief_overall+
  facet_wrap(~topic,nrow=3)
projection_spBelief_byTopic
#ggsave(projection_spBelief_byTopic ,filename="../graphs/speakerBeliefRatings/projection~spBeliefs_byTopic.png")

#by predicate
projection_spBelief_byPredicate <-projection_spBelief_overall+
  facet_wrap(~predicate,nrow=3)
projection_spBelief_byPredicate
#ggsave(projection_spBelief_byPredicate ,filename="../graphs/speakerBeliefRatings/projection~spBeliefs_byPredicate.png")

# J & J figure 4
conservativeCCs = cd %>% 
  filter(item_type == "critical" & orientation =="conservative") %>% 
  droplevels()
nrow(conservativeCCs) 


proj.means = conservativeCCs %>%
  group_by(predicate,socialInfo) %>%
  summarise(Mean = mean(projection), CILow = ci.low(projection), CIHigh = ci.high(projection)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, predicate = fct_reorder(as.factor(predicate),Mean))  
proj.means
#View(proj.means)

nrow(proj.means) #24

high = proj.means %>%
  filter(socialInfo == "R") %>%
  mutate(predicate = fct_reorder(predicate,Mean))

conservativeCCs = conservativeCCs %>%
  mutate(predicate = fct_relevel(predicate,levels(high$predicate)))
table(conservativeCCs$predicate)

# change factor levels for prior_type for plotting
conservativeCCs = conservativeCCs %>%
  mutate(socialInfo= fct_relevel(socialInfo, "D", "R"))
levels(conservativeCCs$socialInfo)

ggplot(conservativeCCs, aes(x=spBelief_rating, y=projection,color=socialInfo)) +
  #geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=.3) +
  scale_color_manual(values=c("#56B4E9","tomato"),labels=c("Democrat","Republican"),name="Speaker affiliation") +
  xlab("Prior probability rating") +
  ylab("Certainty rating") +
  theme(legend.position = "bottom", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  #xlim(0,1) +
  #ylim(0,1) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(~predicate)+
  ggtitle("conservative CCs")
ggsave(f="../graphs/projection-by-prior-conservative.png",height=7,width=7)


#liberal CCs
liberalCCs = cd %>% 
  filter(item_type == "critical" & orientation =="liberal") %>% 
  droplevels()
nrow(liberalCCs) 


proj.means = liberalCCs %>%
  group_by(predicate,socialInfo) %>%
  summarise(Mean = mean(projection), CILow = ci.low(projection), CIHigh = ci.high(projection)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, predicate = fct_reorder(as.factor(predicate),Mean))  
proj.means
#View(proj.means)

nrow(proj.means) #24

high = proj.means %>%
  filter(socialInfo == "D") %>%
  mutate(predicate = fct_reorder(predicate,Mean))

liberalCCs = liberalCCs %>%
  mutate(predicate = fct_relevel(predicate,levels(high$predicate)))
table(liberalCCs$predicate)

# change factor levels for prior_type for plotting
liberalCCs = liberalCCs %>%
  mutate(socialInfo= fct_relevel(socialInfo, "D", "R"))
levels(liberalCCs$socialInfo)

ggplot(liberalCCs, aes(x=spBelief_rating, y=projection,color=socialInfo)) +
  #geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=.3) +
  scale_color_manual(values=c("#56B4E9","tomato"),labels=c("Democrat","Republican"),name="Speaker affiliation") +
  xlab("Prior probability rating") +
  ylab("Certainty rating") +
  theme(legend.position = "bottom", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  #xlim(0,1) +
  #ylim(0,1) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(~predicate)+
  ggtitle("liberal CCs")
ggsave(f="../graphs/projection-by-prior-liberal.png",height=7,width=7)

#for main clauses, do speaker beliefs predict certainty ratings?
MCs = cd %>% 
  filter(item_type == "MC") %>% 
  droplevels()
nrow(MCs) 


proj.means = MCs %>%
  group_by(socialInfo, orientation,topic) %>%
  summarise(Mean = mean(projection), CILow = ci.low(projection), CIHigh = ci.high(projection)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh, topic = fct_reorder(as.factor(topic),Mean))  
proj.means
#View(proj.means)

nrow(proj.means) #8

high = proj.means %>%
  filter(socialInfo == "D") %>%
  mutate(predicate = fct_reorder(topic,Mean))

MCs = MCs %>%
  mutate(topic = fct_relevel(topic,levels(high$topic)))
table(MCs$topic)

# change factor levels for prior_type for plotting
MCs= MCs %>%
  mutate(socialInfo= fct_relevel(socialInfo, "D", "R"))
levels(MCs$socialInfo)

ggplot(MCs, aes(x=spBelief_rating, y=projection,color=socialInfo)) +
  #geom_abline(intercept=0,slope=1,linetype="dashed",color="gray50") +
  geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=.3) +
  scale_color_manual(values=c("#56B4E9","tomato"),labels=c("Democrat","Republican"),name="Speaker affiliation") +
  xlab("Perceived speaker belief rating") +
  ylab("Certainty rating") +
  theme(legend.position = "bottom", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  #xlim(0,1) +
  #ylim(0,1) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) +
  facet_wrap(~orientation)+
  ggtitle("Main clause contents")

ggsave(f="../graphs/projection-by-prior-MCs.png",height=7,width=7)
f


########Listener beliefs - attempt #2#####

#plot projection by participant beliefs

colsToPaste <- c("socialInfo","orientation")
cd$socialInfo_orientation <- do.call(paste, c(cd[colsToPaste], sep = "-"))
cd$socialInfo_orientation <- as.factor(cd$socialInfo_orientation)

criticalItems <- droplevels(subset(cd,(participant_beliefs!="NA" & projection!="NA" & item_type=="critical")))
levels(criticalItems$socialInfo_orientation)
levels(criticalItems$socialInfo_orientation) <- c("D-liberal","R-liberal","D-conservative","R-conservative")
levels(criticalItems$socialInfo_orientation)

socialInfo.labs <- c(D="Democrat",R="Republican",liberal="liberal",conservative="conservative")
projection_partBeliefs_scatter<-ggplot(criticalItems, aes(x=spBelief_rating, y=participant_beliefs)) +
  #geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=.3) +
  xlab("Perceived Speaker belief rating") +
  ylab("Participant CC belief rating") +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  #guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  #scale_color_manual(values=c("blue2","deepskyblue","tomato","deeppink"),labels=c("D-conservative","D-liberal","R-conservative","R-liberal"),name="Speaker affiliation") +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  theme(panel.spacing.x = unit(4, "mm")) +
  #coord_fixed(ratio = 1)+
  facet_grid(orientation~ socialInfo,labeller = as_labeller(socialInfo.labs))
projection_partBeliefs_scatter
ggsave(f="../graphs/SpBeliefs_PartBeliefsBySpeakerAffiliationAndOrientation.png",height=7,width=7)


###identify "alignment" between perceived speaker and listener beliefs
cd.aligned<-subset(cd,(participant_beliefs<=0.5 & spBelief_rating <=0.5) | ((participant_beliefs>=0.5 & spBelief_rating >=0.5)))
nrow(cd)
nrow(cd.aligned)

cd.aligned.positive <- subset(cd,(participant_beliefs>=0.5 & spBelief_rating >=0.5))
cd.aligned.negative <- subset(cd,(participant_beliefs<=0.5 & spBelief_rating <=0.5))


alignedBeliefs<-ggplot(subset(cd.aligned,(spBelief_rating!="NA" & projection!="NA" & item_type=="critical")))+
  geom_point(shape=20, size=1, alpha=.3,aes(x=participant_beliefs,y=spBelief_rating,color="participant_beliefs")) +
  #scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("speaker beliefs") +
  xlab("participant beliefs")+
  #guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  #theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  ggtitle("Alignment")

alignedBeliefs

projection_alignedBeliefs<-ggplot(subset(cd.aligned.negative,(spBelief_rating!="NA" & projection!="NA" & item_type=="critical")))+
  geom_smooth(method="lm",aes(x=spBelief_rating,y=projection,color="spBelief_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=spBelief_rating,y=projection,color="spBelief_rating")) +
  geom_smooth(method="lm",aes(x=participant_beliefs,y=projection,color="participant_beliefs")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=participant_beliefs,y=projection,color="participant_beliefs")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("Belief ratings")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  #theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  ggtitle("Alignment")
projection_alignedBeliefs
ggsave(f="../graphs/AlignedBeliefs_projection.png",height=7,width=7)



cd.misaligned<-subset(cd,(participant_beliefs>0.5 & spBelief_rating <0.5) | ((participant_beliefs<0.5 & spBelief_rating >0.5)))

cd.misaligned.speakerPositive <- subset(cd, (participant_beliefs<0.5 & spBelief_rating >0.5))
cd.misaligned.speakerNegative <- subset(cd, (participant_beliefs>0.5 & spBelief_rating <0.5))



nrow(subset(cd,spBelief_rating!="NA" & projection!="NA" & item_type=="critical")) #2544
nrow(subset(cd.aligned,spBelief_rating!="NA" & projection!="NA" & item_type=="critical")) #1403
nrow(subset(cd.misaligned,spBelief_rating!="NA" & projection!="NA" & item_type=="critical")) #1141
#1403+1141 = 2544

misalignedBeliefs<-ggplot(subset(cd.misaligned,(spBelief_rating!="NA" & projection!="NA" & item_type=="critical")))+
  geom_point(shape=20, size=1, alpha=.3,aes(x=participant_beliefs,y=spBelief_rating,color="participant_beliefs")) +
  #scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("speaker beliefs") +
  xlab("participant beliefs")+
  #guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  #theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  ggtitle("Misalignment")

misalignedBeliefs

projection_misalignedBeliefs<-ggplot(subset(cd.misaligned.speakerNegative,(spBelief_rating!="NA" & projection!="NA" & item_type=="critical")))+
  geom_smooth(method="lm",aes(x=spBelief_rating,y=projection,color="spBelief_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=spBelief_rating,y=projection,color="spBelief_rating")) +
  geom_smooth(method="lm",aes(x=participant_beliefs,y=projection,color="participant_beliefs")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=participant_beliefs,y=projection,color="participant_beliefs")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("Belief ratings")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  #theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  ggtitle("Misalignment")

projection_misalignedBeliefs

ggsave(f="../graphs/MisalignedBeliefs_projection.png",height=7,width=7)

#########################################################
###Some other ancilliary plots/analyses#################
########################################################

###are speaker belief ratings predicted by participant prior beliefs?
spBeliefs_partBeliefs_overall<-ggplot(subset(cd,(participant_beliefs!="NA" & projection!="NA" & item_type=="critical")), aes(x=participant_beliefs, y=spBelief_rating)) +
  geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=.3) +
  scale_color_manual(values=c("#56B4E9","#E69F00"),labels=c("lower probability","higher probability"),name="Fact") +
  xlab("Participant belief rating") +
  ylab("speaker belief rating") +
  theme(legend.position = "top", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  #xlim(0,1) +
  #ylim(0,1) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1) 

spBeliefs_partBeliefs_overall
#ggsave(spBeliefs_partBeliefs_overall ,filename="../graphs/speakerBeliefRatings/SpBelief~partBelief.png")

#by topic
spBeliefs_partBeliefs_byTopic<-spBeliefs_partBeliefs_topic<-spBeliefs_partBeliefs_overall+
  facet_wrap(~topic,nrow=3)

spBeliefs_partBeliefs_byTopic
#ggsave(spBeliefs_partBeliefs_byTopic ,filename="../graphs/speakerBeliefRatings/SpBelief~partBelief_byTopic.png")


#plot speaker beliefs and participant beliefs on the same graph
projection_bothBeliefs<-ggplot(subset(cd,(spBelief_rating!="NA" & projection!="NA" & item_type=="critical")))+
  geom_smooth(method="lm",aes(x=spBelief_rating,y=projection,color="spBelief_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=spBelief_rating,y=projection,color="spBelief_rating")) +
  geom_smooth(method="lm",aes(x=participant_beliefs,y=projection,color="participant_beliefs")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=participant_beliefs,y=projection,color="participant_beliefs")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("Belief ratings")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)

projection_bothBeliefs
#ggsave(projection_bothBeliefs ,filename="../graphs/projection~BothBeliefs.png")


##plot model predictions instead - looks pretty much the same as the one above so we'll stick with that.
#participant beliefs (sum coding for predicates):
#(Intercept)            0.56470    0.01245  362.76957  45.346  < 2e-16 ***
#participant_beliefs    0.04127    0.01307 2368.69008   3.159 0.001603 ** 

#speaker beliefs (sum coding for predicates)
#Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)        0.49380    0.01285  409.38187  38.416  < 2e-16 ***
#spBelief_rating    0.19022    0.01454 2370.46619  13.079  < 2e-16 ***




#predicate projection for critical items
projection_predicate_criticalItems<-ggplot(subset(cd,item_type=="critical"), aes(x=reorder(predicate,projection),y=projection) )+
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  xlab("Predicate")+
  theme(axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18),axis.text.x = element_text(size=12,angle = 45, hjust=1))+
  scale_y_continuous(breaks = c(0,.25,.5,.75,1), limits = c(0,1))

projection_predicate_criticalItems
#ggsave(projection_predicate_criticalItems ,filename="../graphs/Projection~Predicate_criticalItems.png")


#is there an effect of gender on speaker beliefs?

#by orientation - both critical and non-critical items
spBeliefs_speakerGender_critical_nonCritical<-ggplot(subset(cd,(orientation!="N")), aes(x=orientation,y=spBelief_rating,fill=SpeakerGender)) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  ylab("speaker belief rating")+
  xlab("CC orientation")+
  theme(legend.position = c(.2,.85),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1))+
  scale_fill_manual(labels=c("Female speaker","Male speaker"),values=c("purple","yellowgreen"))

spBeliefs_speakerGender_critical_nonCritical
#ggsave(spBeliefs_speakerGender_critical_nonCritical ,filename="../graphs/SpeakerGender/spBeliefs~speakerGender_critical_nonCriticalItems.png")

#by orientation - just critical items
spBeliefs_speakerGender_critical <- ggplot(subset(cd,(orientation!="N" & item_type=="critical")), aes(x=orientation,y=spBelief_rating,fill=SpeakerGender)) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  ylab("speaker belief rating")+
  xlab("CC orientation")+
  theme(legend.position = c(.2,.85),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1))+
  scale_fill_manual(labels=c("Female speaker","Male speaker"),values=c("purple","yellowgreen"))

spBeliefs_speakerGender_critical
#ggsave(spBeliefs_speakerGender_critical ,filename="../graphs/SpeakerGender/spBeliefs~speakerGender_criticalItems.png")



#is there an effect of gender on projection? include critical and non-critical items
projection_speakerGender_critical_nonCritical<-ggplot(subset(cd,orientation!="N"), aes(x=socialInfo,y=projection,fill=SpeakerGender)) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  xlab("CC orientation")+
  ylab("Mean certainty rating")+
  theme(legend.position = "bottom",axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_x_discrete(labels=c("Democrat", "Republican")) +
  scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1))+
  scale_fill_manual(labels=c("Female speaker","Male speaker"),values=c("purple","yellowgreen"))+
  facet_wrap(~orientation)

projection_speakerGender_critical_nonCritical
#ggsave(projection_speakerGender_critical_nonCritical,filename="../graphs/SpeakerGender/projection~speakerGender_critical_nonCriticalItems.png")


#is there an effect of gender on projection? include just critical items
projection_speakerGender_critical<-ggplot(subset(cd,(orientation!="N" &item_type=="critical")), aes(x=socialInfo,y=projection,fill=SpeakerGender)) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  xlab("CC orientation")+
  ylab("Mean certainty rating")+
  theme(legend.position = "bottom",axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_x_discrete(labels=c("Democrat", "Republican")) +
  scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1))+
  scale_fill_manual(labels=c("Female speaker","Male speaker"),values=c("purple","yellowgreen"))+
  facet_wrap(~orientation)
projection_speakerGender_critical
ggsave(projection_speakerGender_critical ,filename="../graphs/SpeakerGender/projection~speakerGender_criticalItems.png")
