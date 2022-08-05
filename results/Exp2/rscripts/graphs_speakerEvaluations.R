#graphs for speaker evaluation data from Exp 1
#based on Judith Tonhauser's code

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

################################################
##################PREDICTING TRAITS#############
################################################

############
##empathy###
############


#by speaker affiliation
empathy_socialInfo_nonNeutralCriticalItems <- ggplot(subset(cd,(orientation!="N" & item_type=="critical")), aes(x=socialInfo,y=empathy_rating,fill=socialInfo),xpd=FALSE) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Non-Southern","Southern"),values=c("#56B4E9","#D55E00"))+
  ylab("Mean empathy rating")+
  theme(legend.position="left")+
  labs(fill="Social Info")+
  ylim(0,1)+
  theme(legend.position = c(.5,.85),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),axis.title.x = element_blank(),axis.text.x=element_blank(),legend.title=element_blank(),legend.text=element_text(size=18),aspect.ratio=1)

empathy_socialInfo_nonNeutralCriticalItems

#ggsave(empathy_socialInfo_nonNeutralCriticalItems,filename="../graphs/SpeakerEvaluations/empathy/empathy~socialInfo_nonNeutralCriticalItems.png")


#adding gender
empathy_socialInfo_Gender_nonNeutralCriticalItems <- ggplot(subset(cd,(orientation!="N" & item_type=="critical")), aes(x=socialInfo,y=empathy_rating,fill=SpeakerGender)) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  ylab("mean empathy rating")+
  xlab("Speaker party")+
  scale_x_discrete(labels=c("Non-Southern","Southern"))+
  theme(legend.position = c(.2,.85),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1))+
  scale_fill_manual(labels=c("Female speaker","Male speaker"),values=c("purple","yellowgreen"))

empathy_socialInfo_Gender_nonNeutralCriticalItems

#ggsave(empathy_socialInfo_Gender_nonNeutralCriticalItems,filename="../graphs/SpeakerEvaluations/empathy/empathy~socialInfo_Gender_nonNeutralCriticalItems.png")


#################
##assertiveness###
#################

#by speaker affiliation
assertiveness_socialInfo_nonNeutralCriticalItems <- ggplot(subset(cd,(orientation!="N" & item_type=="critical")), aes(x=socialInfo,y=assertiveness_rating,fill=socialInfo),xpd=FALSE) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Non-Southern","Southern"),values=c("#56B4E9","#D55E00"))+
  ylab("Mean assertiveness rating")+
  theme(legend.position="left")+
  labs(fill="Social Info")+
  ylim(0,1)+
  theme(legend.position = c(.5,.85),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),axis.title.x = element_blank(),axis.text.x=element_blank(),legend.title=element_blank(),legend.text=element_text(size=18),aspect.ratio=1)

assertiveness_socialInfo_nonNeutralCriticalItems

#ggsave(empathy_socialInfo_nonNeutralCriticalItems,filename="../graphs/SpeakerEvaluations/assertiveness/assertiveness~socialInfo_nonNeutralCriticalItems.png")


#adding gender
assertiveness_socialInfo_Gender_nonNeutralCriticalItems <- ggplot(subset(cd,(orientation!="N" & item_type=="critical")), aes(x=socialInfo,y=assertiveness_rating,fill=SpeakerGender)) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  ylab("mean assertiveness rating")+
  xlab("Speaker party")+
  scale_x_discrete(labels=c("Non-Southern","Southern"))+
  theme(legend.position = c(.2,.85),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1))+
  scale_fill_manual(labels=c("Female speaker","Male speaker"),values=c("purple","yellowgreen"))

assertiveness_socialInfo_Gender_nonNeutralCriticalItems

#ggsave(assertiveness_socialInfo_Gender_nonNeutralCriticalItems,filename="../graphs/SpeakerEvaluations/assertiveness/assertiveness~socialInfo_Gender_nonNeutralCriticalItems.png")

#################
##femininity###
#################

#by speaker affiliation
femininity_socialInfo_nonNeutralCriticalItems <- ggplot(subset(cd,(orientation!="N" & item_type=="critical")), aes(x=socialInfo,y=femininity_rating,fill=socialInfo),xpd=FALSE) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Non-Southern","Southern"),values=c("#56B4E9","#D55E00"))+
  ylab("Mean femininity rating")+
  theme(legend.position="left")+
  labs(fill="Social Info")+
  ylim(0,1)+
  theme(legend.position = c(.5,.85),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),axis.title.x = element_blank(),axis.text.x=element_blank(),legend.title=element_blank(),legend.text=element_text(size=18),aspect.ratio=1)

femininity_socialInfo_nonNeutralCriticalItems


#ggsave(femininity_socialInfo_nonNeutralCriticalItems,filename="../graphs/SpeakerEvaluations/femininity/femininity~socialInfo_nonNeutralCriticalItems.png")

#adding speaker gender
femininity_socialInfo_Gender_nonNeutralCriticalItems <-ggplot(subset(cd,(orientation!="N" & item_type=="critical")), aes(x=socialInfo,y=femininity_rating,fill=SpeakerGender)) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  ylab("mean femininity rating")+
  xlab("Speaker party")+
  scale_x_discrete(labels=c("Non-Southern","Southern"))+
  theme(legend.position = c(.2,.85),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1))+
  scale_fill_manual(labels=c("Female speaker","Male speaker"),values=c("purple","yellowgreen"))

femininity_socialInfo_Gender_nonNeutralCriticalItems

#ggsave(femininity_socialInfo_Gender_nonNeutralCriticalItems,filename="../graphs/SpeakerEvaluations/femininity/femininity~socialInfo_Gender_nonNeutralCriticalItems.png")


#################
##intelligence###
#################

#by speaker affiliation
intelligence_socialInfo_nonNeutralCriticalItems<-ggplot(subset(cd,(orientation!="N" & item_type=="critical")), aes(x=socialInfo,y=intelligence_rating,fill=socialInfo),xpd=FALSE) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Non-Southern","Southern"),values=c("#56B4E9","#D55E00"))+
  ylab("Mean intelligence rating")+
  theme(legend.position="left")+
  labs(fill="Social Info")+
  ylim(0,1)+
  theme(legend.position = c(.5,.85),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),axis.title.x = element_blank(),axis.text.x=element_blank(),legend.title=element_blank(),legend.text=element_text(size=18),aspect.ratio=1)

intelligence_socialInfo_nonNeutralCriticalItems

#ggsave(intelligence_socialInfo_nonNeutralCriticalItems,filename="../graphs/SpeakerEvaluations/intelligence/intelligence~socialInfo_nonNeutralCriticalItems.png")


#adding gender
intelligence_socialInfo_Gender_nonNeutralCriticalItems<-ggplot(subset(cd,(orientation!="N" & item_type=="critical")), aes(x=socialInfo,y=intelligence_rating,fill=SpeakerGender)) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  ylab("mean intelligence rating")+
  xlab("Speaker party")+
  scale_x_discrete(labels=c("Non-Southern","Southern"))+
  theme(legend.position = c(.2,.85),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1))+
  scale_fill_manual(labels=c("Female speaker","Male speaker"),values=c("purple","yellowgreen"))

intelligence_socialInfo_Gender_nonNeutralCriticalItems

#ggsave(intelligence_socialInfo_Gender_nonNeutralCriticalItems,filename="../graphs/SpeakerEvaluations/intelligence/intelligence~socialInfo_Gender_nonNeutralCriticalItems.png")



#############################################
###############PREDICTING projection#########
#############################################

#do participant ratings explain projection ratings above and beyond social info?

#################
#assertiveness###
#################


#collapsing over speaker type
projection_assertiveness_orientation_allSpeakers <- ggplot(subset(cd,(assertiveness_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N")))+
  geom_smooth(method="lm",aes(x=assertiveness_rating,y=projection,color="assertiveness_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=empathy_rating,y=projection,color="assertiveness_rating")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("assertiveness ratings")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_grid(~orientation)+
  ggtitle("all speakers")

projection_assertiveness_orientation_allSpeakers

#ggsave(projection_assertiveness_orientation_allSpeakers,filename="../graphs/SpeakerEvaluations/assertiveness/projection~assertiveness_allSpeakers.png")

#for Republican speakers
projection_assertiveness_socialInfo_RepublicanSpeakers <- ggplot(subset(cd,(assertiveness_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N"&socialInfo=="N")))+
  geom_smooth(method="lm",aes(x=assertiveness_rating,y=projection,color="assertiveness_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=assertiveness_rating,y=projection,color="assertiveness_rating")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("assertiveness ratings")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_grid(~orientation)+
  ggtitle("Republican speakers")

projection_assertiveness_socialInfo_RepublicanSpeakers

#ggsave(projection_assertiveness_socialInfo_RepublicanSpeakers,filename="../graphs/SpeakerEvaluations/assertiveness/projection~assertiveness_RepublicanSpeakers.png")


#for Democrat speakers
projection_assertiveness_socialInfo_DemocratSpeakers <- ggplot(subset(cd,(assertiveness_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N"&socialInfo=="N")))+
  geom_smooth(method="lm",aes(x=assertiveness_rating,y=projection,color="assertiveness_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=assertiveness_rating,y=projection,color="assertiveness_rating")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("assertiveness ratings")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_grid(~orientation)+
  ggtitle("Democrat speakers")

projection_assertiveness_socialInfo_DemocratSpeakers

#ggsave(projection_assertiveness_socialInfo_RepublicanSpeakers,filename="../graphs/SpeakerEvaluations/assertiveness/projection~assertiveness_DemocratSpeakers.png")


#############
#empathy####
#############

#collapsing over speaker type
projection_empathy_orientation_allSpeakers <- ggplot(subset(cd,(empathy_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N")))+
  geom_smooth(method="lm",aes(x=empathy_rating,y=projection,color="assertiveness_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=empathy_rating,y=projection,color="assertiveness_rating")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("empathy ratings")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_grid(~orientation)+
  ggtitle("all speakers")

projection_empathy_orientation_allSpeakers
#ggsave(projection_empathy_orientation_allSpeakers,filename="../graphs/SpeakerEvaluations/empathy/projection~empathy_orientation_allSpeakers.png")


#for Republican speakers
projection_empathy_orientation_RepublicanSpeakers <-ggplot(subset(cd,(empathy_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N"&socialInfo=="S")))+
  geom_smooth(method="lm",aes(x=empathy_rating,y=projection,color="assertiveness_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=empathy_rating,y=projection,color="assertiveness_rating")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("empathy ratings")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_grid(~orientation)+
  ggtitle("Republican speakers")

projection_empathy_orientation_RepublicanSpeakers
#ggsave(projection_empathy_orientation_RepublicanSpeakers,filename="../graphs/SpeakerEvaluations/empathy/projection~empathy_socialinfo_RepublicanSpeakers.png")


#for Democrat speakers
projection_empathy_orientation_DemocratSpeakers  <- ggplot(subset(cd,(empathy_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N"&socialInfo=="N")))+
  geom_smooth(method="lm",aes(x=empathy_rating,y=projection,color="empathy_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=empathy_rating,y=projection,color="empathy_rating")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("empathy ratings")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_grid(~orientation)+
  ggtitle("Democrat speakers")


projection_empathy_orientation_DemocratSpeakers
#ggsave(projection_empathy_orientation_DemocratSpeakers,filename="../graphs/SpeakerEvaluations/empathy/projection~empathy_socialinfo_DemocratSpeakers.png")


###############
#femininity####
###############


#collapsing over speaker type
projection_femininity_orientation_allSpeakers <- ggplot(subset(cd,(femininity_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N")))+
  geom_smooth(method="lm",aes(x=empathy_rating,y=projection,color="femininity_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=empathy_rating,y=projection,color="assertiveness_rating")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("femininity ratings")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_grid(~orientation)+
  ggtitle("all speakers")

projection_femininity_orientation_allSpeakers
#ggsave(projection_femininity_orientation_allSpeakers,filename="../graphs/SpeakerEvaluations/femininity/projection~femininity+orientation_allSpeakers .png")



#for Republican speakers
projection_femininity_socialInfo_RepublicanSpeakers <- ggplot(subset(cd,(femininity_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N"&socialInfo=="S")))+
  geom_smooth(method="lm",aes(x=femininity_rating,y=projection,color="femininity_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=femininity_rating,y=projection,color="assertiveness_rating")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("femininity rating")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_grid(~orientation)+
  ggtitle("Republican speakers")

projection_femininity_socialInfo_RepublicanSpeakers 
#ggsave(projection_femininity_socialInfo_RepublicanSpeakers,filename="../graphs/SpeakerEvaluations/femininity/projection~femininity_socialinfo_RepublicanSpeakers.png")


#for Democrat speakers
projection_femininity_socialInfo_DemocratSpeakers <- ggplot(subset(cd,(femininity_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N"&socialInfo=="N")))+
  geom_smooth(method="lm",aes(x=femininity_rating,y=projection,color="femininity_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=femininity_rating,y=projection,color="assertiveness_rating")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("femininity rating")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_grid(~orientation)+
  ggtitle("Democrat speakers")

projection_femininity_socialInfo_DemocratSpeakers 
#ggsave(projection_femininity_socialInfo_DemocratSpeakers,filename="../graphs/SpeakerEvaluations/femininity/projection~femininity_socialinfo_DemocratSpeakers.png")


projection_femininity_socialInfo_DemocratSpeakers_byGender <- ggplot(subset(cd,(femininity_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N"&socialInfo=="N")))+
  geom_smooth(method="lm",aes(x=femininity_rating,y=projection,color="femininity_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=femininity_rating,y=projection,color="assertiveness_rating")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("femininity rating")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_grid(~orientation+SpeakerGender)+
  ggtitle("Democrat speakers by gender")

projection_femininity_socialInfo_DemocratSpeakers_byGender 
#ggsave(projection_femininity_socialInfo_DemocratSpeakers_byGender,filename="../graphs/SpeakerEvaluations/femininity/projection~femininity_socialinfo_DemocratSpeakers_bySpeakerGender.png")

projection_femininity_socialInfo_RepublicanSpeakers_byGender <- ggplot(subset(cd,(femininity_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N"&socialInfo=="S")))+
  geom_smooth(method="lm",aes(x=femininity_rating,y=projection,color="femininity_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=femininity_rating,y=projection,color="assertiveness_rating")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("femininity rating")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_grid(~orientation+SpeakerGender)+
  ggtitle("Republican speakers by speaker gender")

projection_femininity_socialInfo_RepublicanSpeakers_byGender
#ggsave(projection_femininity_socialInfo_RepublicanSpeakers_byGender,filename="../graphs/SpeakerEvaluations/femininity/projection~femininity_socialinfo_RepublicanSpeakers_bySpeakerGender.png")



#################
#intelligence####
#################
#collapsing over speaker type
projection_intelligence_orientation_allSpeakers <- ggplot(subset(cd,(intelligence_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N")))+
  geom_smooth(method="lm",aes(x=empathy_rating,y=projection,color="femininity_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=empathy_rating,y=projection,color="assertiveness_rating")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("intelligence ratings")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_grid(~orientation)+
  ggtitle("all speakers")

projection_intelligence_orientation_allSpeakers
#ggsave(projection_intelligence_orientation_allSpeakers,filename="../graphs/SpeakerEvaluations/intelligence/projection~intelligence+orientation_allSpeakers .png")


#for Republican speakers
projection_intelligence_orientation_RepublicanSpeakers<- ggplot(subset(cd,(intelligence_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N"&socialInfo=="S")))+
  geom_smooth(method="lm",aes(x=intelligence_rating,y=projection,color="assertiveness_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=intelligence_rating,y=projection,color="assertiveness_rating")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("intelligence rating")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_grid(~orientation)+
  ggtitle("Republican speakers")

projection_intelligence_orientation_RepublicanSpeakers
#ggsave(projection_intelligence_orientation_RepublicanSpeakers,filename="../graphs/SpeakerEvaluations/intelligence/projection~intelligence+socialInfo_RepublicanSpeakers.png")


#for Democrat speakers
projection_intelligence_orientation_DemocratSpeakers<-ggplot(subset(cd,(intelligence_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N"&socialInfo=="N")))+
  geom_smooth(method="lm",aes(x=intelligence_rating,y=projection,color="assertiveness_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=intelligence_rating,y=projection,color="assertiveness_rating")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("intelligence rating")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_grid(~orientation)+
  ggtitle("Democrat speakers")


projection_intelligence_orientation_DemocratSpeakers
#ggsave(projection_intelligence_orientation_DemocratSpeakers,filename="../graphs/SpeakerEvaluations/intelligence/projection~intelligence+socialInfo_DemocratSpeakers.png")



#look at participants political affiliation
projection_intelligence_orientation_participantPoliticalAffil <- ggplot(subset(cd,(intelligence_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N"&socialInfo=="N" & (party=="Dem"|party=="Rep"))))+
  geom_smooth(method="lm",aes(x=intelligence_rating,y=projection,color="assertiveness_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=intelligence_rating,y=projection,color="assertiveness_rating")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("intelligence rating")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_grid(~party+orientation)+
  ggtitle("Democrat speakers; top row label = participant political party")

projection_intelligence_orientation_participantPoliticalAffil
#ggsave(projection_intelligence_orientation_participantPoliticalAffil,filename="../graphs/SpeakerEvaluations/intelligence/projection~intelligence+orientation+participantPoliticalAffil.png")

