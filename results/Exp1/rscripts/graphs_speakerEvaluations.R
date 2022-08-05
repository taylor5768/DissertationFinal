#graphs for speaker evaluation data from Exp 1
#based on Judith Tonhauser's code

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)


require(tidyverse)
library(ggrepel)
library(dichromat)
library(forcats)
library(ggrepel)
library(RColorBrewer)
library(Hmisc)



# load helper functions
source('../../helpers.R')


cd = read.csv("../data/data_preprocessed.csv")
cd$orientation<-as.factor(cd$orientation)

length(unique(cd$workerid))

################################################
##################PREDICTING TRAITS#############
################################################




#from https://r-coder.com/correlation-plot-r/
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  Cor <- abs(cor(x, y)) # Remove abs function if desired
  txt <- paste0(prefix, format(c(Cor, 0.123456789), digits = digits)[1])
  if(missing(cex.cor)) {
    cex.cor <- 0.4 / strwidth(txt)
  }
  text(0.5, 0.5, txt,
       cex = 1 + cex.cor * Cor) # Resize the text by level of correlation
}

pairs(cd.sub,upper.panel = panel.cor,lower.panel = panel.smooth)

#speakerEvalcorrelations.png

############
##empathy###
############


#by speaker affiliation
empathy_socialInfo_nonNeutralCriticalItems <- ggplot(subset(cd,(orientation!="N" & item_type=="critical")), aes(x=socialInfo,y=empathy_rating,fill=socialInfo),xpd=FALSE) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Democrat speaker","Republican speaker"),values=c("#56B4E9","#D55E00"))+
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
  scale_x_discrete(labels=c("Democrat","Republican"))+
  theme(legend.position = c(.2,.85),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1))+
  scale_fill_manual(labels=c("Female speaker","Male speaker"),values=c("purple","yellowgreen"))

#ggsave(empathy_socialInfo_Gender_nonNeutralCriticalItems,filename="../graphs/SpeakerEvaluations/empathy/empathy~socialInfo_Gender_nonNeutralCriticalItems.png")


#################
##assertiveness###
#################

#by speaker affiliation
assertiveness_socialInfo_nonNeutralCriticalItems <- ggplot(subset(cd,(orientation!="N" & item_type=="critical")), aes(x=socialInfo,y=assertiveness_rating,fill=socialInfo),xpd=FALSE) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Democrat speaker","Republican speaker"),values=c("#56B4E9","#D55E00"))+
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
  scale_x_discrete(labels=c("Democrat","Republican"))+
  theme(legend.position = c(.2,.85),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1))+
  scale_fill_manual(labels=c("Female speaker","Male speaker"),values=c("purple","yellowgreen"))


#ggsave(assertiveness_socialInfo_Gender_nonNeutralCriticalItems,filename="../graphs/SpeakerEvaluations/assertiveness/assertiveness~socialInfo_Gender_nonNeutralCriticalItems.png")

#################
##femininity###
#################

#by speaker affiliation
femininity_socialInfo_nonNeutralCriticalItems <- ggplot(subset(cd,(orientation!="N" & item_type=="critical")), aes(x=socialInfo,y=femininity_rating,fill=socialInfo),xpd=FALSE) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Democrat speaker","Republican speaker"),values=c("#56B4E9","#D55E00"))+
  ylab("Mean femininity rating")+
  theme(legend.position="left")+
  labs(fill="Social Info")+
  ylim(0,1)+
  theme(legend.position = c(.5,.85),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),axis.title.x = element_blank(),axis.text.x=element_blank(),legend.title=element_blank(),legend.text=element_text(size=18),aspect.ratio=1)


#ggsave(femininity_socialInfo_nonNeutralCriticalItems,filename="../graphs/SpeakerEvaluations/femininity/femininity~socialInfo_nonNeutralCriticalItems.png")

#adding speaker gender
femininity_socialInfo_Gender_nonNeutralCriticalItems <-ggplot(subset(cd,(orientation!="N" & item_type=="critical")), aes(x=socialInfo,y=femininity_rating,fill=SpeakerGender)) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  ylab("mean femininity rating")+
  xlab("Speaker party")+
  scale_x_discrete(labels=c("Democrat","Republican"))+
  theme(legend.position = c(.2,.85),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1))+
  scale_fill_manual(labels=c("Female speaker","Male speaker"),values=c("purple","yellowgreen"))

#ggsave(femininity_socialInfo_Gender_nonNeutralCriticalItems,filename="../graphs/SpeakerEvaluations/femininity/femininity~socialInfo_Gender_nonNeutralCriticalItems.png")


#################
##intelligence###
#################

#by speaker affiliation
intelligence_socialInfo_nonNeutralCriticalItems<-ggplot(subset(cd,(orientation!="N" & item_type=="critical")), aes(x=socialInfo,y=intelligence_rating,fill=socialInfo),xpd=FALSE) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Democrat speaker","Republican speaker"),values=c("#56B4E9","#D55E00"))+
  ylab("Mean intelligence rating")+
  theme(legend.position="left")+
  labs(fill="Social Info")+
  ylim(0,1)+
  theme(legend.position = c(.5,.85),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),axis.title.x = element_blank(),axis.text.x=element_blank(),legend.title=element_blank(),legend.text=element_text(size=18),aspect.ratio=1)


#ggsave(intelligence_socialInfo_nonNeutralCriticalItems,filename="../graphs/SpeakerEvaluations/intelligence/intelligence~socialInfo_nonNeutralCriticalItems.png")


#adding gender
intelligence_socialInfo_Gender_nonNeutralCriticalItems<-ggplot(subset(cd,(orientation!="N" & item_type=="critical")), aes(x=socialInfo,y=intelligence_rating,fill=SpeakerGender)) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  ylab("mean intelligence rating")+
  xlab("Speaker party")+
  scale_x_discrete(labels=c("Democrat","Republican"))+
  theme(legend.position = c(.2,.85),axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18))+
  scale_y_continuous(breaks = c(0,.5,1), limits = c(0,1))+
  scale_fill_manual(labels=c("Female speaker","Male speaker"),values=c("purple","yellowgreen"))

#ggsave(intelligence_socialInfo_Gender_nonNeutralCriticalItems,filename="../graphs/SpeakerEvaluations/intelligence/intelligence~socialInfo_Gender_nonNeutralCriticalItems.png")



#############################################
###############PREDICTING projection#########
#############################################
#do social evals explain projection ratings above and beyond social info?


###first plot all on the same graph
projection_allTraits<-ggplot(subset(cd,(projection!="NA" & item_type=="critical" & orientation!="N")))+
  geom_smooth(se=F,method="lm",aes(x=assertiveness_rating,y=projection,color="assertiveness_rating")) +
  #geom_point(shape=20, size=1, alpha=.3,aes(x=assertiveness_rating,y=projection,color="assertiveness_rating")) +
  geom_smooth(se=F,method="lm",aes(x=empathy_rating,y=projection,color="empathy_rating")) +
  #geom_point(shape=20, size=1, alpha=.3,aes(x=empathy_rating,y=projection,color="empathy_rating"))+
  geom_smooth(se=F,method="lm",aes(x=femininity_rating,y=projection,color="femininity_rating"))+
  #geom_point(shape=20, size=1, alpha=.3,aes(x=femininity_rating,y=projection,color="femininity_rating"))+
  geom_smooth(se=F,method="lm",aes(x=intelligence_rating,y=projection,color="intelligence_rating"))#+
  #geom_point(shape=20, size=1, alpha=.3,aes(x=intelligence_rating,y=projection,color="intelligence_rating"))

projection_allTraits

ggsave(projection_allTraits,filename="../graphs/SpeakerEvaluations/projection~allTraits.png")


#by condition
projection_allTraits_byCondition<-projection_allTraits+
  facet_wrap(~socialInfo+orientation)

projection_allTraits_byCondition

ggsave(projection_allTraits_byCondition,filename="../graphs/SpeakerEvaluations/projection~allTraits_byCondition.png")

#zoom out
projection_allTraits_byCondition_zoomedOut<-projection_allTraits+
  xlim(0,1)+
  ylim(0,1)

projection_allTraits_byCondition_zoomedOut


ggsave(projection_allTraits_byCondition_zoomedOut,filename="../graphs/SpeakerEvaluations/projection~allTraits_byCondition_zoomedOut.png")
#################
#assertiveness###
#################


#collapsing over speaker type
projection_assertiveness_orientation_allSpeakers <- ggplot(subset(cd,(assertiveness_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N" & (party=="Dem" | party =="Rep"))))+
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
  facet_grid(~party+orientation)+
  ggtitle("all speakers")

projection_assertiveness_orientation_allSpeakers


#############
#empathy####
#############

#collapsing over speaker type
projection_empathy_orientation_allSpeakers <- ggplot(subset(cd,(empathy_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N" & (party=="Dem" | party =="Rep"))))+
  geom_smooth(method="lm",aes(x=empathy_rating,y=projection,color="assertiveness_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=empathy_rating,y=projection,color="empathy_rating")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("empathy ratings")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_grid(~party+orientation)+
  ggtitle("all speakers")

projection_empathy_orientation_allSpeakers


projection_empathy_orientation_allSpeakers <- ggplot(subset(cd,(empathy_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N")))+
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
  facet_grid(socialInfo~orientation)+
  ggtitle("all speakers")

projection_empathy_orientation_allSpeakers


###############
#femininity####
###############


#collapsing over speaker type
projection_femininity_orientation_allSpeakers <- ggplot(subset(cd,(femininity_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N" & (party=="Dem" | party =="Rep"))))+
  geom_smooth(method="lm",aes(x=femininity_rating,y=projection,color="femininity_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=femininity_rating,y=projection,color="assertiveness_rating")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("femininity ratings")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_grid(~party+orientation)+
  ggtitle("all speakers")

projection_femininity_orientation_allSpeakers
#ggsave(projection_femininity_orientation_allSpeakers,filename="../graphs/SpeakerEvaluations/femininity/projection~femininity+orientation_allSpeakers .png")




#################
#intelligence####
#################
#collapsing over speaker type
projection_intelligence_orientation_allSpeakers <- ggplot(subset(cd,(intelligence_rating!="NA" & projection!="NA" & item_type=="critical" & orientation!="N" & (party=="Dem" | party =="Rep"))))+
  geom_smooth(method="lm",aes(x=intelligence_rating,y=projection,color="intelligence_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=intelligence_rating,y=projection,color="assertiveness_rating")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("intelligence ratings")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(.2,-.5,2,0),"cm"),legend.position=c(0.4,-0.3),legend.text=element_text(size=18),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=18),axis.title=element_text(size=18,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_grid(~party+orientation)+
  ggtitle("all speakers")

projection_intelligence_orientation_allSpeakers
#ggsave(projection_intelligence_orientation_allSpeakers,filename="../graphs/SpeakerEvaluations/intelligence/projection~intelligence+orientation_allSpeakers .png")
