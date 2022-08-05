#identifying CCs that display strong associations w/Democrat/Republican parties
#graphs
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

# load helper functions
source('../../helpers.R')

cd = read.csv("../data/data_preprocessed.csv")
cd = droplevels(subset(cd,(trigger_class!="Party" & trigger_class!="Empty")))
addedInfo = read.csv("../../../../Speakers.csv")
addedInfo= subset(addedInfo, select = -c(List,SpeakerGender,Region) )
names(addedInfo)[1] <- "name"
cd = merge(cd,addedInfo, by.x="name")



agr= aggregate(response ~ socialInfo+socialCondition, data=cd, FUN="mean")
agr$CILow = aggregate(response ~ socialInfo+socialCondition, data=cd, FUN="ci.low")$response
agr$CIHigh = aggregate(response ~ socialInfo+socialCondition, data=cd, FUN="ci.high")$response
agr$YMin = agr$response - agr$CILow
agr$YMax = agr$response + agr$CIHigh
agr

#look at the overall responses
ggplot(agr, aes(x=socialInfo,y=response,fill=socialCondition),xpd=FALSE) +
  geom_bar(stat="identity",width=.5,position=position_dodge(.9)) +
  scale_fill_manual(labels=c("Democrat","Republican"),values=c("#56B4E9", "tomato1"))+
  scale_x_discrete(labels=c("Non-Southern", "Southern")) +
  xlab("Speaker region") +
  ylab("Mean response") + 
  ylim(0,1)+
  labs(fill="Question Type")+
  theme(text = element_text(size=16))+
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=position_dodge(.9))

#look at mean responses to individual locations
agrID= aggregate(response ~ socialInfo+socialCondition+AllSpeakerInfo, data=cd, FUN="mean")
agrID$CILow = aggregate(response ~ socialInfo+socialCondition+AllSpeakerInfo, data=cd, FUN="ci.low")$response
agrID$CIHigh = aggregate(response ~ socialInfo+socialCondition+AllSpeakerInfo, data=cd, FUN="ci.high")$response
agrID$YMin = agrID$response - agrID$CILow
agrID$YMax = agrID$response + agrID$CIHigh
agrID


agrID.southern = subset(agrID,socialInfo=="southern")

#bar graph
ggplot(agrID.southern, aes(x=socialCondition,y=response,fill=socialCondition)) +
  geom_bar(stat="identity",width=.5,position=position_dodge(.9)) +
  #scale_x_discrete(labels=c("non-southern", "southern")) +
  scale_fill_manual(labels=c("Democrat","Republican"),values=c("#56B4E9","#D55E00"))+
  xlab("Question type")+
  ylab("Mean rati'ng")+
  ggtitle("Southern locations")+
  #theme(axis.text.x=element_text(angle = 45),legend.position="left")+
  labs(fill="Question Type")+
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=position_dodge(.9))+
  facet_wrap(~AllSpeakerInfo, nrow=3,labeller=as_labeller(agrID.southern$SpeakerState))


agrID.nonsouthern = subset(agrID,socialInfo=="non-southern")
#bar graph
ggplot(agrID.nonsouthern, aes(x=socialCondition,y=response,fill=socialCondition)) +
  geom_bar(stat="identity",width=.5,position=position_dodge(.9)) +
  #scale_x_discrete(labels=c("non-southern", "southern")) +
  scale_fill_manual(labels=c("Democrat","Republican"),values=c("#56B4E9","#D55E00"))+
  xlab("Question type")+
  ylab("Mean rating")+
  ggtitle("Non-southern Locations")+
  #theme(axis.text.x=element_text(angle = 45),legend.position="left")+
  labs(fill="Question Type")+
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=position_dodge(.9))+
  facet_wrap(~AllSpeakerInfo, nrow=3)

#boxplots
#subset for southern/non-southern
cd.southern = subset(cd, socialInfo=="southern")

ggplot(cd.southern, aes(x=socialInfo, y=response,fill=socialCondition)) + 
  geom_boxplot()+
  scale_fill_manual(labels=c("Democrat","Republican"),values=c("#56B4E9", "tomato1"))+
  xlab("Political orientation of complement")+
  labs(fill="Question type")+
  facet_wrap(~AllSpeakerInfo)


cd.nonsouthern = subset(cd, socialInfo=="non-southern")

ggplot(cd.nonsouthern, aes(x=socialInfo, y=response,fill=socialCondition)) + 
  geom_boxplot()+
  scale_fill_manual(labels=c("Democrat","Republican"),values=c("#56B4E9", "tomato1"))+
  xlab("Political orientation of complement")+
  labs(fill="Question type")+
  facet_wrap(~AllSpeakerInfo)


#find the usable items by creatinglooking at mean diffs for each topic
agrID.byTopic = agrID %>%
  pivot_wider(names_from = socialCondition, values_from = c(response,CILow,CIHigh,YMin,YMax)) %>%
  mutate(meanR_minus_meanD = response_R-response_D)

agrID.byTopic.southern = subset(agrID.byTopic,socialInfo=="southern")
agrID.byTopic.non_southern = subset(agrID.byTopic,socialInfo=="non-southern")

mean(agrID.byTopic.southern$meanR_minus_meanD)  #0.3760625
median(agrID.byTopic.southern$meanR_minus_meanD)  #0.3688333


mean(agrID.byTopic.non_southern$meanR_minus_meanD)  #-0.3736875
median(agrID.byTopic.non_southern$meanR_minus_meanD)  #-0.3566667


#look at participant politics
#agrPolitics= aggregate(response ~ socialCondition+orientation+politics, data=cd, FUN="mean")
#agrPolitics$CILow = aggregate(response ~ socialCondition+orientation+politics, data=cd, FUN="ci.low")$response
#agrPolitics$CIHigh = aggregate(response ~ socialCondition+orientation+politics, data=cd, FUN="ci.high")$response
#agrPolitics$YMin = agrPolitics$response - agrPolitics$CILow
#agrPolitics$YMax = agrPolitics$response + agrPolitics$CIHigh
#agrPolitics



