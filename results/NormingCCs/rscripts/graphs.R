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
cd = droplevels(subset(cd,trigger_class!="MC"))
agr= aggregate(response ~ orientation+socialCondition, data=cd, FUN="mean")
agr$CILow = aggregate(response ~ orientation+socialCondition, data=cd, FUN="ci.low")$response
agr$CIHigh = aggregate(response ~ orientation+socialCondition, data=cd, FUN="ci.high")$response
agr$YMin = agr$response - agr$CILow
agr$YMax = agr$response + agr$CIHigh
agr

#look at the overall responses
ggplot(agr, aes(x=orientation,y=response,fill=socialCondition),xpd=FALSE) +
  geom_bar(stat="identity",width=.5,position=position_dodge(.9)) +
  scale_fill_manual(labels=c("Democrat","Republican"),values=c("#56B4E9", "tomato1"))+
  scale_x_discrete(labels=c("Left", "Right")) +
  xlab("Political orientation of complement") +
  ylab("Mean response") + 
  ylim(0,1)+
  labs(fill="Question Type")+
  theme(text = element_text(size=16))+
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=position_dodge(.9))

#look at mean responses to individual CCs, by topic
agrID= aggregate(response ~ orientation+socialCondition+topic, data=cd, FUN="mean")
agrID$CILow = aggregate(response ~ orientation+socialCondition+topic, data=cd, FUN="ci.low")$response
agrID$CIHigh = aggregate(response ~ orientation+socialCondition+topic, data=cd, FUN="ci.high")$response
agrID$YMin = agrID$response - agrID$CILow
agrID$YMax = agrID$response + agrID$CIHigh
agrID


#bar graph
ggplot(agrID, aes(x=orientation,y=response,fill=socialCondition)) +
  geom_bar(stat="identity",width=.5,position=position_dodge(.9)) +
  scale_x_discrete(labels=c("conservative", "liberal")) +
  scale_fill_manual(labels=c("Democrat","Republican"),values=c("#56B4E9","#D55E00"))+
  ylab("Mean rating")+
  theme(legend.position="bottom",axis.title.x = element_blank())+
  labs(fill="Question Type")+
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25,position=position_dodge(.9))+
  facet_wrap(~topic, nrow=6)

ggsave(f="../graphs/CCbarGraphsForDoc.png",height=7,width=7)


#boxplots
ggplot(cd, aes(x=orientation, y=response,fill=socialCondition)) + 
  geom_boxplot()+
  scale_fill_manual(labels=c("Democrat","Republican"),values=c("#56B4E9", "tomato1"))+
  xlab("Political orientation of complement")+
  labs(fill="Question type")+
  facet_wrap(~topic)

customQuantiles<- function(responses){
  quantile(responses,c(.25,.30,.35,.40,.45,.50,.55,.60,.65,.70,.75))
}



#find the usable items by looking at mean diffs for each topic
agrID.byTopic = agrID %>%
  pivot_wider(names_from = c(socialCondition,orientation), values_from = c(response,CILow,CIHigh,YMin,YMax)) %>%
  mutate(conservative_meanR_minus_meanD = response_R_conservative-response_D_conservative)
agrID.byTopic
#agrID.byTopic.conservative = subset(agrID.byTopic,orientation=="conservative")
#agrID.byTopic.liberal = subset(agrID.byTopic,orientation=="liberal")

#mean(agrID.byTopic.conservative$meanR_minus_meanD)  #0.4551162
#median(agrID.byTopic.liberal$meanR_minus_meanD)  #-0.5598333


#mean(agrID.byTopic.non_southern$meanR_minus_meanD)  #-0.3736875
#median(agrID.byTopic.non_southern$meanR_minus_meanD)  #-0.3566667


#look at participant politics
#agrPolitics= aggregate(response ~ socialCondition+orientation+politics, data=cd, FUN="mean")
#agrPolitics$CILow = aggregate(response ~ socialCondition+orientation+politics, data=cd, FUN="ci.low")$response
#agrPolitics$CIHigh = aggregate(response ~ socialCondition+orientation+politics, data=cd, FUN="ci.high")$response
#agrPolitics$YMin = agrPolitics$response - agrPolitics$CILow
#agrPolitics$YMax = agrPolitics$response + agrPolitics$CIHigh
#agrPolitics



