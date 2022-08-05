this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)

setwd(this.dir)

source('../helpers.R')
library(tidyverse)



#read in dfs for each exp
d1 <- read.csv("../Exp1/data/data_preprocessed.csv")
d2 <-read.csv("../Exp2/data/data_preprocessed.csv")
d3 <- read.csv("../Exp3/data/data_preprocessed.csv")
nrow(d1)
nrow(d2)
nrow(d3)
nrow(d1)+nrow(d2)+nrow(d3)

#add column specifying each exp.
d1$experiment <- "Exp1"
d2$experiment <- "Exp2"
d3$experiment <- "Exp3"

#merge dfs into single df
allData <- merge(d1,d2,all=TRUE)
allData <- merge(allData,d3,all=TRUE)
nrow(allData)

allData$singleSocialInfo <- ""
allData$singleSocialInfo[allData$socialInfo == "N" |allData$socialInfo == "D" ] <- "D"
allData$singleSocialInfo[allData$socialInfo == "S" | allData$socialInfo == "R"] <- "R"

allData$topic[allData$topic=="covid"] <- "COVID"
allData_sub <- droplevels(subset(allData,(item_type=="critical")))
allData_sub <- droplevels(subset(allData_sub,(orientation!="N")))
allData_sub$topic <- as.factor(allData_sub$topic)
levels(allData_sub$topic)

exp_names <- 
  c('Exp1'= 'Experiment 1','Exp2'='Experiment 2', 'Exp3' = 'Experiment 3')

#overall
projection_socialInfo_political<-
  ggplot(subset(allData,(orientation!="N" & item_type=="critical")), aes(x=orientation,y=projection,fill=singleSocialInfo),xpd=FALSE) +
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  #stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", size=4, vjust = -0.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Democrat/\nnon-Southern speaker","Republican/\nSouthern speaker"),values=c("#56B4E9","#D55E00"))+
  xlab("CC orientation")+
  ylab("Mean certainty rating")+
  #theme(legend.position="left")+
  labs(fill="Social Info:")+
  ylim(0,1)+
  theme(legend.position="bottom",axis.text.x=element_text(size=10,angle=45,hjust=1),axis.title=element_text(size=10,face="bold"),legend.text=element_text(size=10),aspect.ratio=1,legend.title=element_text(face="bold"))+
  #facet_wrap(~experiment)
  facet_wrap(~experiment,labeller = labeller(experiment=exp_names))
#annotate("text", x = 1:1.8, y = .68, label = "*",size=10)+
#annotate("text", x = 2.05:2.8, y = .68, label = "*",size=10)


projection_socialInfo_political
ggsave(projection_socialInfo_political,filename="graphs/projection~socialInfo_Political.png",width=5,height=5)


###perceived speaker beliefs as a function of social information

#conservative CCs
conservativeCCs = allData %>% 
  filter(item_type == "critical" & orientation =="conservative") %>% 
  droplevels()
nrow(conservativeCCs) 


means = conservativeCCs %>%
  group_by(singleSocialInfo,topic,experiment) %>%
  summarise(Mean=mean(spBelief_rating),CILow=ci.low(spBelief_rating),CIHigh=ci.high(spBelief_rating)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
means


high = means %>%
  filter(singleSocialInfo == "R") %>%
  mutate(eventItem = fct_reorder(topic,Mean))

means = means %>%
  mutate(topic = fct_relevel(topic,levels(high$topic))) %>% 
  mutate(singleSocialInfo = fct_relevel(singleSocialInfo,"D"))
means

subjmeans = conservativeCCs %>%
  group_by(topic,workerid,singleSocialInfo) %>%
  summarise(Mean = mean(spBelief_rating)) %>% 
  ungroup() %>% 
  mutate(singleSocialInfo = fct_relevel(as.factor(as.character(singleSocialInfo)),"D"))
subjmeans$topic <- factor(subjmeans$topic, levels = unique(levels(means$topic)))
levels(subjmeans$topic)
names(subjmeans)

ggplot(means, aes(x=topic, y=Mean, color=singleSocialInfo,shape=singleSocialInfo,fill=singleSocialInfo)) + 
  geom_point(data=subjmeans,aes(fill=singleSocialInfo,color=singleSocialInfo),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_shape_manual(values=c(25, 24),labels=c("Democrat/non-Southern","Republican/Southern"),name="Social info:") +
  scale_fill_manual(values=c("#56B4E9","tomato"),labels=c("Democrat/non-Southern","Republican/Southern"),name="Social info:") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,.2,.4,.6,.8,1), labels=c("0", ".2", ".4", ".6", ".8", "1")) +
  scale_color_manual(name="Social info:",labels=c("Democrat/non-Southern", "Republican/Southern"), values=c("#56B4E9","tomato")) +
  coord_flip() +
  theme(legend.position = "bottom", legend.text=element_text(size=12)) +
  # theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  ylab("Mean perceived speaker belief rating") +
  xlab("Topic")+
  ggtitle("conservative CCs")+
  facet_wrap(~experiment,labeller = labeller(experiment=exp_names))
ggsave(f="graphs/conservativeCC-speakerBelief-ratings-byTopic.png",height=5,width=8)

#liberal CCs
liberalCCs = allData %>% 
  filter(item_type == "critical" & orientation =="liberal") %>% 
  droplevels()
nrow(liberalCCs) 


means = liberalCCs %>%
  group_by(singleSocialInfo,topic,experiment) %>%
  summarise(Mean=mean(spBelief_rating),CILow=ci.low(spBelief_rating),CIHigh=ci.high(spBelief_rating)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
means


high = means %>%
  filter(singleSocialInfo == "R") %>%
  mutate(eventItem = fct_reorder(topic,Mean))

means = means %>%
  mutate(topic = fct_relevel(topic,levels(high$topic))) %>% 
  mutate(singleSocialInfo = fct_relevel(singleSocialInfo,"D"))
means

subjmeans = liberalCCs %>%
  group_by(topic,workerid,singleSocialInfo) %>%
  summarise(Mean = mean(spBelief_rating)) %>% 
  ungroup() %>% 
  mutate(singleSocialInfo = fct_relevel(as.factor(as.character(singleSocialInfo)),"D"))
subjmeans$topic <- factor(subjmeans$topic, levels = unique(levels(means$topic)))
levels(subjmeans$topic)
names(subjmeans)

ggplot(means, aes(x=topic, y=Mean, color=singleSocialInfo,shape=singleSocialInfo,fill=singleSocialInfo)) + 
  geom_point(data=subjmeans,aes(fill=singleSocialInfo,color=singleSocialInfo),shape=21,alpha=.05) +
  geom_point(stroke=.5,size=3,color="black") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_shape_manual(values=c(25, 24),labels=c("Democrat/non-Southern","Republican/Southern"),name="Social info:") +
  scale_fill_manual(values=c("#56B4E9","tomato"),labels=c("Democrat/non-Southern","Republican/Southern"),name="Social info:") +
  scale_y_continuous(limits = c(0,1),breaks = c(0,.2,.4,.6,.8,1), labels=c("0", ".2", ".4", ".6", ".8", "1")) +
  scale_color_manual(name="Social info:",labels=c("Democrat/non-Southern", "Republican/Southern"), values=c("#56B4E9","tomato")) +
  coord_flip() +
  theme(legend.position = "bottom", legend.text=element_text(size=12)) +
  # theme(text = element_text(size=12), axis.text.x = element_text(size = 12, angle = 75, hjust = 1)) +
  ylab("Mean perceived speaker belief rating") +
  xlab("Topic")+
  ggtitle("liberal CCs")+
  facet_wrap(~experiment,labeller = labeller(experiment=exp_names))
ggsave(f="graphs/liberalCC-speakerBelief-ratings-byTopic.png",height=5,width=8)


#projection by speaker beliefs by experiment
projection_spBelief_overall<-ggplot(subset(allData,(spBelief_rating!="NA" & projection!="NA" & item_type=="critical")), aes(x=spBelief_rating, y=projection)) +
  geom_smooth(method="lm",colour="grey50") +
  geom_point(shape=20, size=1, alpha=.3) +
  xlab("perceived speaker belief rating") +
  ylab("Certainty rating") +
  #theme(legend.position = "top", legend.text=element_text(size=12)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1,size=3))) +
  #xlim(0,1) +
  #ylim(0,1) +
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  theme(panel.spacing.x = unit(4, "mm")) +
  coord_fixed(ratio = 1)+
  facet_wrap(~experiment,labeller = labeller(experiment=exp_names))
projection_spBelief_overall


#both speaker and listener beliefs  ###DID NOT INCLUDE
projection_bothBeliefs<-ggplot(subset(allData,(spBelief_rating!="NA" & projection!="NA" & item_type=="critical")))+
  geom_smooth(method="lm",aes(x=spBelief_rating,y=projection,color="spBelief_rating")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=spBelief_rating,y=projection,color="spBelief_rating")) +
  geom_smooth(method="lm",aes(x=participant_beliefs,y=projection,color="participant_beliefs")) +
  geom_point(shape=20, size=1, alpha=.3,aes(x=participant_beliefs,y=projection,color="participant_beliefs")) +
  scale_colour_manual("", breaks = c("spBelief_rating", "participant_beliefs"), values = c("darkgreen", "purple"),labels=c("perceived speaker belief","listener CC belief"),)+
  ylab("Certainty rating") +
  xlab("Belief rating")+
  guides(color=guide_legend(override.aes=list(fill=c("darkgreen","purple"))))+
  theme(plot.margin=unit(c(-1,0,0,0),"cm"),legend.position=c(0.5,-.45),legend.text=element_text(size=12),legend.margin = margin(t=-.5,l=.1,r=.1,b=.1,unit='cm'),axis.text=element_text(size=12),axis.title=element_text(size=12,face='bold'))+
  scale_x_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  scale_y_continuous(breaks=c(0,.5,1),labels=c("0",".5","1")) +
  coord_fixed(ratio = 1)+
  facet_wrap(~experiment,labeller = labeller(experiment=exp_names))

projection_bothBeliefs

#ggsave(projection_bothBeliefs ,filename="graphs/bothBeliefsAcrossExps.png")
