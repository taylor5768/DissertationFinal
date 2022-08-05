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

source('../../helpers.R')

cd = read.csv("../data/data_preprocessed.csv")

certaintyRatingDist_topic <- ggplot(subset(cd,item_type=="critical" & projection!="NA" &orientation!="N"), aes(x=projection)) +
  geom_histogram(bins=50) +
  xlab("Certainty rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=.1))+
  facet_wrap(~topic+orientation)

certaintyRatingDist_topic
ggsave(certaintyRatingDist_topic,filename="../graphs/modelingAssumptions/certaintyRatingDist_topic.png",width=8,height=11,units="in")

certaintyRatingDist_socialInfo <- ggplot(subset(cd,item_type=="critical" & projection!="NA" &orientation!="N"), aes(x=projection)) +
  geom_histogram(bins=50) +
  xlab("Certainty rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=.1))+
  facet_wrap(~orientation+socialInfo)

ggsave(certaintyRatingDist_socialInfo,filename="../graphs/modelingAssumptions/certaintyRatingDist_socialInfo.png")

cd.RepublicanParticipants <- subset(cd,party=="Rep")

ggplot(subset(cd.RepublicanParticipants,item_type=="critical" & projection!="NA" &orientation!="N"), aes(x=projection)) +
  geom_histogram(bins=50) +
  xlab("Certainty rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=.1))+
  facet_wrap(~orientation+socialInfo)

cd.DemocratParticipants <- subset(cd,party=="Dem")
ggplot(subset(cd.DemocratParticipants,item_type=="critical" & projection!="NA" &orientation!="N"), aes(x=projection)) +
  geom_histogram(bins=50) +
  xlab("Certainty rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=.1))+
  facet_wrap(~orientation+socialInfo)

ggplot(subset(cd,item_type=="critical" & projection!="NA" &orientation!="N" & (party=="Dem" | party=="Rep")), aes(x=projection)) +
  geom_histogram(bins=50) +
  xlab("Certainty rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=.1))+
  facet_wrap(~orientation+socialInfo+party)

certainty_dist_partGender <- ggplot(subset(cd,item_type=="critical" & projection!="NA" &orientation!="N" & (gender == "Male" |gender =="Female") ), aes(x=projection)) +
  geom_histogram(bins=50) +
  xlab("Certainty rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=.1))+
  facet_wrap(~gender)

certainty_dist_partGender

ggsave(certainty_dist_partGender,filename="../graphs/modelingAssumptions/certaintyRatingDist_partGender.png")

certainty_dist_partGender_predicate <- ggplot(subset(cd,item_type=="critical" & projection!="NA" &orientation!="N" & (gender == "Male" |gender =="Female")), aes(x=projection)) +
  #geom_histogram(aes(y=(stat(count)/sum(stat(count)))*100))+
  #geom_histogram(aes(y=(stat(count)/sum(stat(count)))))+
  geom_histogram(bins=50) +
  xlab("Certainty rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=.1))+
  facet_wrap(~predicate+gender)

certainty_dist_partGender_predicate


