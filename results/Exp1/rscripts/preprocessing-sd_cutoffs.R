#identifying CCs that display strong associations w/Democrat/Republican parties
#preprocessing - based on Judith Tonhauser's code 

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)



source('../../helpers.R')

# load required packages for pre-processing data
library(tidyverse)
library(Hmisc)

# read in the raw data
d = read_csv("../data/experiment-trials.csv")
d1 = read_csv("../data/experiment-trials-1participant.csv")
d = rbind(d, d1)
length(unique(d$workerid)) #243
d = droplevels(subset(d,(orientation=="conservative" | orientation == "liberal" | orientation == "N" | is.na(orientation)) )) 
length(unique(d$workerid)) #240  after removing 3 for programming error


exps = unique(d$list_whichSpPoliticsFirst)
for(exp in exps){
  subdata = subset(d,list_whichSpPoliticsFirst==exp)
  participants=unique(subdata$workerid)
  count = length(participants)
  #print(paste0("List",exp,"\n"))
  print(exp)
  print(count)
}


block1 <- droplevels(subset(d,block=="block1"))
block1 = block1[,!sapply(block1, function(x) mean(is.na(x)))==1]
block2 <- droplevels(subset(d,block=="block2"))
block2 = block2[,!sapply(block2, function(x) mean(is.na(x)))==1]
block3 <- droplevels(subset(d,block=="block3"))
block3 = block3[,!sapply(block3, function(x) mean(is.na(x)))==1]



d = block1 %>%
  full_join(block2,by=c("workerid","CC","proliferate.condition","SpeakerGender","file","item_type","list","list_genderMaxes","list_whichSpPoliticsFirst","matrix_clause","name","orientation","predicate","socialInfo","speakerGendersMaxes","whichSpPoliticsFirst","topic"))


names(d)[names(d) == 'rt.x'] <- 'rt_block1'
names(d)[names(d) == 'rt.y'] <- 'rt_block2'

names(d)[names(d) == 'slide_number_in_experiment.x'] <- 'slide_number_block1'
names(d)[names(d) == 'slide_number_in_experiment.y'] <- 'slide_number_block2'

names(d)[names(d) == 'block.x'] <-'block1'
names(d)[names(d) == 'block.y'] <-'block2'


d = d %>%
  full_join(block3,by=c("workerid","CC","proliferate.condition","SpeakerGender","file","item_type","list","list_genderMaxes","list_whichSpPoliticsFirst","matrix_clause","name","orientation","predicate","socialInfo","speakerGendersMaxes","whichSpPoliticsFirst","topic"))


names(d)[names(d) == 'rt'] <- 'rt_block3'
names(d)[names(d) == 'slide_number_in_experiment'] <- 'slide_number_block3'
names(d)[names(d) == 'block'] <-'block3'



# make a trial number
d <- d %>%
  mutate(trial = ifelse(block1=="block1",slide_number_block1-3, #subtract 3 initial slides
                         ifelse(block2=="block2",slide_number_block2-3-1,#subtract 3 initial slides and instructions for block 2
                                 ifelse(block3=="block3",slide_number_block3-3-1-1,"NA")) #subtract 3 initial slides and instructions for block 2 and block 3
                         )
  )
      


ds = read_csv("../data/experiment-subject_information.csv")
ds1=read_csv("../data/experiment-subject_information-1participant.csv")
ds = rbind(ds,ds1)

# merge subject information into data
d = d %>%
  left_join(ds, by=c("workerid"))

# how many participants?
length(unique(d$workerid)) #243

# look at Turkers' comments
unique(ds$comments)


# participant info
table(d$age) 
d$age[d$age == "2o"] <- 20   #someone used an "o" instead of a 0
d$age[d$age == "1987"] <- 34   #someone put birth year instead of age
d$age <- as.numeric (d$age)
length(which(is.na(d$age))) # 24 missing values - this is one person
range(d$age,na.rm=TRUE)#18-83
median(d$age,na.rm=TRUE) #34




d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarise(count=n())
#124 female, 113 male, 3 non-binary






# exclude non-American English speakers
length(unique(d$workerid))# X
length(which(is.na(d$american))) #0 (everybody responded)
table(d$american) #everyone said yes


#  other languages
table(ds$language) 

#d <- d %>%
#  filter(language != "Chinese" & language != "Russian" & language != "telugu") %>%  #droplevels()
#length(unique(d$workerid)) # (data from 0 Turker excluded, 42 remaining Turkers)






##############################################
##################################################
#####Exclude workers based on attention checks####
##################################################
##############################################


##############################################
#####Block 1 - objective attention checks####
#############################################
names(d)
d.Objective <- d %>%
  filter(item_type== "O") %>%
  droplevels()
nrow(d.Objective) #120 (42 Turkers x 2 MCs)

#attention check 1: capital of US
# this is the false attention check, so responses are expected to be low
table(d$topic)
d.Objective.attention_capital <- d.Objective %>%
  filter(topic == "capital") %>%
  droplevels()
nrow(d.Objective.attention_capital) #42 (42 instances of attention check 1; 1 for each worker)

# group mean for attention check 1
round(mean(d.Objective.attention_capital$participant_beliefs),2) #.01


Block1_FalseAttentionCheck<-ggplot(d.Objective.attention_capital, aes(x=workerid,y=participant_beliefs)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("response to false attention check: capital")


Block1_FalseAttentionCheck
#ggsave(Block1_FalseAttentionCheck,filename="../graphs/DataExclusion/Block1_FalseAttentioncheck.png")


#attention check 2: pres. elections
# this is the true attention check, so  responses are expected to be high
table(d$topic)
d.Objective.attention_elections <- d.Objective %>%
  filter(topic == "elections") %>%
  droplevels()
nrow(d.Objective.attention_elections) #240 ( 1 for each worker)

# group mean for attention check 2
round(mean(d.Objective.attention_elections$participant_beliefs),2) #.97

Block1_TrueAttentionCheck <- ggplot(d.Objective.attention_elections, aes(x=workerid,y=participant_beliefs)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("mean response to true attention check:  elections")

Block1_TrueAttentionCheck
#ggsave(Block1_TrueAttentionCheck,filename="../graphs/DataExclusion/Block1_TrueAttentioncheck.png")


# get the Turkers who are more than 2 standard deviations above the mean on attention check 1
f <-d.Objective.attention_capital[d.Objective.attention_capital$participant_beliefs > (mean(d.Objective.attention_capital$participant_beliefs) + 2*sd(d.Objective.attention_capital$participant_beliefs)),]
length(unique(f$workerid)) #7


# get the Turkers who are more than 2 standard deviations below the mean on attention check 2
t<-d.Objective.attention_elections[d.Objective.attention_elections$participant_beliefs < (mean(d.Objective.attention_elections$participant_beliefs) - 2*sd(d.Objective.attention_elections$participant_beliefs)),]
length(unique(t$workerid)) #7


# look at the items that these "outlier" Turkers did
# make data subset of just the outliers
outliers_objective <- d.Objective.attention_elections %>%
  filter(workerid %in% f$workerid | workerid %in% t$workerid)
outliers_objective = droplevels(outliers_objective)
unique(outliers_objective$workerid) 
length(unique(outliers_objective$workerid)) #14

#14 unique participants are excluded based on responses to objective attention checks

###############################################
#####Block 2 -Democrat/Republican responses####
###############################################
names(d)
d.block2 <- d %>%
  filter(block2== "block2") %>%
  droplevels()
nrow(d.block2) #3840 (240 workers x 16 trials)

d.DemocratSpeaker <- d.block2 %>%
  filter(socialInfo=="D") %>%
  droplevels()

round(mean(d.DemocratSpeaker$republican_rating),2) #.06
round(mean(d.DemocratSpeaker$democrat_rating),2) #.94

#responses for Democrat speakers
#are the responses negatively correlated with each other?
Block2_DemocratRating_RepublicanRating_DemocratSpeaker<-ggplot(d.DemocratSpeaker, aes(x=democrat_rating,y=republican_rating)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("republican rating")+
  ggtitle("correlation b/t democrat and republican ratings for Democrat speakers")
Block2_DemocratRating_RepublicanRating_DemocratSpeaker

#ggsave(Block2_DemocratRating_RepublicanRating_DemocratSpeaker,filename="../graphs/DataExclusion/Block2_DemocratRating~RepublicanRating_DemocratSpeakers.png")



d.DemocratSpeaker$reverseDemRating <- 1-d.DemocratSpeaker$democrat_rating
d.DemocratSpeaker$avg_Rep_reverseDemrating <- (d.DemocratSpeaker$reverseDemRating+d.DemocratSpeaker$republican_rating)/2

Block2_avg_RepRating_reverseDemRating_DemSpeaker_byitem <- ggplot(d.DemocratSpeaker, aes(x=workerid,y=avg_Rep_reverseDemrating)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("(avg(rep. rating, reversed dem rating)\nindividual items")+
  ggtitle("Democrat speaker")

Block2_avg_RepRating_reverseDemRating_DemSpeaker_byitem 
#ggsave(Block2_avg_RepRating_reverseDemRating_byitem,filename="../graphs/DataExclusion/Block2_avg(RepRating,reversedDemRating)_DemocratSpeakers_byItem.png")


#responses for Republican Speakers
d.RepublicanSpeaker <- d.block2 %>%
  filter(socialInfo=="R") %>%
  droplevels()

round(mean(d.RepublicanSpeaker$republican_rating),2) #.93
round(mean(d.RepublicanSpeaker$democrat_rating),2) #.07

#are the responses negatively correlated wiht eachother?
Block2_DemocratRating_RepublicanRating_RepublicanSpeaker<-ggplot(d.RepublicanSpeaker, aes(x=democrat_rating,y=republican_rating)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("republican rating")+
  ggtitle("correlation b/t democrat and republican ratings for Republican speakers")


#ggsave(Block2_DemocratRating_RepublicanRating_RepublicanSpeaker,filename="../graphs/DataExclusion/Block2_DemocratRating~RepublicanRating_RepublicanSpeakers.png")

d.RepublicanSpeaker$reverseDemRating <- 1-d.RepublicanSpeaker$democrat_rating
d.RepublicanSpeaker$avg_Rep_reverseDemrating <- (d.RepublicanSpeaker$reverseDemRating+d.RepublicanSpeaker$republican_rating)/2

Block2_DemocratRating_RepublicanRating_RepublicanSpeaker_byitem <-ggplot(d.RepublicanSpeaker, aes(x=workerid,y=avg_Rep_reverseDemrating)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("(avg(rep. rating, reversed dem rating)\nindividual items")+
  ggtitle("Republican speakers")

Block2_DemocratRating_RepublicanRating_RepublicanSpeaker_byitem
#ggsave(Block2_avg_RepRating_reverseDemRating_byitem,filename="../graphs/DataExclusion/Block2_avg(RepRating,reversedDemRating)_RepubicanSpeakers_byItem.png")



#look at mean (recoded) responses to Dem and Rep speakers to filter outliers
DemSpeaker.means = d.DemocratSpeaker %>%
  group_by(workerid) %>%
  summarise(Mean = mean(avg_Rep_reverseDemrating), CI.Low=ci.low(avg_Rep_reverseDemrating), CI.High=ci.high(avg_Rep_reverseDemrating)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High)

Block2_DemocratRating_RepublicanRating_DemocratSpeaker_meansBySpeaker<-ggplot(DemSpeaker.means, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("avg 'Republican' response by worker:\n (1-Democrat_response + RepublicanResponse)/2 \nfor Democrat speakers")

Block2_DemocratRating_RepublicanRating_DemocratSpeaker_meansBySpeaker
#ggsave(Block2_DemocratRating_RepublicanRating_DemocratSpeaker_meansBySpeaker,filename="../graphs/DataExclusion/Block2_avg(RepRating,reversedDemRating)_DemocratSpeakers_meansBySpeaker.png")

demSpeaker.outliers <-DemSpeaker.means[DemSpeaker.means$Mean>(mean(DemSpeaker.means$Mean) + 2*sd(DemSpeaker.means$Mean)),]
length(unique(demSpeaker.outliers$workerid))  #15



RepSpeaker.means = d.RepublicanSpeaker %>%
  group_by(workerid) %>%
  summarise(Mean = mean(avg_Rep_reverseDemrating), CI.Low=ci.low(avg_Rep_reverseDemrating), CI.High=ci.high(avg_Rep_reverseDemrating)) %>%
  ungroup() %>%
  mutate(YMin = Mean-CI.Low, YMax = Mean+CI.High)

Block2_DemocratRating_RepublicanRating_RepublicanSpeaker_meansBySpeaker<-ggplot(RepSpeaker.means, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("'Republican' response by worker:\n (1-Democrat_response + RepublicanResponse)/2 \nfor Republican speakers")
Block2_DemocratRating_RepublicanRating_RepublicanSpeaker_meansBySpeaker
#ggsave(Block2_DemocratRating_RepublicanRating_RepublicanSpeaker_meansBySpeaker,filename="../graphs/DataExclusion/Block2_avg(RepRating,reversedDemRating)_RepubicanSpeakers_meansBySpeaker.png")


repSpeaker.outliers <-RepSpeaker.means[RepSpeaker.means$Mean<(mean(RepSpeaker.means$Mean) - 2*sd(RepSpeaker.means$Mean)),]
length(unique(repSpeaker.outliers$workerid))  #15

block2outliers <- c(demSpeaker.outliers$workerid,repSpeaker.outliers$workerid)
length(unique(block2outliers))  #17



##################################
#####Block 3-MC responses#######
##################################
d.MC <- d %>%
  filter(item_type == "MC" & block3 == "block3") %>%
  droplevels()
nrow(d.MC) #3504 (240 * 2 MCs)

round(mean(d.MC$projection),2) #.91

ggplot(d.MC, aes(x=workerid,y=projection)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("projection responses to MCs")

projection_socialInfo_MCs<-ggplot(subset(d,(item_type=="MC" & block3=="block3")), aes(x=orientation,y=projection,fill=socialInfo))+
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  #stat_summary(aes(label=round(..y..,2)), fun=mean, geom="text", size=4, vjust = -0.5)+
  scale_fill_manual(labels=c("Democrat speaker","Republican speaker"),values=c("#56B4E9", "tomato1"))+
  xlab("MC orientation") +
  ylab("Mean certainty rating") + 
  scale_x_discrete(labels=c("conservative", "liberal")) +
  ylim(0,1)+
  theme(legend.position = "right",axis.text=element_text(size=10),axis.title=element_text(size=12,face="bold"),legend.title=element_blank(),legend.text=element_text(size=12))#+
#annotate("text", x = 1:1.8, y = .68, label = "*",size=10)+
#annotate("text", x = 2.05:2.8, y = .68, label = "*",size=10)


projection_socialInfo_MCs
ggsave(projection_socialInfo_MCs,filename="../graphs/Non-critical_items/Block3_projection~socialInfo_MCs.png")
  
#DID NOT DO THE BELOW 
#since responses are a bit diff. across conditions, we'll aggregate by condition and then exclude
# MC_conservative_Rep <- subset(d.MC,(orientation=="conservative" & socialInfo=="R"))
# block3.outliers.1 <-MC_conservative_Rep[MC_conservative_Rep$projection < (mean(MC_conservative_Rep$projection) - 2*sd(MC_conservative_Rep$projection)),]
# length(block3.outliers.1$workerid) #6 workers
# 
# 
# MC_conservative_Dem <- subset(d.MC,(orientation=="conservative" & socialInfo=="D"))
# block3.outliers.2 <-MC_conservative_Dem[MC_conservative_Dem$projection < (mean(MC_conservative_Dem$projection) - 2*sd(MC_conservative_Dem$projection)),]
# length(block3.outliers.2$workerid) #11 workers
# 
# MC_liberal_Rep <- subset(d.MC,(orientation=="liberal" & socialInfo=="R"))
# block3.outliers.3 <-MC_liberal_Rep[MC_liberal_Rep$projection < (mean(MC_liberal_Rep$projection) - 2*sd(MC_liberal_Rep$projection)),]
# length(block3.outliers.3$workerid) #8 workers
# 
# 
# MC_liberal_Dem <- subset(d.MC,(orientation=="liberal" & socialInfo=="D"))
# block3.outliers.4 <-MC_liberal_Dem[MC_liberal_Dem$projection < (mean(MC_liberal_Dem$projection) - 2*sd(MC_liberal_Dem$projection)),]
# length(block3.outliers.4$workerid) #4 workers
# 
# 
# MC_outliers = c(block3.outliers.1$workerid,block3.outliers.2$workerid,block3.outliers.3$workerid,block3.outliers.4$workerid)
# length(unique(MC_outliers))  #25 workers

###########################
### Final Exclusion #######
##########################

#since there is a pattern to MC data, don't exclude based on it. 
outliersAcrossTasks = c(outliers_objective$workerid,block2outliers)
length(unique(outliersAcrossTasks))  #28 unique workers to exclude

# exclude all outliers identified above
d <- d %>%
    filter(!(workerid %in% outliersAcrossTasks)) %>%
  droplevels()
length(unique(d$workerid)) 
#######################
###Demographics########
########################

# age and gender of remaining participants
table(d$age) 
range(d$age,na.rm=TRUE)#18-83
length(which(is.na(d$age))) # 24 missing values
median(d$age,na.rm=TRUE) #34

gender <- d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarise(count=n())
#111 female, 99 male, 2 NB
gender

genderPercent <- d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarise(count=n()) %>%
  mutate(freq = round(count / sum(count), 4))

genderPercent

party <- d %>% 
  select(party, workerid) %>% 
  unique() %>% 
  group_by(party) %>% 
  summarise(count=n())


partyPercent <- d %>% 
  select(party, workerid) %>% 
  unique() %>% 
  group_by(party) %>% 
  summarise(count=n()) %>%
  mutate(freq = round(count / sum(count), 3))
partyPercent

unique(d$otherParty)


states <- d %>% 
  select(state, workerid) %>% 
  unique() %>% 
  group_by(state) %>% 
  summarise(count=n()) %>%
  mutate(freq = round(count / sum(count), 3))

states

current_state_map <- plot_usmap(data = states, values = "count", color = "black") + 
  scale_fill_continuous(low = "gray98", high = "black", name = "Number of participants\ncurrently living in each state", breaks=c(0,5,10,15,20,25,30),limits=c(0,30),labels = c(0,5,10,15,20,25,30),na.value="white") + 
  labs(title = "Number of participants in each state") +
  theme(legend.position = "right")

current_state_map

ggsave(current_state_map,filename="../graphs/participantCurrentStates.png",width=5,height=4)

politicsRatings <- droplevels(subset(d, politics!="NA"))
range(politicsRatings$politics)
median(politicsRatings$politics)

politicsRatingDist <- ggplot(subset(d,politics!="NA"), aes(x=politics)) +
  geom_histogram(bins=50) +
  xlab("Politics rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=.1))



politicsRatingDist
ggsave(politicsRatingDist,filename="../graphs/participantPoliticsRatingDist.png")

###income level
incomeDist <- d %>% 
  select(income, workerid) %>% 
  unique() %>% 
  group_by(income) %>% 
  summarise(count=n()) %>%
  mutate(freq = round(count / sum(count), 3))
incomeDist

#education
educationDist <- d %>% 
  select(education, workerid) %>% 
  unique() %>% 
  group_by(education) %>% 
  summarise(count=n()) %>%
  mutate(freq = round(count / sum(count), 3))
educationDist

#race
d$race_white[d$race_white == 1] <- "white"
d$race_white[d$race_white == 0] <- NA
d$race_asian[d$race_asian == 1] <- "asian"
d$race_asian[d$race_asian == 0] <- NA
d$race_black[d$race_black == 1] <- "black"
d$race_black[d$race_black == 0] <- NA
d$race_latino[d$race_latino == 1] <- "latino"
d$race_latino[d$race_latino == 0] <- NA
d$race_native[d$race_native == 1] <- "native"
d$race_native[d$race_native == 0] <- NA
d$race_otherRaceCheck[d$race_otherRaceCheck == 1] <- "other"
d$race_otherRaceCheck[d$race_otherRaceCheck == 0] <- NA

colsToPaste<-c("race_white","race_black","race_asian","race_latino","race_native","race_otherRaceCheck")

d$raceCollapsed <-apply(d[, colsToPaste], 1, function(x) toString(na.omit(x)))

d$multipleRaces <- 0
d$multipleRaces["," %in% d$raceCollapsed] <-1

raceDist <- d %>% 
  select(raceCollapsed, workerid) %>% 
  unique() %>% 
  group_by(raceCollapsed) %>% 
  summarise(count=n()) %>%
  mutate(freq = round(count / sum(count), 3))
raceDist

#raceCollapsed                 count  freq
#<chr>                         <int> <dbl>
#  1 ""                                2 0.009
#2 "asian"                           7 0.033   **
#3 "asian, latino"                   1 0.005
#4 "black"                          11 0.052    **
#5 "black, asian"                    1 0.005
#6 "black, latino, native"           2 0.009
#7 "latino"                          6 0.028    ***
#8 "other"                           1 0.005
#9 "white"                         162 0.764
#10 "white, asian"                    3 0.014
#11 "white, black"                    4 0.019
#12 "white, black, asian, native"     1 0.005
#13 "white, latino"                   6 0.028
#14 "white, latino, native"           1 0.005
#15 "white, native"                   3 0.014
#16 "white, other"                    1 0.005

#check how many per list + social condition combo
exps = unique(d$list_whichSpPoliticsFirst)
for(exp in exps){
  subdata = subset(d,list_whichSpPoliticsFirst==exp)
  participants=unique(subdata$workerid)
  count = length(participants)
  #print(paste0("List",exp,"\n"))
  print(exp)
  print(count)
}


write.csv(d, file="../data/data_preprocessed.csv",row.names=F,quote=T)

