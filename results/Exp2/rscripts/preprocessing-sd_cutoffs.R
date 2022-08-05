#preprocessing data for Exp 2- based on Judith Tonhauser's code 

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)



source('../../helpers.R')

# load required packages for pre-processing data
library(tidyverse)
library(Hmisc)
library(usmap)

# read in the raw data
d = read_csv("../data/experiment-trials.csv")
#d1 = read_csv("../data/experiment-trials-1participant.csv")
#d = rbind(d, d1)
length(unique(d$workerid)) #324
d = droplevels(subset(d,(orientation=="conservative" | orientation == "liberal" | orientation == "N" | is.na(orientation)) )) 
length(unique(d$workerid)) #324


exps = unique(d$list_whichSpPoliticsFirst)
for(exp in exps){
  subdata = subset(d,list_whichSpPoliticsFirst==exp)
  participants=unique(subdata$workerid)
  count = length(participants)
  #print(paste0("List",exp,"\n"))
  print(exp)
  print(count)
}

#[1] "list2_NS"
#[1] 99
#[1] "list1_NS"
#[1] 75
#[1] "list1_S"
#[1] 75
#[1] "list2_S"
#[1] 75

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
#ds1=read_csv("../data/experiment-subject_information-1participant.csv")
#ds = rbind(ds,ds1)

# merge subject information into data
d = d %>%
  left_join(ds, by=c("workerid"))

# how many participants?
length(unique(d$workerid)) #324

# look at Turkers' comments
unique(ds$comments)


# participant info
table(d$age) 
#d$age[d$age == "2o"] <- 20   #someone used an "o" instead of a 0
#d$age[d$age == "1987"] <- 34   #someone put birth year instead of age
d$age <- as.numeric (d$age)
length(which(is.na(d$age))) # 0
range(d$age,na.rm=TRUE)#18-76
median(d$age,na.rm=TRUE) #35




d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarise(count=n())
#204 female, 110 male, 10 non-binary






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
nrow(d.Objective) #648 (324 Turkers x 2 Objective items)

#attention check 1: capital of US
# this is the false attention check, so responses are expected to be low
table(d$topic)
d.Objective.attention_capital <- d.Objective %>%
  filter(topic == "capital") %>%
  droplevels()
nrow(d.Objective.attention_capital) #243 (243 instances of attention check 1; 1 for each worker)

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
nrow(d.Objective.attention_elections) #324 ( 1 for each worker)

# group mean for attention check 2
round(mean(d.Objective.attention_elections$participant_beliefs),2) #.96

Block1_TrueAttentionCheck <- ggplot(d.Objective.attention_elections, aes(x=workerid,y=participant_beliefs)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("mean response to true attention check:  elections")

Block1_TrueAttentionCheck
#ggsave(Block1_TrueAttentionCheck,filename="../graphs/DataExclusion/Block1_TrueAttentioncheck.png")


# get the Turkers who are more than 2 standard deviations above the mean on attention check 1
f <-d.Objective.attention_capital[d.Objective.attention_capital$participant_beliefs > (mean(d.Objective.attention_capital$participant_beliefs) + 2*sd(d.Objective.attention_capital$participant_beliefs)),]
length(unique(f$workerid)) #5


# get the Turkers who are more than 2 standard deviations below the mean on attention check 2
t<-d.Objective.attention_elections[d.Objective.attention_elections$participant_beliefs < (mean(d.Objective.attention_elections$participant_beliefs) - 2*sd(d.Objective.attention_elections$participant_beliefs)),]
length(unique(t$workerid)) #11


# look at the items that these "outlier" Turkers did
# make data subset of just the outliers
outliers_objective <- d.Objective.attention_elections %>%
  filter(workerid %in% f$workerid | workerid %in% t$workerid)
outliers_objective = droplevels(outliers_objective)
outliers_objective = unique(outliers_objective$workerid) 
length(unique(outliers_objective)) #14

#14 unique participants are excluded based on responses to objective attention checks

###############################################
#####Block 2 -Location responses####
###############################################
names(d)
d.block2 <- d %>%
  filter(block2== "block2") %>%
  droplevels()
nrow(d.block2) #3840 (240 workers x 16 trials)

d.correctLocation <- d.block2 %>%
  filter(state_type=="CORRECT") %>%
  droplevels()

round(mean(d.correctLocation$location_rating),2) #.98

#responses for correct location items
correctLocationRatings<-ggplot(d.correctLocation, aes(x=workerid,y=location_rating)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("responses to 'correct' location items")

correctLocationRatings
#ggsave(correctLocationRatings,filename="../graphs/DataExclusion/Block2_correctLocationRatings.png")


#responses for incorrect location items
d.incorrectLocation <- d.block2 %>%
  filter(state_type=="INCORRECT") %>%
  droplevels()

incorrectLocationRatings<-ggplot(d.incorrectLocation, aes(x=workerid,y=location_rating)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("responses to 'incorrect' location items")

incorrectLocationRatings

round(mean(d.incorrectLocation$location_rating),2) #.01

#ggsave(incorrectLocationRatings,filename="../graphs/DataExclusion/Block2_incorrectLocationRatings.png")

#look at mean responses to 'correct' location items to filter outliers

correctLocation.means = d.correctLocation %>%
  group_by(workerid) %>%
  summarise(Mean = mean(location_rating))

correctLocation.meansByParticipant<-ggplot(correctLocation.means, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("avg location_rating for 'correct' location items")

correctLocation.meansByParticipant
#ggsave(correctLocation.meansByParticipant,filename="../graphs/DataExclusion/Block2_avg_CorrectLocationMeansByParticipant.png")


correctLocation.outliers <-correctLocation.means[correctLocation.means$Mean<(mean(correctLocation.means$Mean) - 2*sd(correctLocation.means$Mean)),]
length(unique(correctLocation.outliers$workerid))  #4

#look at mean responses to 'incorrect' location items to filter outliers
incorrectLocation.means = d.incorrectLocation %>%
  group_by(workerid) %>%
  summarise(Mean = mean(location_rating))

incorrectLocation.meansByParticipant<-ggplot(incorrectLocation.means, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("avg location_rating for 'incorrect' location items")
incorrectLocation.meansByParticipant
#ggsave(incorrectLocation.meansByParticipant,filename="../graphs/DataExclusion/Block2_avg_IncorrectLocationMeansByParticipant.png")


incorrectLocation.outliers <-incorrectLocation.means[incorrectLocation.means$Mean>(mean(incorrectLocation.means$Mean) + 2*sd(incorrectLocation.means$Mean)),]
length(unique(incorrectLocation.outliers$workerid))  #10

block2outliers <- c(correctLocation.outliers$workerid,incorrectLocation.outliers$workerid)
length(unique(block2outliers))  #11


##################################
#####Block 3-MC responses#######
##################################
d.MC <- d %>%
  filter(item_type == "MC" & block3 == "block3") %>%
  droplevels()
nrow(d.MC) #1296 (324 * 2 MCs)

round(mean(d.MC$projection),2) #.95

IndividualMCResponsesByOrientation<- ggplot(d.MC, aes(x=workerid,y=projection)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("projection responses to MCs")+
  facet_wrap(~orientation)

IndividualMCResponsesByOrientation
ggsave(IndividualMCResponsesByOrientation,filename="../graphs/DataExclusion/Block3_IndividualMCResponsesByOrientation.png")


MCresponse.means = d.MC %>%
  group_by(workerid,orientation) %>%
  summarise(Mean = mean(projection))

MC_responses.meansByParticipant<-ggplot(MCresponse.means, aes(x=workerid,y=Mean)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("avg location_rating for 'incorrect' location items")+
  facet_wrap(~orientation)

MC_responses.meansByParticipant

#ggsave(MC_responses.meansByParticipant,filename="../graphs/DataExclusion/Block3_avg_MCMeansByParticipant.png")


projection_socialInfo_MCs<-ggplot(subset(d,(item_type=="MC" & block3=="block3")), aes(x=orientation,y=projection,fill=socialInfo))+
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Non-southern speaker","Southern speaker"),values=c("#56B4E9", "tomato1"))+
  xlab("CC orientation") +
  ylab("Mean certainty rating") + 
  scale_x_discrete(labels=c("conservative", "liberal","neutral")) +
  ylim(0,1)+
  theme(legend.position = "right",aspect.ratio=1,axis.text=element_text(size=18),axis.title=element_text(size=18,face="bold"),legend.title=element_blank(),legend.text=element_text(size=18),legend.margin = margin(t=-.1,l=.1,r=.1,b=.1,unit='cm'))+
ggtitle("Main clauses (block 3)-no data removed")

projection_socialInfo_MCs
#ggsave(projection_socialInfo_MCs,filename="../graphs/DataExclusion/Block3_projection~socialInfo_MCs(no data removed).png")

##political MCs
projection_socialInfo_MCs_political<-ggplot(subset(d,(item_type=="MC" & block3=="block3" & orientation!="N")), aes(x=orientation,y=projection,fill=socialInfo))+
  stat_summary(geom="bar",position=position_dodge(),fun=mean,width=.5)+
  stat_summary(geom="errorbar",fun.data=mean_cl_boot,position=position_dodge(),width=.5)+
  scale_fill_manual(labels=c("Non-southern speaker","Southern speaker"),values=c("#56B4E9", "tomato1"))+
  xlab("Main clause\ncontent orientation") +
  ylab("Mean certainty rating") + 
  scale_x_discrete(labels=c("conservative", "liberal")) +
  ylim(0,1)+
  theme(legend.position = "right",aspect.ratio=1,axis.text=element_text(size=8),axis.title=element_text(size=8,face="bold"),legend.title=element_blank(),legend.text=element_text(size=8),legend.margin = margin(t=-.1,l=.1,r=.1,b=.1,unit='cm'))

projection_socialInfo_MCs_political
ggsave(projection_socialInfo_MCs_political,filename="../graphs/Non-critical_items/Block3_projection~socialInfo_MCs_Political.png")

#outliers based on responses to neutral MCs
 MC_neutral <- subset(d.MC,(orientation=="N"))
 mean(MC_neutral$projection) #.96
 MC_neutral.outliers <- MC_neutral[ MC_neutral$projection < (mean( MC_neutral$projection) - 2*sd( MC_neutral$projection)),]
 
 MC_neutral.outliers <- unique(MC_neutral.outliers$workerid)
length(unique(MC_neutral.outliers)) #17 workers



###########################
### Final Exclusion #######
##########################

outliersAcrossTasks = c(outliers_objective,block2outliers,MC_neutral.outliers)
length(unique(outliersAcrossTasks))  # 41 unique workers to exclude

# exclude all outliers identified above
d <- d %>%
    filter(!(workerid %in% outliersAcrossTasks)) %>%
  droplevels()
length(unique(d$workerid))  #300

####Demographics####
#################
#############

# age and gender of remaining participants
table(d$age) 
range(d$age,na.rm=TRUE) #18-76
length(which(is.na(d$age))) # 0 missing values
median(d$age,na.rm=TRUE) #35


d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarise(count=n())
#204 female, 110 male, 10 NB


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

unique(d$otherParty)
partyPercent <- d %>% 
  select(party, workerid) %>% 
  unique() %>% 
  group_by(party) %>% 
  summarise(count=n()) %>%
  mutate(freq = round(count / sum(count), 3))
partyPercent


party
slices <- party$count
lbls <- c("Democrat","Green","Libertarian","Other","Republican","no response")
colors = c("blue", "green", "gold", "grey", "red", "white") 

pie(slices, labels = lbls, col=colors, main="Participant party affiliations")

partyPercent <- d %>% 
  select(party, workerid) %>% 
  unique() %>% 
  group_by(party) %>% 
  summarise(count=n()) %>%
  mutate(freq = round(count / sum(count), 3))


partyPercent

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

highPopStates <- subset(d,state %in% c("FL","OH","NC","NY","TX"))
highPopStates$state <- as.factor(highPopStates$state)


statePartyTable <- xtabs(~state + party,data=highPopStates)
statePartyTableUnique <- statePartyTable/26

statePartyTableUnique
statePartyTableTotals <- cbind(statePartyTableUnique, total = rowSums(statePartyTableUnique))
statePartyTableTotals
proportions(statePartyTableUnique,"state")

politicsRatingsHighPop <- droplevels(subset(highPopStates, politics!="NA"))

politicsRatingsHighPopMedian <- politicsRatingsHighPop  %>%
  group_by(state)%>% 
  summarise(Median=median(politics))
politicsRatingsHighPopMedian



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
#[1] "list2_NS"
#[1] 89
#[1] "list1_NS"
#[1] 72
#[1] "list1_S"
#[1] 71
#[1] "list2_S"
#[1] 68


write.csv(d, file="../data/data_preprocessed.csv",row.names=F,quote=T)

