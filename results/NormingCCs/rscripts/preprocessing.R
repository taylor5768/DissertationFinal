#identifying CCs that display strong associations w/Democrat/Republican parties
#preprocessing - based on Judith Tonhauser's code 

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

source('../../helpers.R')

# load required packages for pre-processing data
library(tidyverse)

# read in the raw data
d = read_csv("../data/experiment-trials.csv")

# read in the subject information
ds = read_csv("../data/experiment-subject_information.csv")

# merge subject information into data
d = d %>%
  left_join(ds, by=c("workerid"))

# how many participants?
length(unique(d$workerid)) #42

# look at Turkers' comments
unique(ds$comments)


# participant info
table(d$age) 
range(d$age)#19-57
length(which(is.na(d$age))) # 0 missing values
median(d$age,na.rm=TRUE) #27

d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#27 female, 14 male, 1 other, 0 undeclared


# make a trial number
unique(d$slide_number_in_experiment) #slide numbers from 5 to 26
d$trial = d$slide_number_in_experiment - 3
unique(d$trial) 
range(unique(d$trial))# trial numbers from 1 to 22


### exclude non-English speakers and non-American English speakers
# exclude non-English speakers
length(which(is.na(ds$language))) #no missing responses
table(ds$language) #everyone has English as first language

#this wont do anything since everyone has English as first lang, but just for the future:
d <- d %>%
  filter(language != "Chinese" & language != "Russian" & language != "telugu") %>%  droplevels()
length(unique(d$workerid)) # (data from 0 Turker excluded, 42 remaining Turkers)



# exclude non-American English speakers
length(unique(d$workerid))# X
length(which(is.na(d$american))) #0 (everybody responded)
table(d$american) #everyone said yes

# exclude Turkers based on attention checks

# attention checks
names(d)
d.MC <- d %>%
  filter(trigger_class == "MC") %>%
  droplevels()
nrow(d.MC) #120 (42 Turkers x 2 MCs)

#attention check 1: capital of US
# this is the false attention check, so responses are expected to be low
table(d$topic)
d.MC.attention_capital <- d.MC %>%
  filter(topic == "capital") %>%
  droplevels()
nrow(d.MC.attention_capital) #42 (42 instances of attention check 1; 1 for each worker)

# group mean for attention check 1
round(mean(d.MC.attention_capital$response),2) #.09


ggplot(d.MC.attention_capital, aes(x=workerid,y=response)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("mean response to false attention check: capital")

#attention check 2: pres. elections
# this is the true attention check, so  responses are expected to be high
table(d$topic)
d.MC.attention_elections <- d.MC %>%
  filter(topic == "elections") %>%
  droplevels()
nrow(d.MC.attention_elections) #42 (42 instances of attentin check 2 - 1 for each worker)

# group mean for attention check 2
round(mean(d.MC.attention_elections$response),2) #.89

ggplot(d.MC.attention_elections, aes(x=workerid,y=response)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("mean response to true attention check:  elections")


# get the Turkers who are more than 2 standard deviations above the mean on attention check 1
f <-d.MC.attention_capital[d.MC.attention_capital$response > (mean(d.MC.attention_capital$response) + 2*sd(d.MC.attention_capital$response)),]
f



# get the Turkers who are more than 2 standard deviations below the mean on attention check 2
t <- d.MC.attention_elections[d.MC.attention_elections$response < (mean(d.MC.attention_elections$response) - 2*sd(d.MC.attention_elections$response)),]
t

# look at the main clauses that these "outlier" Turkers did
# make data subset of just the outliers
outliers <- d.MC %>%
  filter(workerid %in% f$workerid | workerid %in% t$workerid)
outliers = droplevels(outliers)
nrow(outliers) #10
unique(outliers$workerid) #5 turkers


# exclude all outliers identified above
d <- d %>%
  filter(!(workerid %in% f$workerid | workerid %in% t$workerid)) %>%
  droplevels()
length(unique(d$workerid)) # 37 remaining Turkers (5 Turkers excluded)


# exclude turkers who always clicked on roughly the same point on the scale 
# ie turkers whose variance in overall response distribution is more 
# than 2 sd below mean by-participant variance
variances = d %>%
  filter(trigger_class != "MC") %>%
  group_by(workerid) %>%
  summarize(Variance = var(response)) %>%
  mutate(TooSmall = Variance < mean(Variance) - 2*sd(Variance))


lowvarworkers = as.character(variances[variances$TooSmall,]$workerid)
summary(variances)
lowvarworkers # 0 turkers consistently clicked on roughly the same point on the scale

lvw = d %>%
  filter(as.character(workerid) %in% lowvarworkers) %>%
  droplevels() %>%
  mutate(Participant = as.factor(as.character(workerid)))

ggplot(lvw,aes(x=Participant,y=response)) +
  geom_point()

# exclude the Turkers identified above
d <- droplevels(subset(d, !(d$workerid %in% lowvarworkers)))
length(unique(d$workerid)) #37 Turkers remain

# age and gender of remaining participants
table(d$age) #19-57
range(d$age)
length(which(is.na(d$age))) # 0 missing values
median(d$age,na.rm=TRUE) #29

d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#23 female, 13 male, 1 other, 0 undeclared
# write cleaned dataset to file

#check how many per list + social condition combo
exps = unique(d$list_firstSocCond)
for(exp in exps){
  subdata = subset(d,list_firstSocCond==exp)
  participants=unique(subdata$workerid)
  count = length(participants)
  #print(paste0("List",exp,"\n"))
  print(exp)
  print(count)
}

write.csv(d, file="../data/data_preprocessed.csv",row.names=F,quote=T)

