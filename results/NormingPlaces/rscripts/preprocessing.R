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
length(unique(d$workerid)) #44

# look at Turkers' comments
unique(ds$comments)

# participant info
table(d$age) 
range(d$age) #18-64
length(which(is.na(d$age))) # 0 missing values
median(d$age,na.rm=TRUE) #26

d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#28 female, 14 male, 2 other, 0 undeclared


# make a trial number
unique(d$slide_number_in_experiment) #slide numbers from 4 to 27
d$trial = d$slide_number_in_experiment - 3
unique(d$trial) 
range(unique(d$trial))# trial numbers from 1 to 24


# exclude non-American English speakers
length(unique(d$workerid))#41
d <- d %>%
  filter(american != "-1" & american !="NA") %>%  droplevels()
table(d$american) #everyone said yes
length(unique(d$workerid))  #no one was excluded


# exclude Turkers based on attention checks

# attention checks (Party trials)
names(d)
d.Party <- d %>%
  filter(trigger_class == "Party") %>%
  droplevels()
nrow(d.Party) #176 (44 Turkers x 4 )


#d.Empty <- d %>%
#     filter(trigger_class == "Empty") %>%
 #      droplevels()


#responses to "Same" trials (e.g., speaker is Democrat, asked about whether speaker is Democrat)
table(d$socialCondition)
d.Party.Same <- d.Party%>%
  filter((socialInfo == "Democrat" & socialCondition == "D") | socialInfo == "Republican" & socialCondition == "R") %>%
  droplevels()
nrow(d.Party.Same) #88  (44 Turkers x 2) 


# group mean for same trials
round(mean(d.Party.Same$response),2) #.97

ggplot(d.Party.Same, aes(x=workerid,y=response)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("mean response 'same' trials")

#responses to "Different" trials (e.g., speaker is Democrat, asked about whether speaker is Republican)
d.Party.Different <- d.Party%>%
  filter((socialInfo == "Democrat" & socialCondition == "R") | socialInfo == "Republican" & socialCondition == "D") %>%
  droplevels()
nrow(d.Party.Different) #88 (44 Turkers x2)


# group mean for same trials
round(mean(d.Party.Different$response),2) #.09

ggplot(d.Party.Different, aes(x=workerid,y=response)) +
  geom_point() +
  geom_text(aes(label=workerid), vjust = 1, cex= 5)+
  ylab("mean response to 'different' trials")



# get the Turkers who are more than 2 standard deviations below the mean on "Same" trials
same <- d.Party.Same[d.Party.Same$response < (mean(d.Party.Same$response) - 2*sd(d.Party.Same$response)),]
same

# get the Turkers who are more than 2 standard deviations above the mean on "Different" trials
different <-d.Party.Different[d.Party.Different$response > (mean(d.Party.Different$response) + 2*sd(d.Party.Different$response)),]
different


# look at the trials that these "outlier" Turkers did
# make data subset of just the outliers
outliers <- d.Party %>%
  filter(workerid %in% same$workerid | workerid %in% different$workerid)
outliers = droplevels(outliers)
nrow(outliers) #28
unique(outliers$workerid) #7 turkers


# exclude all outliers identified above
d <- d %>%
  filter(!(workerid %in% same$workerid | workerid %in% different$workerid)) %>%
  droplevels()
length(unique(d$workerid)) # 37 remaining Turkers


# exclude turkers who always clicked on roughly the same point on the scale 
# ie turkers whose variance in overall response distribution is more 
# than 2 sd below mean by-participant variance
variances = d %>%
  filter(trigger_class != "Party") %>%
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
table(d$age) 
range(d$age)#18-64
length(which(is.na(d$age))) # 0 missing values
median(d$age,na.rm=TRUE) #23

d %>% 
  select(gender, workerid) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
#24 female, 11 male, 2 other, 0 undeclared

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
