###compare results across experiments

this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)

setwd(this.dir)

source('../helpers.R')
library(tidyverse)
library(lme4)
library(lmerTest)
library(standardize)
library(car)
source('../model_to_RTF.R')



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

allData$item <- as.factor(paste(allData$predicate,allData$topic,allData$orientation))


#add a new variable where social info is named the same across exps
allData$singleSocialInfo <- ""
allData$singleSocialInfo[allData$socialInfo == "N" |allData$socialInfo == "D" ] <- "D"
allData$singleSocialInfo[allData$socialInfo == "S" | allData$socialInfo == "R"] <- "R"

allData$topic[allData$topic=="covid"] <- "COVID"
allData_sub <- droplevels(subset(allData,(item_type=="critical")))
allData_sub <- droplevels(subset(allData_sub,(orientation!="N")))
allData_sub$topic <- as.factor(allData_sub$topic)
levels(allData_sub$topic)



metaModel <- lmer(projection~ (singleSocialInfo + orientation + experiment)^3   + predicate + (1|item) + (1|workerid),
                      data=subset(allData,item_type=="critical" & orientation !="N"),REML=F)

metaModel.1 <- lmer(projection~ (singleSocialInfo + orientation + experiment)^2   + predicate + (1|item) + (1|workerid),
                    data=subset(allData,item_type=="critical" & orientation !="N"),REML=F)

anova(metaModel,metaModel.1) #3-way interaction is significant

#add more complex raneffs
metaModel.final <- lmer(projection~ singleSocialInfo * orientation * experiment   + predicate + (1|item) + (1|workerid),
     data=subset(allData,item_type=="critical" & orientation !="N"),contrasts=list(predicate=named_contr_sum(allData$predicate)),REML=F)
summary(metaModel.final)

#(Intercept)                                            0.50826    0.01380 2064.36173  36.827  < 2e-16 ***
#  singleSocialInfoR                                      0.11835    0.01409 8662.59616   8.400  < 2e-16 ***
#  orientationliberal                                     0.15201    0.01450 4376.10242  10.485  < 2e-16 ***
#  experimentExp2                                         0.04364    0.01774 2126.77817   2.460    0.014 *  
#  experimentExp3                                         0.13384    0.01805 2134.63582   7.413 1.76e-13 ***
#  predicateacknowledge                                   0.13916    0.01051  297.63773  13.235  < 2e-16 ***
#  predicateadmit                                         0.12056    0.01059  315.41945  11.387  < 2e-16 ***
#  predicatebe_annoyed                                    0.11934    0.01071  321.28888  11.140  < 2e-16 ***
#  predicatebe_right                                     -0.41475    0.01043  295.87274 -39.751  < 2e-16 ***
#  predicateconfess                                       0.05770    0.01026  276.58774   5.625 4.53e-08 ***
#  predicatehear                                          0.11436    0.01060  303.18539  10.784  < 2e-16 ***
#  predicateknow                                          0.22821    0.01043  289.70701  21.885  < 2e-16 ***
 # predicatepretend                                      -0.18757    0.01041  287.96697 -18.021  < 2e-16 ***
#  predicatesay                                          -0.12642    0.01057  306.63765 -11.957  < 2e-16 ***
#  predicatesee                                           0.17311    0.01047  300.59729  16.526  < 2e-16 ***
#  predicatesuggest                                      -0.16482    0.01046  296.91755 -15.753  < 2e-16 ***
#  singleSocialInfoR:orientationliberal                  -0.23551    0.01992 8661.42805 -11.826  < 2e-16 ***
 # singleSocialInfoR:experimentExp2                      -0.07576    0.01841 8668.20640  -4.116 3.89e-05 ***
#  singleSocialInfoR:experimentExp3                      -0.09176    0.01872 8681.15268  -4.902 9.64e-07 ***
#  orientationliberal:experimentExp2                     -0.09387    0.01839 8651.09552  -5.105 3.38e-07 ***
#  orientationliberal:experimentExp3                     -0.11283    0.01874 8687.02850  -6.022 1.80e-09 ***
#  singleSocialInfoR:orientationliberal:experimentExp2    0.17906    0.02602 8665.90706   6.881 6.35e-12 ***
#  singleSocialInfoR:orientationliberal:experimentExp3    0.17512    0.02646 8678.99831   6.618 3.87e-11 ***


metaModel.final <- lmer(projection~ singleSocialInfo * orientation * experiment   + predicate + (1|item) + (1|workerid),
                        data=subset(allData,item_type=="critical" & orientation !="N"),contrasts=list(predicate=named_contr_sum(allData$predicate)),REML=F)

saveModelAsRTF(metaModel.final, 'metaModel.rtf',7.5)

#see if there's a difference between Experiments 2 and 3
allData$experiment <- as.factor(allData$experiment)
allData$experiment <- relevel(allData$experiment,ref="Exp2")

metaModel.final.relevel <- lmer(projection~ singleSocialInfo * orientation * experiment   + predicate + (1|item) + (1|workerid),
                        data=subset(allData,item_type=="critical" & orientation !="N"),contrasts=list(predicate=named_contr_sum(allData$predicate)),REML=F)

summary(metaModel.final.relevel) #no difference b/t Exps 2 and 3

allData$experiment <- relevel(allData$experiment,ref="Exp1")


####perceived speaker beliefs as a function of social info and CC orientation
speaker_beliefs_interaction.full<- lmer(spBelief_rating ~ (singleSocialInfo + orientation + experiment)^3 + (1|item) ,data=subset(allData,item_type=="critical" & orientation !="N"),REML=F)

speaker_beliefs_interaction.1<- lmer(spBelief_rating ~ (singleSocialInfo + orientation + experiment)^2 + (1|item) ,data=subset(allData,item_type=="critical" & orientation !="N"),REML=F)

anova(speaker_beliefs_interaction.full,speaker_beliefs_interaction.1)
#interaction is significant

speaker_beliefs_interaction.final<- lmer(spBelief_rating ~ singleSocialInfo + orientation + experiment + singleSocialInfo:orientation+singleSocialInfo:experiment + orientation:experiment + singleSocialInfo:orientation:experiment + (1|item) ,data=subset(allData,item_type=="critical" & orientation !="N"),REML=F)

summary(speaker_beliefs_interaction.final)
#(Intercept)                                            0.16948    0.01009 3061.27802   16.80   <2e-16 ***
#singleSocialInfoR                                      0.54101    0.01303 9377.34850   41.51   <2e-16 ***
#  orientationliberal                                     0.63852    0.01426 3061.97053   44.77   <2e-16 ***
#  experimentExp2                                         0.12526    0.01203 9367.71068   10.41   <2e-16 ***
#  experimentExp3                                         0.20082    0.01230 9465.89993   16.33   <2e-16 ***
#  singleSocialInfoR:orientationliberal                  -1.14132    0.01843 9377.48690  -61.92   <2e-16 ***
#  singleSocialInfoR:experimentExp2                      -0.23181    0.01703 9384.48183  -13.61   <2e-16 ***
#  singleSocialInfoR:experimentExp3                      -0.31232    0.01733 9403.20858  -18.02   <2e-16 ***
#  orientationliberal:experimentExp2                     -0.26202    0.01701 9367.24752  -15.40   <2e-16 ***
#  orientationliberal:experimentExp3                     -0.41913    0.01740 9466.37456  -24.09   <2e-16 ***
#  singleSocialInfoR:orientationliberal:experimentExp2    0.48871    0.02409 9384.39137   20.29   <2e-16 ***
#  singleSocialInfoR:orientationliberal:experimentExp3    0.67371    0.02451 9404.35678   27.48   <2e-16 ***

saveModelAsRTF(speaker_beliefs_interaction.final, 'models/speakerBeliefsFromSocialInfo.rtf',5.5)


speaker_beliefs.full <- lmer(projection ~ (spBelief_rating + experiment)^2 + predicate + (1|item) + (1|workerid),data=subset(allData,item_type=="critical" & orientation !="N"),REML=F)

speaker_beliefs.1 <- lmer(projection ~ spBelief_rating + experiment+ predicate + (1|item) + (1|workerid),data=subset(allData,item_type=="critical" & orientation !="N"),REML=F)

anova(speaker_beliefs.full,speaker_beliefs.1) 
#3-way interaction is significant

speaker_beliefs.final <- lmer(projection ~ spBelief_rating + experiment + spBelief_rating:experiment + predicate+ (1|item) + (1|workerid),data=subset(allData,item_type=="critical" & orientation !="N"),contrasts=list(predicate=named_contr_sum(allData$predicate)),REML=F)

summary(speaker_beliefs.final)
#(Intercept)                       0.49402    0.01251 1518.23352  39.500  < 2e-16 ***
#  spBelief_rating                   0.19099    0.01387 8826.51761  13.773  < 2e-16 ***
#  experimentExp2                    0.06645    0.01662 1656.31768   3.999 6.65e-05 ***
#  experimentExp3                    0.13006    0.01749 1876.20944   7.437 1.56e-13 ***
#  predicateacknowledge              0.13979    0.01061  299.98636  13.172  < 2e-16 ***
#  predicateadmit                    0.12055    0.01068  317.34673  11.283  < 2e-16 ***
#  predicatebe_annoyed               0.11918    0.01081  322.87247  11.030  < 2e-16 ***
#  predicatebe_right                -0.41493    0.01053  297.94593 -39.398  < 2e-16 ***
#  predicateconfess                  0.05770    0.01036  278.97268   5.571 5.96e-08 ***
#  predicatehear                     0.11382    0.01070  305.14912  10.637  < 2e-16 ***
#  predicateknow                     0.22848    0.01052  291.63119  21.711  < 2e-16 ***
#  predicatepretend                 -0.18748    0.01050  289.86978 -17.848  < 2e-16 ***
#  predicatesay                     -0.12767    0.01067  308.38883 -11.968  < 2e-16 ***
#  predicatesee                      0.17341    0.01057  302.53330  16.405  < 2e-16 ***
#  predicatesuggest                 -0.16480    0.01056  299.21511 -15.605  < 2e-16 ***
#  spBelief_rating:experimentExp2   -0.13263    0.01983 8825.78146  -6.689 2.38e-11 ***
#  spBelief_rating:experimentExp3   -0.11592    0.02226 8905.68264  -5.208 1.95e-07 ***

saveModelAsRTF(speaker_beliefs.final, 'models/ProjectionFromSpeakerBeliefs.rtf',5.5)

allData$experiment <- as.factor(allData$experiment)
allData$experiment <- relevel(allData$experiment,ref="Exp2")

speaker_beliefs.final.relevel <- lmer(projection ~ spBelief_rating + experiment + spBelief_rating:experiment + predicate+ (1|item) + (1|workerid),data=subset(allData,item_type=="critical" & orientation !="N"),contrasts=list(predicate=named_contr_sum(allData$predicate)),REML=F)

summary(speaker_beliefs.final.relevel)

#### DID NOT END UP USING THESE:
speaker_beliefs.full <- lmer(projection ~ spBelief_rating + participant_beliefs + experiment + spBelief_rating:experiment + participant_beliefs:experiment + predicate + (1|item) + (1|workerid),data=subset(allData,item_type=="critical" & orientation !="N"),REML=F)

speaker_beliefs.1 <- lmer(projection ~ spBelief_rating + participant_beliefs + experiment  + participant_beliefs:experiment + predicate + (1|item) + (1|workerid),data=subset(allData,item_type=="critical" & orientation !="N"),REML=F)

anova(speaker_beliefs.full,speaker_beliefs.1)
#spBelief_rating:experiment is significant

speaker_beliefs.2 <- lmer(projection ~ spBelief_rating + participant_beliefs + experiment  + spBelief_rating:experiment + predicate + (1|item) + (1|workerid),data=subset(allData,item_type=="critical" & orientation !="N"),REML=F)

anova(speaker_beliefs.full, speaker_beliefs.2)
#not significant

speaker_beliefs.2a <- lmer(projection ~ spBelief_rating  + experiment  + spBelief_rating:experiment + predicate + (1|item) + (1|workerid),data=subset(allData,item_type=="critical" & orientation !="N"),REML=F)

anova(speaker_beliefs.2,speaker_beliefs.2a)
#participant_beliefs is significant


speaker_beliefs.2b <- lmer(projection ~ spBelief_rating  + experiment  + participant_beliefs + spBelief_rating:experiment + (1|item) + (1|workerid),data=subset(allData,item_type=="critical" & orientation !="N"),REML=F)

anova(speaker_beliefs.2,speaker_beliefs.2b)
#predicate is significant

speaker_beliefs.final <- lmer(projection ~ spBelief_rating  + experiment  + participant_beliefs + spBelief_rating:experiment + predicate + (1+spBelief_rating*experiment|item) + (1|workerid),data=subset(allData,item_type=="critical" & orientation !="N"),contrasts=list(predicate=named_contr_sum(allData$predicate)),REML=F)

vif(speaker_beliefs.final)

