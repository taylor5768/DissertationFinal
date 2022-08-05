#primary analysis for Exp 2
#based on Judith Tonhauser's code


# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
library(tidyverse)
library(lme4)
library(lmerTest)
library(standardize)
library(emmeans)


# load helper functions
source('../../helpers.R')
source('../../model_to_RTF.R')

cd = read.csv("../data/data_preprocessed.csv")
cd$orientation = as.factor(cd$orientation)
cd$socialInfo = as.factor(cd$socialInfo)
cd$predicate = as.factor(cd$predicate)
cd$predicate <-relevel(cd$predicate,ref="know")
cd$socialInfo<-as.factor(cd$socialInfo)

cd$speakerOverallRepPerception <- ((1-cd$democrat_rating)+(cd$republican_rating))/2
cd$similarity <- abs(cd$politics - cd$speakerOverallRepPerception)


cd$item <- as.factor(paste(cd$predicate,cd$topic,cd$orientation))

cd_critical = droplevels(subset(cd,item_type=="critical"))



cd$MC.item <- as.factor(paste(cd$topic,cd$orientation))

# what's the min and max number of unique predicate/content combinations?
table(cd_critical$predicate,cd_critical$CC,cd_critical$socialInfo)
min(table(cd_critical$predicate,cd_critical$CC,cd_critical$socialInfo)) # 1
max(table(cd_critical$predicate,cd_critical$CC,cd_critical$socialInfo)) # 14
mean(table(cd_critical$predicate,cd_critical$CC,cd_critical$socialInfo)) # 6.25



########################H.2.1a.#################################
#is neutral CC projection NOT affected by speaker politics?####
###############################################################

#5.2.22
neutral.full <- lmer(projection~ socialInfo * predicate + (1|item) +(1|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F)


neutral.1<- lmer(projection~ socialInfo + predicate + (1|item) +(1|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F) 


anova(neutral.full,neutral.1)
#interaction is n.s.
#27 582.68 731.06 -264.34   528.68 2.1214 11      0.998

neutral.2<- lmer(projection~ predicate + (1|item) +(1|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F) 

anova(neutral.1,neutral.2)
#socialInfo is n.s
#16 562.80 650.73 -265.40   530.80 0.862  1     0.3532

neutral.3 <- lmer(projection~ socialInfo + (1|topic) +(1|workerid),data=subset(cd,orientation=="N" &item_type=="critical"),REML=F) 

anova(neutral.1,neutral.3)
#562.8  650.73 -265.40    530.8 676.25 11  < 2.2e-16 ***
#predicate is significant - neutral.1 is final model

#this is the most raneffecs that would converge
neutral.final<- lmer(projection~ predicate + (1|item) +(1|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F) 
#singular fit


neutral.final.sum <- lmer(projection~ predicate + (1|item) +(1|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),contrasts=list(predicate=named_contr_sum(cd$predicate)),REML=F)

summary(neutral.final.sum)
#predicateacknowledge   0.20417    0.02543 131.61278   8.029 4.82e-13 ***
#  predicateadmit         0.08452    0.02550 132.35823   3.314  0.00119 ** 
#  predicatebe_annoyed    0.14341    0.02538 133.24229   5.651 9.25e-08 ***
#  predicatebe_right      0.20518    0.02549 135.60235   8.050 3.72e-13 ***
#  predicateconfess      -0.32438    0.02541 137.09490 -12.764  < 2e-16 ***
#  predicatehear          0.04524    0.02535 131.24732   1.785  0.07655 .  
#predicateknow          0.16518    0.02526 128.01494   6.538 1.35e-09 ***
#  predicatepretend      -0.13376    0.02464 121.16859  -5.428 2.97e-07 ***
#  predicatesay          -0.23384    0.02533 128.85931  -9.231 6.90e-16 ***
#  predicatesee           0.15848    0.02474 121.74799   6.406 2.95e-09 ***
#  predicatesuggest      -0.19231    0.02504 123.69345  -7.679 4.20e-12 ***

saveModelAsRTF(neutral.final.sum, '../models/Exp2Projection_neutral.rtf',3)
################################################################################
###H.2.1b: Are projection ratings higher for conservative CCs w/Republican speakers vs. Democrat speakers and vice versa for liberal CCs#####
###############################################################################

####check MCs
political.MCs.full <- lmer(projection~ socialInfo * orientation + (1|MC.item) + (1|workerid),
                           data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)

political.MCs.1 <- lmer(projection~ socialInfo + orientation + (1|MC.item) + (1|workerid),
                        data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)

anova(political.MCs.full,political.MCs.1)
#interaction is n.s.

political.MCs.1a <- lmer(projection~ orientation + (1|MC.item) + (1|workerid),
                        data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)


anova(political.MCs.1,political.MCs.1a)
#social info is n.s.

political.MCs.1b <- lmer(projection~ socialInfo + (1|MC.item) + (1|workerid),
                         data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)

anova(political.MCs.1,political.MCs.1b)
#orientation is n.s.

political.MCs.final <- lmer(projection~ (1|MC.item) + (1|workerid),
                         data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)
summary(political.MCs.final)

saveModelAsRTF(political.MCs.final, '../models/Exp2Projection_MCs.rtf',6.5)

#check critical items
political.full<- lmer(projection~ (socialInfo + orientation + predicate)^3 + (1|item) + (1|workerid),
                      data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)
#doesn't converge

political.full<- lmer(projection~ (socialInfo + orientation + predicate)^3 + (1|CC) + (1|workerid),
                      data=subset(cd,item_type=="critical" & orientation !="N"),REML=F) 


political.1<-lmer(projection~ (socialInfo +  orientation + predicate)^2+ (1|CC) + (1|workerid),
                 data=subset(cd,item_type=="critical" & orientation !="N"),REML=F) 

anova(political.full,political.1)
#51 655.98 971.6 -276.99   553.98 4.3783 11     0.9575
#3-way interaction is n.s.

political.1a<-lmer(projection~ socialInfo +  orientation + predicate+ 
                    socialInfo:orientation+
                    orientation:predicate+
                    (1|CC) + (1|workerid),
                  data=subset(cd,item_type=="critical" & orientation !="N"),REML=F) 
anova(political.1,political.1a)
#  40 638.35 885.90 -279.18   558.35 17.19 11     0.1024
#socialInfo:predicate interaction is not significant


political.1b<-lmer(projection~ socialInfo +  orientation + predicate+ 
                     socialInfo:orientation+
                     socialInfo:predicate +
                     (1|CC) + (1|workerid),
                   data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)
anova(political.1,political.1b)
#predicate:orientation is n.s.
#40 638.35 885.90 -279.18   558.35 3.0916 11     0.9895


political.1c<-lmer(projection~ socialInfo +  orientation + predicate+ 
                     socialInfo:predicate +
                     predicate:orientation +
                     (1|CC) + (1|workerid),
                   data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)
anova(political.1,political.1c) 
#socialInfo:orientation is significant
#40 638.35 885.90 -279.18   558.35 11.64  1  0.0006453 ***


political.final <- lmer(projection~ socialInfo +  orientation + predicate+ 
                      socialInfo:orientation+
                      (1|CC) + (1|workerid),
                    data=subset(cd,item_type=="critical" & orientation !="N"),REML=F) 

#try more complex raneffs - below is most complex that would converge
political.final <- lmer(projection~ socialInfo +  orientation + predicate+ 
                          socialInfo:orientation+
                          (1|item) + (1|workerid),
                        data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

political.final.sum <- lmer(projection~ socialInfo +  orientation + predicate+ 
                              socialInfo:orientation+
                              (1|item) + (1|workerid),
                            data=subset(cd,item_type=="critical" & orientation !="N"),contrasts=list(predicate=named_contr_sum(cd$predicate)),REML=F)

summary(political.final.sum)
#(Intercept)                       0.55297    0.01281  175.15748  43.179  < 2e-16 ***
#socialInfoS                       0.04058    0.01153 3299.81581   3.520 0.000438 ***
#  orientationliberal                0.05682    0.01162  772.87772   4.891 1.22e-06 ***
#  predicateacknowledge              0.20918    0.01437  274.07994  14.561  < 2e-16 ***
#  predicateadmit                    0.11151    0.01441  273.05685   7.737 1.98e-13 ***
#  predicatebe_annoyed               0.11227    0.01436  280.23338   7.817 1.10e-13 ***
 # predicatebe_right                 0.14772    0.01446  286.35000  10.218  < 2e-16 ***
 # predicateconfess                 -0.39480    0.01445  300.10990 -27.328  < 2e-16 ***
 # predicatehear                     0.06092    0.01431  273.49284   4.256 2.86e-05 ***
#  predicateknow                     0.10071    0.01424  265.89898   7.073 1.34e-11 ***
#  predicatepretend                 -0.17855    0.01380  252.30469 -12.936  < 2e-16 ***
 # predicatesay                     -0.13392    0.01427  265.32272  -9.383  < 2e-16 ***
 # predicatesee                      0.19056    0.01384  244.85551  13.769  < 2e-16 ***
 # predicatesuggest                 -0.14093    0.01402  240.46302 -10.055  < 2e-16 ***
 # socialInfoS:orientationliberal   -0.05505    0.01629 3299.62204  -3.380 0.000733 ***

saveModelAsRTF(political.final.sum, '../models/Exp2Projection_Political.rtf',4.5)


###############################################################################
#####Beliefs#####
#############################################################################

#relationship b/t speaker and listener beliefs
cor(cd_critical$participant_beliefs,cd_critical$spBelief_rating,use="complete.obs")
#r=.1025042



#similarity by speaker
bothBeliefs.full <- lmer(projection ~ spBelief_rating + participant_beliefs*similarity + predicate + (1|workerid) + (1|item), data=subset(cd_critical,orientation!="N"&similarity!="NA"), REML="F")
vif(bothBeliefs.full)

bothBeliefs.1 <- lmer(projection ~ spBelief_rating + participant_beliefs+similarity + predicate + (1|workerid) + (1|item), data=subset(cd_critical,orientation!="N"& similarity!="NA"), REML="F")



anova(bothBeliefs.full,bothBeliefs.1)
#interaction is n.s.

bothBeliefs.1a <- lmer(projection ~ spBelief_rating + participant_beliefs+similarity + (1|workerid) + (1|item), data=subset(cd_critical,orientation!="N" & similarity!="NA"), REML="F")

anova(bothBeliefs.full,bothBeliefs.1a)
#predicate is significant

bothBeliefs.1b <- lmer(projection ~ participant_beliefs+similarity + predicate + (1|workerid) + (1|item), data=subset(cd_critical,orientation!="N" & similarity!="NA"), REML="F")

anova(bothBeliefs.1,bothBeliefs.1b)
#speaker beliefs is significant

bothBeliefs.1c <- lmer(projection ~ spBelief_rating + similarity + predicate + (1|workerid) + (1|item), data=subset(cd_critical,orientation!="N" & similarity!="NA"), REML="F")

anova(bothBeliefs.1,bothBeliefs.1c)
#participant beliefs are significant

bothBeliefs.1d <- lmer(projection ~ spBelief_rating + participant_beliefs + predicate + (1|workerid) + (1|item), data=subset(cd_critical,orientation!="N" & similarity!="NA"), REML="F")

anova(bothBeliefs.1,bothBeliefs.1d)
#similarity is n.s. 

bothBeliefs.final <- lmer(projection ~ spBelief_rating + participant_beliefs +  predicate + (1+participant_beliefs|workerid) + (1|topic/item), data=subset(cd_critical,orientation!="N" & similarity!="NA"), contrasts=list(predicate=named_contr_sum(cd$predicate)),REML="F")


summary(bothBeliefs.final)
saveModelAsRTF(bothBeliefs.final, '../models/Projection_from_bothBeliefs.rtf',4)


####super complex model
bothBeliefs.complex.full <- lmer(projection ~ spBelief_rating+predicate+orientation+participant_beliefs + similarity +
(participant_beliefs+similarity+predicate)^3+
(spBelief_rating+predicate+orientation)^3+
  (1|workerid) + (1|item), data=subset(cd_critical,orientation!="N"&similarity!="NA"), REML="F")

step(bothBeliefs.complex.full,test="Chisq",direction="backward")
  

vif(bothBeliefs.full)


################################################################################
#H.2.3.a for political items are speaker belief ratings predicted by socialInfo x orientation interaction?
###############################################################################

#would not converge with participant raneff.
speaker_beliefs_interaction.full<- lmer(spBelief_rating ~ socialInfo * orientation + (1|item),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

speaker_beliefs_interaction.1<- lmer(spBelief_rating ~ socialInfo + orientation + (1|item) ,data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)


anova(speaker_beliefs_interaction.full,speaker_beliefs_interaction.1)
#interaction is significant
# 6  221.08  258.21 -104.54   209.08 1299.8  1  < 2.2e-16 ***

speaker_beliefs_interaction.final<- lmer(spBelief_rating ~ socialInfo * orientation + (1|topic/item),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

#(Intercept)                       0.29444    0.01111   45.64240   26.50   <2e-16 ***
#socialInfoS                       0.31114    0.01170 3565.67265   26.60   <2e-16 ***
#  orientationliberal                0.37645    0.01318  660.54485   28.55   <2e-16 ***
#  socialInfoS:orientationliberal   -0.65584    0.01654 3566.11679  -39.64   <2e-16 ***


summary(speaker_beliefs_interaction.final)
#  socialInfoS                       0.31118    0.01156 3582.90642   26.91  < 2e-16 ***
#  orientationliberal                0.37964    0.02546   29.86284   14.91 2.25e-15 ***
#  socialInfoS:orientationliberal   -0.65743    0.01635 3582.90642  -40.20  < 2e-16 ***

saveModelAsRTF(speaker_beliefs_interaction.final, '../models/Exp2SpBeliefs_Political_bySpAffilAndOrientation.rtf',2.5)



################################################################################
###H.2.3b: are perceived speaker belief ratings a better predictor of projection than listener belief ratings?#########################################################

speaker_beliefs.full <- lmer(projection ~ (spBelief_rating + predicate + orientation)^3 + (1|item)+(1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)


speaker_beliefs.1 <- lmer(projection ~ (spBelief_rating + predicate + orientation)^2 + (1|item)+(1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

anova(speaker_beliefs.full,speaker_beliefs.1)
#3-way interaction is significant

#add more complex raneffs
speaker_beliefs.final.sum <- lmer(projection ~ spBelief_rating * predicate * orientation + (1|item)+(1+spBelief_rating|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),contrasts=list(predicate=named_contr_sum(cd$predicate)),REML=F)

summary(speaker_beliefs.final.sum)
saveModelAsRTF(speaker_beliefs.final.sum, '../models/Exp2Projection_bySpeakerBeliefs.rtf',8)


####check MCs
speaker_beliefs.MCs.full <- lmer(projection ~ spBelief_rating * orientation + (1|MC.item) + (1|workerid),data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)

speaker_beliefs.MCs.1 <- lmer(projection ~ spBelief_rating + orientation + (1|MC.item) + (1|workerid),data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)

anova(speaker_beliefs.MCs.full,speaker_beliefs.MCs.1 )
#interaction is n.s.

speaker_beliefs.MCs.1a <- lmer(projection ~ spBelief_rating + (1|MC.item) + (1|workerid),data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)


anova(speaker_beliefs.MCs.1,speaker_beliefs.MCs.1a)
#orientation is n.s.

speaker_beliefs.MCs.1b <- lmer(projection ~ orientation + (1|MC.item) + (1|workerid),data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)

anova(speaker_beliefs.MCs.1,speaker_beliefs.MCs.1b )
#speaker beliefs are not signifciant

speaker_beliefs.MCs.final <- lmer( projection ~ (1|MC.item) + (1|workerid),data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)

summary(speaker_beliefs.MCs.final)
#this is the same model as political.MCs.final