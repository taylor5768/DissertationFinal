#primary analysis for Exp 1
#based on Judith Tonhauser's code


# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
library(tidyverse)
library(lme4)
library(lmerTest)
library(standardize)
library(gtsummary)
library(gt)
library(emmeans)


# load helper functions
source('../../../helpers.R')
source('../../../model_to_RTF.R')

cd = read.csv("../../data/data_preprocessed.csv")
cd$orientation = as.factor(cd$orientation)
cd$socialInfo = as.factor(cd$socialInfo)
cd$predicate = as.factor(cd$predicate)
cd$predicate <-relevel(cd$predicate,ref="know")
cd$socialInfo<-as.factor(cd$socialInfo)
cd$item <- as.factor(paste(cd$predicate,cd$topic,cd$orientation))
cd$MC.item <- as.factor(paste(cd$topic,cd$orientation))


#cd$FullSpeakerParty[cd$socialInfo == "D"] <- "Democrat"
#cd$FullSpeakerParty[cd$socialInfo == "R"] <- "Republican"
#cd$FullSpeakerParty <- as.factor(cd$FullSpeakerParty)


cd_critical = droplevels(subset(cd,item_type=="critical"))




# what's the min and max number of unique predicate/content combinations?
table(cd_critical$predicate,cd_critical$CC,cd_critical$socialInfo)
min(table(cd_critical$predicate,cd_critical$CC,cd_critical$socialInfo)) # 0!!
max(table(cd_critical$predicate,cd_critical$CC,cd_critical$socialInfo)) # 11
mean(table(cd_critical$predicate,cd_critical$CC,cd_critical$socialInfo)) # 4.42




########################H.1.1a.#################################
#is neutral CC projection NOT affected by speaker politics?####
###############################################################
neutral.full <- lmer(projection~ socialInfo * predicate + (1|item) +(1|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F)

neutral.1<- lmer(projection~ socialInfo + predicate + (1|item) +(1|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F) 

anova(neutral.full,neutral.1)
#27 495.57 634.57 -220.78   441.57 6.0419 11     0.8706
#interaction is not significant

neutral.2<- lmer(projection~ predicate + (1|item) +(1|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F) 

anova(neutral.1,neutral.2)
#16 479.61 561.98 -223.81   447.61 3.928  1    0.04749 *
#socialInfo is significant

neutral.3 <- lmer(projection~ socialInfo + (1|topic) +(1|workerid),data=subset(cd,orientation=="N" &item_type=="critical"),REML=F) 

anova(neutral.1,neutral.3)
#16 479.61 561.98 -223.80   447.61 463.66 11  < 2.2e-16 ***
#predicate is significant - neutral.1 is final model


#add all possible raneffs
neutral.final<- lmer(projection~ socialInfo + predicate + (1+socialInfo|topic/item) +(1+socialInfo|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F) 
#singular fit

neutral.final<- lmer(projection~ socialInfo + predicate + (1|topic/item) +(1|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F) 
#singular fit

neutral.final<- lmer(projection~ socialInfo + predicate + (1+socialInfo|item) +(1+socialInfo|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F)
#singular fit

neutral.final<- lmer(projection~ socialInfo + predicate + (1+socialInfo|item) +(1|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F)
#singular fit

neutral.final<- lmer(projection~ socialInfo + predicate + (1|item) +(1+socialInfo|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F)

neutral.final<- lmer(projection~ socialInfo + predicate + (1|item) +(1+socialInfo|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F)
#converges 

summary(neutral.final)
#(Intercept)            0.86066    0.02873 181.93982  29.958  < 2e-16 ***
#socialInfoR           -0.02978    0.01599 213.31610  -1.862 0.063949 .  
#predicateacknowledge  -0.14893    0.03713 130.89407  -4.011 0.000101 ***

#since it appears  that speaker affiliation is only marginally significant, we will redo the model comparisons with the more complex random effects.

neutral.full.redo <- lmer(projection~ socialInfo * predicate + (1|item) +(1+socialInfo|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F)

neutral.1.redo <-lmer(projection~ socialInfo + predicate + (1|item) +(1+socialInfo|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F)

anova(neutral.full.redo,neutral.1.redo)
#interaction is n.s
#neutral.full.redo   29 497.27 646.58 -219.64   439.27 6.0132 11     0.8725


neutral.2.redo<- lmer(projection~ predicate + (1|item) +(1+socialInfo|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F) 

anova(neutral.1.redo,neutral.2.redo)
#interaction is n.s
#18 481.29 573.96 -222.64   445.29 3.4402  1    0.06363 .


neutral.3.redo<- lmer(projection~ socialInfo + (1|item) +(1+socialInfo|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F) 

anova(neutral.1.redo,neutral.3.redo)
#predicate is significant
#18 481.29 573.96 -222.64   445.29 228.38 11  < 2.2e-16 ***

named_predicate_contrasts <- named_contr_sum(cd$predicate)

neutral.final.redo.sum <- lmer(projection~ predicate + (1|item) +(1+socialInfo|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),contrasts=list(predicate=named_contr_sum(cd$predicate)),REML=F)

summary(neutral.final.redo.sum)
#(Intercept)            0.61379    0.01209 170.96058  50.789  < 2e-16 ***
#  predicateacknowledge   0.23460    0.02529 137.82989   9.277 3.28e-16 ***
#  predicateadmit         0.08318    0.02498 123.64535   3.330  0.00114 ** 
#  predicatebe_annoyed    0.11654    0.02610 154.77532   4.465 1.54e-05 ***
#  predicatebe_right      0.19022    0.02599 142.31465   7.319 1.68e-11 ***
#  predicateconfess      -0.33328    0.02476 123.65871 -13.459  < 2e-16 ***
#  predicatehear          0.04747    0.02466 129.86028   1.925  0.05640 .  
#predicateknow          0.14606    0.02623 150.15218   5.568 1.15e-07 ***
#  predicatepretend      -0.10005    0.02456 117.36270  -4.073 8.45e-05 ***
#  predicatesay          -0.15365    0.02714 181.86795  -5.662 5.76e-08 ***
#  predicatesee           0.19425    0.02552 138.45005   7.612 3.78e-12 ***
 # predicatesuggest      -0.25237    0.02675 154.38949  -9.435  < 2e-16 ***


saveModelAsRTF(neutral.final.redo.sum, '../../models/Exp1Projection_neutral.rtf',3)


################################################################################
###H.1.1b: Are projection ratings higher for conservative CCs w/Republican speakers vs. Democrat speakers and vice versa for liberal CCs#####
###############################################################################

###check MCs####
political.MCs.full <- lmer(projection~ socialInfo * orientation + (1|MC.item) + (1|workerid),
                   data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)

political.MCs.1 <- lmer(projection~ socialInfo + orientation + (1|MC.item) + (1|workerid),
                    data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)

anova(political.MCs.full,political.MCs.1)

summary(political.MCs.full)
saveModelAsRTF(political.MCs.full, '../../models/Exp1Projection_MCs.rtf',2.5)


#(Intercept)                      0.87637    0.02047  16.36870  42.822  < 2e-16 ***
#  socialInfoR                      0.05726    0.02016 208.57837   2.841  0.00495 ** 
#  orientationliberal               0.08235    0.02942  17.20581   2.799  0.01224 *  
#  socialInfoR:orientationliberal  -0.10381    0.02922 212.55547  -3.552  0.00047 ***
#more raneffs are unidentifable

political.full<- lmer(projection~ (socialInfo + orientation + predicate)^3 + (1|item) + (1|workerid),
                          data=subset(cd,item_type=="critical" & orientation !="N"),REML=F) 

political.1<-lmer(projection~ (socialInfo +  orientation + predicate)^2+ (1|item) + (1|workerid),
                 data=subset(cd,item_type=="critical" & orientation !="N"),REML=F) 

anova(political.full,political.1)
#51 829.04 1127.0 -363.52   727.04 9.8235 11     0.5463
#3-way interaction is n.s.

political.1a<-lmer(projection~ socialInfo +  orientation + predicate+ 
                    socialInfo:orientation+
                    orientation:predicate+
                    (1|item) + (1|workerid),
                  data=subset(cd,item_type=="critical" & orientation !="N"),REML=F) 
anova(political.1,political.1a)
# 40 816.87 1050.53 -368.43   736.87 13.272 11     0.2759
#socialInfo:predicate interaction is n.s.

political.1b<-lmer(projection~ socialInfo +  orientation + predicate+ 
                    socialInfo:orientation+
                    socialInfo:predicate+
                    (1|item) + (1|workerid),
                  data=subset(cd,item_type=="critical" & orientation !="N"),REML=F) 
anova(political.1,political.1b)
# 40 816.87 1050.53 -368.43   736.87  19.8 11    0.04816 *
#predicate:orientation interaction is  significant

political.1c<-lmer(projection~ socialInfo +  orientation + predicate+
                    socialInfo:predicate+
                    orientation:predicate +
                    (1|item) + (1|workerid),
                  data=subset(cd,item_type=="critical" & orientation !="N"),REML=F) 


anova(political.1,political.1c)
#political.1    40 816.87 1050.5 -368.43   736.87 117.43  1  < 2.2e-16 ***
#orientation:social condition interaction is significant


#add all possible random effects - more complex raneffs did not converge
political.final <-lmer(projection~ socialInfo +  orientation + predicate+
                     socialInfo:orientation +
                     orientation:predicate +
                     (1+socialInfo|topic/item) + (1+socialInfo*orientation|workerid),
                   data=subset(cd,item_type=="critical" & orientation !="N"),REML=F) 
#converges
named_predicate_contrasts <- named_contr_sum(cd$predicate)

political.final.sum<- lmer(projection~ socialInfo +  orientation + predicate+
                                       socialInfo:orientation +
                                       orientation:predicate +
                                       (1|item) + (1|workerid),
                                     data=subset(cd,item_type=="critical" & orientation !="N"),contrasts=list(predicate=named_contr_sum(cd$predicate)),REML=F)



summary(political.final.sum)
#socialInfoR                              1.145e-01  1.486e-02  2.332e+03   7.707 1.89e-14 ***
#  orientationliberal                       1.496e-01  1.493e-02  7.071e+02  10.020  < 2e-16 ***
#  predicateacknowledge                     1.709e-01  2.498e-02  2.587e+02   6.841 5.66e-11 ***
#  predicateadmit                           1.713e-01  2.464e-02  2.308e+02   6.951 3.68e-11 ***
#  predicatebe_annoyed                      1.124e-01  2.572e-02  2.882e+02   4.372 1.72e-05 ***
#  predicatebe_right                        1.300e-01  2.562e-02  2.653e+02   5.074 7.33e-07 ***
#  predicateconfess                        -3.625e-01  2.443e-02  2.309e+02 -14.838  < 2e-16 ***
#  predicatehear                            4.079e-02  2.433e-02  2.424e+02   1.676 0.094948 .  
#predicateknow                            3.673e-02  2.584e-02  2.797e+02   1.421 0.156319    
#predicatepretend                        -1.419e-01  2.425e-02  2.196e+02  -5.853 1.75e-08 ***
#  predicatesay                            -9.536e-02  2.671e-02  3.381e+02  -3.570 0.000408 ***
#  predicatesee                             1.737e-01  2.519e-02  2.598e+02   6.897 4.04e-11 ***
#  predicatesuggest                        -1.579e-01  2.634e-02  2.876e+02  -5.995 6.09e-09 ***
#  socialInfoR:orientationliberal          -2.304e-01  2.101e-02  2.331e+03 -10.966  < 2e-16 ***
#  orientationliberal:predicateacknowledge  9.593e-02  3.471e-02  2.420e+02   2.764 0.006155 ** 
#  orientationliberal:predicateadmit       -6.325e-02  3.425e-02  2.165e+02  -1.847 0.066180 .  
#orientationliberal:predicatebe_annoyed  -5.784e-03  3.570e-02  2.689e+02  -0.162 0.871403    
#orientationliberal:predicatebe_right     6.858e-03  3.557e-02  2.476e+02   0.193 0.847254    
#orientationliberal:predicateconfess     -6.847e-02  3.398e-02  2.173e+02  -2.015 0.045127 *  
#  orientationliberal:predicatehear         3.855e-02  3.384e-02  2.276e+02   1.139 0.255831    
#orientationliberal:predicateknow         5.247e-02  3.586e-02  2.611e+02   1.463 0.144633    
#orientationliberal:predicatepretend      1.143e-02  3.374e-02  2.065e+02   0.339 0.735091    
#orientationliberal:predicatesay         -3.943e-02  3.702e-02  3.139e+02  -1.065 0.287696    
#orientationliberal:predicatesee         -4.355e-02  3.498e-02  2.434e+02  -1.245 0.214343    
#orientationliberal:predicatesuggest     -1.570e-02  3.652e-02  2.678e+02  -0.430 0.667665 
saveModelAsRTF(political.final.sum, '../../models/Exp1Projection_Political.rtf',6.5)


#sum for speaker affiliation to see the interaction
politicalOrientationOnly <- droplevels(subset(cd,orientation!="N"))
predicateContrasts <- named_contr_sum(politicalOrientationOnly$predicate)
speakerAffiliationContrasts <- named_contr_sum(politicalOrientationOnly$socialInfo)

political.final.sumForAffiliation<- lmer(projection~ socialInfo +  orientation + predicate+
                             socialInfo:orientation +
                             orientation:predicate +
                             (1|item) + (1|workerid),
                           data=subset(cd,item_type=="critical" & orientation !="N"),contrasts=list(predicate=predicateContrasts,socialInfo=speakerAffiliationContrasts),REML=F)

summary(political.final.sumForAffiliation)
#(Intercept)                              5.668e-01  1.210e-02  2.721e+02  46.836  < 2e-16 ***
#  socialInfoD                             -5.727e-02  7.431e-03  2.332e+03  -7.707 1.89e-14 ***
#  orientationliberal                       3.434e-02  1.060e-02  2.404e+02   3.239 0.001370 ** 
#  predicateacknowledge                     1.709e-01  2.498e-02  2.587e+02   6.841 5.66e-11 ***
#  predicateadmit                           1.713e-01  2.464e-02  2.308e+02   6.951 3.68e-11 ***
#  predicatebe_annoyed                      1.124e-01  2.572e-02  2.882e+02   4.372 1.72e-05 ***
#  predicatebe_right                        1.300e-01  2.562e-02  2.653e+02   5.074 7.33e-07 ***
#  predicateconfess                        -3.625e-01  2.443e-02  2.309e+02 -14.838  < 2e-16 ***
#  predicatehear                            4.079e-02  2.433e-02  2.424e+02   1.676 0.094948 .  
#predicateknow                            3.673e-02  2.584e-02  2.797e+02   1.421 0.156319    
#predicatepretend                        -1.419e-01  2.425e-02  2.196e+02  -5.853 1.75e-08 ***
#  predicatesay                            -9.536e-02  2.671e-02  3.381e+02  -3.570 0.000408 ***
#  predicatesee                             1.737e-01  2.519e-02  2.598e+02   6.897 4.04e-11 ***
#  predicatesuggest                        -1.579e-01  2.634e-02  2.876e+02  -5.995 6.09e-09 ***
#  socialInfoD:orientationliberal           1.152e-01  1.051e-02  2.331e+03  10.966  < 2e-16 ***
#  orientationliberal:predicateacknowledge  9.593e-02  3.471e-02  2.420e+02   2.764 0.006155 ** 
#  orientationliberal:predicateadmit       -6.325e-02  3.425e-02  2.165e+02  -1.847 0.066180 .  
#orientationliberal:predicatebe_annoyed  -5.784e-03  3.570e-02  2.689e+02  -0.162 0.871403    
#orientationliberal:predicatebe_right     6.858e-03  3.557e-02  2.476e+02   0.193 0.847254    
#orientationliberal:predicateconfess     -6.847e-02  3.398e-02  2.173e+02  -2.015 0.045127 *  
#  orientationliberal:predicatehear         3.855e-02  3.384e-02  2.276e+02   1.139 0.255831    
#orientationliberal:predicateknow         5.247e-02  3.586e-02  2.611e+02   1.463 0.144633    
#orientationliberal:predicatepretend      1.143e-02  3.374e-02  2.065e+02   0.339 0.735091    
#orientationliberal:predicatesay         -3.943e-02  3.702e-02  3.139e+02  -1.065 0.287696    
#orientationliberal:predicatesee         -4.355e-02  3.498e-02  2.434e+02  -1.245 0.214343    
#orientationliberal:predicatesuggest     -1.570e-02  3.652e-02  2.678e+02  -0.430 0.667665 


saveModelAsRTF(political.final.sumForOrientation, '../../models/Exp1Projection_Political_sumForAffiliation.rtf',6.5)

###############################################################################
#####Beliefs#####
#############################################################################

#relationship b/t speaker and listener beliefs
cor(cd_critical$participant_beliefs,cd_critical$spBelief_rating,use="complete.obs")
#r=.1173

#similarity by speaker
cd_critical$participantIdeologyBinary <- ""

cd_critical<-cd_critical %>% mutate(participantIdeologyBinary = case_when(
  politics<.5 & socialInfo =="D" ~ "match",
  politics<.5 & socialInfo =="R" ~ "mismatch",
  politics>=.5 & socialInfo =="R" ~ "match",
  politics>=.5 & socialInfo =="D" ~ "mismatch",
))

bothBeliefs.full <- lmer(projection ~ spBelief_rating + participant_beliefs*participantIdeologyBinary + predicate + (1|workerid) + (1|item), data=subset(cd_critical,orientation!="N"), REML="F")
vif(bothBeliefs.full)


bothBeliefs.1 <- lmer(projection ~ spBelief_rating + participant_beliefs+participantIdeologyBinary + predicate +(1|workerid) + (1|item), data=subset(cd_critical,orientation!="N"),REML="F")

anova(bothBeliefs.full,bothBeliefs.1)
#participant beliefs x match is n.s.

bothBeliefs.1a <- lmer(projection ~ spBelief_rating + participant_beliefs+participantIdeologyBinary +(1|workerid) + (1|item), data=subset(cd_critical,orientation!="N"),REML="F")

anova(bothBeliefs.1,bothBeliefs.1a)
#predicate is significant

bothBeliefs.1b <- lmer(projection ~ participant_beliefs+participantIdeologyBinary + predicate + (1|workerid) + (1|item), data=subset(cd_critical,orientation!="N"),REML="F")

anova(bothBeliefs.1,bothBeliefs.1b)
#speaker beliefs is significant

bothBeliefs.1c <- lmer(projection ~ spBelief_rating +participantIdeologyBinary + predicate +(1|workerid) + (1|item), data=subset(cd_critical,orientation!="N"),REML="F")

anova(bothBeliefs.1,bothBeliefs.1c)
#participant beliefs are n.s.

bothBeliefs.1d <- lmer(projection ~ spBelief_rating + participant_beliefs + predicate +(1|workerid) + (1|item), data=subset(cd_critical,orientation!="N"),REML="F")

anova(bothBeliefs.1,bothBeliefs.1d)
#ideologyBinary is n.s.

bothBeliefs.final<- lmer(projection ~ spBelief_rating + predicate +(1+spBelief_rating|workerid) + (1|topic/item), data=subset(cd_critical,orientation!="N"),contrasts=list(predicate=named_contr_sum(cd$predicate)),REML="F")

summary(bothBeliefs.final)

vif(bothBeliefs.final)

saveModelAsRTF(bothBeliefs.final, '../../models/Exp1Projection_bySpeakerBeliefsAndListenerBeliefs.rtf',3.5)

################################################################################
#H.1.3.a for political items are speaker belief ratings predicted by socialInfo x orientation interaction?
###############################################################################
speaker_beliefs_interaction.full<- lmer(spBelief_rating ~ socialInfo * orientation + (1|CC) + (1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

speaker_beliefs_interaction.1<- lmer(spBelief_rating ~ socialInfo + orientation + (1|CC) + (1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)


anova(speaker_beliefs_interaction.full,speaker_beliefs_interaction.1)
#interaction is significant
#7 -525.59 -484.7   269.8  -539.59 2548.2  1  < 2.2e-16 ***

#no convergence with more complex raneffs
summary(speaker_beliefs_interaction.full)
#Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)                            0.80871    0.01807   31.37457   44.76   <2e-16 #***
#  socialInfoR                           -0.59823    0.01209 2321.43756  -49.49   <2e-16 ***
#  orientationconservative               -0.64301    0.02536   30.49009  -25.35   <2e-16 ***
#  socialInfoR:orientationconservative    1.14340    0.01709 2321.43756   66.89   <2e-16 ***

saveModelAsRTF(speaker_beliefs_interaction.full, '../../models/Exp1SpBeliefs_Political_bySpAffilAndOrientation.rtf',2.5)

#main clause contents
speaker_beliefs_interaction.MC.full<- lmer(spBelief_rating ~ socialInfo * orientation + (1|MC.item) + (1|workerid),data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)
#singular fit - can't get anything less complex to work

speaker_beliefs_interaction.MC.full<- lmer(spBelief_rating ~ socialInfo * orientation + (1|MC.item),data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)

speaker_beliefs_interaction.MC.1<- lmer(spBelief_rating ~ socialInfo + orientation + (1|MC.item),data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)

anova(speaker_beliefs_interaction.MC.full,speaker_beliefs_interaction.MC.1)
#interaction is significant

summary(speaker_beliefs_interaction.MC.full)

#(Intercept)                      0.19980    0.03948   6.17394   5.061  0.00212 ** 
#  socialInfoR                      0.39968    0.03523 420.10759  11.343  < 2e-16 ***
#  orientationliberal               0.61821    0.05637   6.39319  10.967 2.21e-05 ***
 # socialInfoR:orientationliberal  -0.87608    0.05126 423.65380 -17.091  < 2e-16 ***


saveModelAsRTF(speaker_beliefs_interaction.MC.full, '../models/Exp1SpBeliefs_Political_bySpAffilAndOrientation_MCs.rtf',2.2)

################################################################################
###H.1.3b: are projection ratings positively correlated with speaker belief ratings? CCs##############################################################################

#main clauses
speaker_beliefs.full.MC <- lmer(projection ~ spBelief_rating * orientation + (1|MC.item) + (1|workerid),data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)


speaker_beliefs.1.MC <- lmer(projection ~ spBelief_rating + orientation + (1|MC.item) + (1|workerid),data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)


anova(speaker_beliefs.full.MC, speaker_beliefs.1.MC)
#interaction is n.s.

speaker_beliefs.1a.MC <- lmer(projection ~ orientation  + (1|MC.item) + (1|workerid),data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)

anova(speaker_beliefs.1.MC, speaker_beliefs.1a.MC)
# speaker beliefs are significant

speaker_beliefs.1c.MC <- lmer(projection ~ spBelief_rating + (1|MC.item) + (1|workerid),data=subset(cd,item_type=="MC" & orientation !="N"),REML=F)

anova(speaker_beliefs.1.MC, speaker_beliefs.1c.MC)
# orientation is n.s.

summary(speaker_beliefs.1c.MC)

###critical items
speaker_beliefs.full <- lmer(projection ~ (spBelief_rating + orientation + predicate)^3 + (1|item) + (1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)


speaker_beliefs.1 <- lmer(projection ~ (spBelief_rating + orientation + predicate)^2 + (1|item) + (1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

anova(speaker_beliefs.full,speaker_beliefs.1)  #3-way interaction is n.s.
#51 786.54 1084.5 -342.27   684.54 10.194 11      0.513

speaker_beliefs.1a <- lmer(projection ~ spBelief_rating + orientation + predicate + spBelief_rating:orientation + spBelief_rating:predicate + (1|item) + (1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

anova(speaker_beliefs.1,speaker_beliefs.1a)
# orientatin:predicate interaction is n.s. - but marginal
#40 774.73 1008.39 -347.37   694.73 18.394 11    0.07288 .



speaker_beliefs.1b <- lmer(projection ~ spBelief_rating + orientation + predicate + orientation:predicate + spBelief_rating:predicate + (1|item) + (1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

anova(speaker_beliefs.1,speaker_beliefs.1b)
#orientation:spBelief_rating is n.s.
#40 774.73 1008.4 -347.37   694.73 0.2384  1     0.6253

speaker_beliefs.1c <- lmer(projection ~ spBelief_rating + orientation + predicate + orientation:predicate + orientation:spBelief_rating + (1|item) + (1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)


anova(speaker_beliefs.1,speaker_beliefs.1c)
#predicate:spBelief_rating is n.s. - but marginal
#40 774.73 1008.39 -347.37   694.73 17.898 11    0.08398 .


speaker_beliefs.2 <- lmer(projection ~ spBelief_rating + orientation + predicate + (1|item) + (1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

speaker_beliefs.2a <- lmer(projection ~  orientation + predicate + (1|item) + (1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

anova(speaker_beliefs.2,speaker_beliefs.2a)
#sp beliefs are significant
#17 766.03  865.33 -366.01   732.03 159.09  1  < 2.2e-16 ***

speaker_beliefs.2b <- lmer(projection ~  spBelief_rating+predicate + (1|item) + (1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

anova(speaker_beliefs.2,speaker_beliefs.2b)
#orientation is n.s.
#17 766.03 865.33 -366.01   732.03 3.8111  1    0.05091 .



speaker_beliefs.2c <- lmer(projection ~ spBelief_rating + orientation  + (1|item) + (1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

anova(speaker_beliefs.2,speaker_beliefs.2c)
#predicate is significant
#17  766.03  865.33 -366.01   732.03 419.43 11  < 2.2e-16

speaker_beliefs.final <- lmer(projection ~ spBelief_rating + predicate + (1|item) + (1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

speaker_beliefs.final <- lmer(projection ~ spBelief_rating + predicate + (1|item) + (1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),contrasts=list(predicate=predicateContrasts),REML=F)

summary(speaker_beliefs.final)
saveModelAsRTF(speaker_beliefs.final, '../../models/Exp1Projection_bySpeakerBeliefs.rtf',3.5)


#look at marginal interaction effect
speaker_beliefs.marginal = lmer(projection~ spBelief_rating+  orientation + predicate+
                          
                          orientation:predicate +
                          +predicate:spBelief_rating+
                          (1|item) + (1|workerid),
                        data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)
summary(speaker_beliefs.marginal)

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
#speaker beliefs are significant

summary(speaker_beliefs.MCs.1a)
saveModelAsRTF(speaker_beliefs.MCs.1a, '../../models/Exp1Projection_bySpeakerBeliefs_MCs.rtf',1.5)

###speaker evals
#empathy 

empathy.full <- lmer(projection ~ politics *empathy_rating *socialInfo *orientation + (1|item) + (1|workerid), data = subset(cd,item_type=="critical" & orientation !="N"))

empathy.1 <- lmer(projection ~ (politics + empathy_rating + socialInfo + orientation)^3 + (1|item) + (1|workerid), data = subset(cd,item_type=="critical" & orientation !="N"))

anova(empathy.full,empathy.1)
empathy.2 <- lmer(projection ~ politics + empathy_rating + socialInfo + orientation + politics:empathy_rating + politics:socialInfo + politics:orientation + empathy_rating:socialInfo + empathy_rating:orientation + socialInfo:orientation + (1|item) + (1|workerid), data = subset(cd,item_type=="critical" & orientation !="N"))

anova(empathy.1,empathy.2)


empathy.2a<- lmer(projection ~ politics + empathy_rating + socialInfo + orientation + politics:socialInfo + politics:orientation + empathy_rating:socialInfo + empathy_rating:orientation + socialInfo:orientation + (1|item) + (1|workerid), data = subset(cd,item_type=="critical" & orientation !="N"))

anova(empathy.2,empathy.2a)

empathy.2b <- lmer(projection ~ politics + empathy_rating + socialInfo + orientation + politics:empathy_rating  + politics:orientation + empathy_rating:socialInfo + empathy_rating:orientation + socialInfo:orientation + (1|item) + (1|workerid), data = subset(cd,item_type=="critical" & orientation !="N"))

anova(empathy.2,empathy.2b)

empathy.2c <- lmer(projection ~ politics + empathy_rating + socialInfo + orientation + politics:empathy_rating + politics:socialInfo  + empathy_rating:socialInfo + empathy_rating:orientation + socialInfo:orientation + (1|item) + (1|workerid), data = subset(cd,item_type=="critical" & orientation !="N"))

anova(empathy.2,empathy.2c)
#empathy.2    14 1188.0 1269.8 -579.99   1160.0 3.0897  1    0.07879 .

empathy.2d <- lmer(projection ~ politics + empathy_rating + socialInfo + orientation + politics:empathy_rating + politics:socialInfo + politics:orientation  + empathy_rating:orientation + socialInfo:orientation + (1|item) + (1|workerid), data = subset(cd,item_type=="critical" & orientation !="N"))

anova(empathy.2,empathy.2d)

empathy.2e <- lmer(projection ~ politics + empathy_rating + socialInfo + orientation + politics:empathy_rating + politics:socialInfo + politics:orientation + empathy_rating:socialInfo  + socialInfo:orientation + (1|item) + (1|workerid), data = subset(cd,item_type=="critical" & orientation !="N"))


anova(empathy.2,empathy.2e)
#empathy_rating:orientation is significant

empathy.2f <- lmer(projection ~ politics + empathy_rating + socialInfo + orientation + politics:empathy_rating + politics:socialInfo + politics:orientation + empathy_rating:socialInfo + empathy_rating:orientation + (1|item) + (1|workerid), data = subset(cd,item_type=="critical" & orientation !="N"))

anova(empathy.2,empathy.2f)
#socialInfo:orientation is significant

empathy.final <- lmer(projection ~ politics + empathy_rating + socialInfo + orientation + empathy_rating:orientation + socialInfo:orientation + (1|item) + (1|workerid), data = subset(cd,item_type=="critical" & orientation !="N"))

summary(empathy.final)

cd$socialInfo <- relevel(cd$socialInfo, ref = "R")
empathy.final <- lmer(projection ~ politics + empathy_rating + socialInfo + orientation + empathy_rating:orientation + socialInfo:orientation + (1|item) + (1|workerid), data = subset(cd,item_type=="critical" & orientation !="N"))
summary(empathy.final)
