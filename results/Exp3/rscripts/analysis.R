#primary analysis for Exp 3
#based on Judith Tonhauser's code


# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)
library(tidyverse)
library(lme4)
library(lmerTest)
library(standardize)


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

# what's the min and max number of unique predicate/content combinations?
table(cd_critical$predicate,cd_critical$CC,cd_critical$socialInfo)
min(table(cd_critical$predicate,cd_critical$CC,cd_critical$socialInfo)) # 1
max(table(cd_critical$predicate,cd_critical$CC,cd_critical$socialInfo)) # 14
mean(table(cd_critical$predicate,cd_critical$CC,cd_critical$socialInfo)) # 6.25



################################################################
#is neutral CC projection NOT affected by speaker politics?####
###############################################################


neutral.full <- lmer(projection~ socialInfo * predicate + (1|item) +(1|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F)


neutral.1<- lmer(projection~ socialInfo + predicate + (1|item) +(1|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F) 


anova(neutral.full,neutral.1)
#interaction is n.s.
#27 500.22 646.55 -223.11   446.22 3.4862 11     0.9826

neutral.2<- lmer(projection~ predicate + (1|item) +(1|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F) 

anova(neutral.1,neutral.2)
#socialInfo is n.s
#16 481.71 568.42 -224.85   449.71 0.0126  1     0.9106

neutral.3 <- lmer(projection~ socialInfo + (1|topic) +(1|workerid),data=subset(cd,orientation=="N" &item_type=="critical"),REML=F) 

anova(neutral.1,neutral.3)
#16  481.71  568.42 -224.85   449.71 810.98 11  < 2.2e-16 ***
#predicate is significant - neutral.2 is final model

#this is the most raneffs that would converge
neutral.final<- lmer(projection~ predicate + (1|item) +(1|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),REML=F) 


neutral.final.sum <- lmer(projection~ predicate + (1|item) +(1|workerid),data=subset(cd,orientation=="N" & item_type=="critical"),contrasts=list(predicate=named_contr_sum(cd$predicate)),REML=F)

summary(neutral.final.sum)
saveModelAsRTF(neutral.final.sum, '../models/Exp3Projection_neutral.rtf',3)

#(Intercept)            0.66116    0.01089 208.40905  60.729  < 2e-16 ***
#  predicateacknowledge   0.16900    0.02812 131.30632   6.009 1.72e-08 ***
#  predicateadmit         0.13448    0.02829 131.68911   4.754 5.16e-06 ***
 # predicatebe_annoyed    0.18778    0.02875 144.38210   6.533 1.03e-09 ***
#  predicatebe_right     -0.38652    0.02766 127.28952 -13.974  < 2e-16 ***
#  predicateconfess       0.04086    0.02698 111.68548   1.515    0.133    
#predicatehear          0.20604    0.02797 133.78125   7.366 1.62e-11 ***
#  predicateknow          0.25396    0.02747 120.24082   9.246 1.06e-15 ***
#  predicatepretend      -0.15855    0.02863 135.82696  -5.539 1.52e-07 ***
#  predicatesay          -0.25219    0.02766 122.39811  -9.119 1.86e-15 ***
#  predicatesee           0.16904    0.02839 138.10394   5.954 2.06e-08 ***
#  predicatesuggest      -0.25891    0.02759 121.70746  -9.383 4.53e-16 ***

################################################################################
###Are projection ratings higher for conservative CCs w/Republican speakers vs. Democrat speakers and vice versa for liberal CCs?#####
###############################################################################

####check MCs  - cannot do mixed effects here because we only have one observation for each participant So we'll do a simple linear model instead.

politicalMCs <- subset(cd, item_type=="MC" & orientation!="N")
mean(politicalMCs$projection)

political.MCs.full <- lm(projection~ socialInfo * orientation, data=subset(cd,item_type=="MC" & orientation !="N"))

political.MCs.1 <- lm(projection~ socialInfo + orientation, data=subset(cd,item_type=="MC" & orientation !="N"))

anova(political.MCs.full,political.MCs.1)
#interaction is n.s.

political.MCs.1a <- lm(projection~  orientation, data=subset(cd,item_type=="MC" & orientation !="N"))

anova(political.MCs.1,political.MCs.1a)
#social info is n.s.

political.MCs.1b <- lm(projection~  socialInfo, data=subset(cd,item_type=="MC" & orientation !="N"))

anova(political.MCs.1,political.MCs.1b)
#orientation is n.s.

political.MCs.final <- lm(projection~  1, data=subset(cd,item_type=="MC" & orientation !="N"))
summary(political.MCs.final)

summary(political.MCs.final)


##look at critical political items
political.full<- lmer(projection~ (socialInfo + orientation + predicate)^3 + (1|item) + (1|workerid),
                      data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)


political.1<-lmer(projection~ (socialInfo +  orientation + predicate)^2+ (1|item) + (1|workerid),
                 data=subset(cd,item_type=="critical" & orientation !="N"),REML=F) 

anova(political.full,political.1)
#51 481.16 792.90 -189.58   379.16 16.411 11     0.1265
#3-way interaction is n.s.

political.1a<-lmer(projection~ socialInfo +  orientation + predicate+ 
                    socialInfo:orientation+
                    orientation:predicate+
                    (1|item) + (1|workerid),
                  data=subset(cd,item_type=="critical" & orientation !="N"),REML=F) 
anova(political.1,political.1a)
# 40 475.57 720.07 -197.78   395.57 19.871 11    0.04714 *
#socialInfo:predicate interaction is significant


political.1b<-lmer(projection~ socialInfo +  orientation + predicate+ 
                     socialInfo:orientation+
                     socialInfo:predicate +
                     (1|item) + (1|workerid),
                   data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)
anova(political.1,political.1b)
#predicate:orientation is n.s.
#40 475.57 720.07 -197.78   395.57 4.3176 11     0.9597


political.1c<-lmer(projection~ socialInfo +  orientation + predicate+ 
                     socialInfo:predicate +
                     predicate:orientation +
                     (1|item) + (1|workerid),
                   data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)
anova(political.1,political.1c) 
#socialInfo:orientation is significant
#40 475.57 720.07 -197.78   395.57 13.871  1  0.0001958 ***


political.final <- lmer(projection~ socialInfo +  orientation + predicate+ 
                      socialInfo:orientation+predicate:socialInfo +
                      (1|item) + (1|workerid),
                    data=subset(cd,item_type=="critical" & orientation !="N"),REML=F) 

#try more complex raneffs - below is most complex that would converge
political.final <- lmer(projection~ socialInfo +  orientation + predicate+ 
                          socialInfo:orientation+predicate:socialInfo +
                          (1|topic/item) + (1+socialInfo|workerid),
                        data=subset(cd,item_type=="critical" & orientation !="N"),REML=F) 

political.final.sum <- lmer(projection~ socialInfo +  orientation + predicate+ 
                              socialInfo:orientation+predicate:socialInfo +
                              (1|topic/item) + (1+socialInfo|workerid),
                            data=subset(cd,item_type=="critical" & orientation !="N"),contrasts=list(predicate=named_contr_sum(cd$predicate)),REML=F)

summary(political.final.sum)
saveModelAsRTF(political.final.sum, '../models/Exp3Projection_Political.rtf',6.5)


#(Intercept)                       6.423e-01  1.225e-02  1.099e+02  52.426  < 2e-16 ***
#  socialInfoS                       2.615e-02  1.232e-02  8.726e+02   2.123 0.034035 *  
#  orientationliberal                3.911e-02  1.274e-02  6.474e+02   3.070 0.002230 ** 
 # predicateacknowledge              2.511e-01  2.148e-02  6.701e+02  11.689  < 2e-16 ***
#  predicateadmit                    1.946e-01  2.177e-02  7.444e+02   8.936  < 2e-16 ***
#  predicatebe_annoyed               1.617e-01  2.296e-02  7.887e+02   7.043 4.10e-12 ***
#  predicatebe_right                 8.370e-02  2.270e-02  8.446e+02   3.687 0.000241 ***
#  predicateconfess                 -4.747e-01  2.161e-02  6.810e+02 -21.969  < 2e-16 ***
#  predicatehear                     6.750e-02  2.209e-02  6.648e+02   3.056 0.002333 ** 
#  predicateknow                     1.732e-01  2.212e-02  7.138e+02   7.829 1.78e-14 ***
 # predicatepretend                 -2.855e-01  2.215e-02  7.051e+02 -12.885  < 2e-16 ***
 # predicatesay                     -1.178e-01  2.336e-02  8.645e+02  -5.042 5.62e-07 ***
#  predicatesee                      1.844e-01  2.292e-02  8.253e+02   8.047 2.96e-15 ***
 # predicatesuggest                 -2.093e-01  2.171e-02  7.098e+02  -9.643  < 2e-16 ***
 # socialInfoS:orientationliberal   -6.105e-02  1.671e-02  2.732e+03  -3.653 0.000264 ***
 # socialInfoS:predicateacknowledge  7.154e-03  2.914e-02  3.287e+03   0.246 0.806060    
#socialInfoS:predicateadmit       -4.972e-02  3.028e-02  3.298e+03  -1.642 0.100668    
#socialInfoS:predicatebe_annoyed  -3.960e-02  3.034e-02  3.265e+03  -1.305 0.191830    
#socialInfoS:predicatebe_right    -3.177e-02  3.118e-02  3.171e+03  -1.019 0.308410    
#socialInfoS:predicateconfess      6.726e-02  2.971e-02  3.281e+03   2.263 0.023673 *  
#  socialInfoS:predicatehear        -2.552e-02  2.845e-02  3.299e+03  -0.897 0.369839    
#socialInfoS:predicateknow        -1.526e-02  3.040e-02  3.303e+03  -0.502 0.615714    
#socialInfoS:predicatepretend      8.104e-02  3.105e-02  3.285e+03   2.610 0.009105 ** 
 # socialInfoS:predicatesay         -1.013e-02  2.954e-02  3.202e+03  -0.343 0.731615    
#socialInfoS:predicatesee         -2.867e-02  3.066e-02  3.207e+03  -0.935 0.349713    
#socialInfoS:predicatesuggest      4.408e-02  2.917e-02  3.272e+03   1.511 0.130860 

politicalOrientationOnly <- droplevels(subset(cd,orientation!="N"))
predicateContrasts <- named_contr_sum(politicalOrientationOnly$predicate)
orientationContrasts <- named_contr_sum(politicalOrientationOnly$orientation)

political.final.sumForOrientation <- lmer(projection~ socialInfo +  orientation + predicate+ 
                              socialInfo:orientation+predicate:socialInfo +
                              (1|topic/item) + (1+socialInfo|workerid),
                            data=subset(politicalOrientationOnly,item_type=="critical"),contrasts=list(predicate=predicateContrasts,orientation=orientationContrasts),REML=F)

summary(political.final.sumForOrientation)


saveModelAsRTF(political.final.sumForOrientation, '../models/Exp3Projection_Political_sumForOrientation.rtf',6.5)


##does empathy play a role?
projection_empathy.full <- lmer(projection ~ empathy_rating + socialInfo + orientation + politics + predicate + socialInfo:orientation + empathy_rating:politics + (1|item) + (1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),contrasts=list(predicate=named_contr_sum(cd$predicate)),REML=F)

projection_empathy.1 <- lmer(projection ~ empathy_rating + socialInfo + orientation + politics + predicate + socialInfo:orientation + (1|item) + (1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),contrasts=list(predicate=named_contr_sum(cd$predicate)),REML=F) 

anova(projection_empathy.full,projection_empathy.1)

summary(projection_empathy.full)



###############################################################################
#####Beliefs#####
#############################################################################

#relationship b/t speaker and listener beliefs
cor(cd_critical$participant_beliefs,cd_critical$spBelief_rating,use="complete.obs")
#r=0.0117166



#similarity by speaker
bothBeliefs.full <- lmer(projection ~ spBelief_rating + participant_beliefs*similarity + predicate + (1|workerid) + (1|item), data=subset(cd_critical,orientation!="N"&similarity!="NA"), REML="F")
vif(bothBeliefs.full)

bothBeliefs.1 <- lmer(projection ~ spBelief_rating + participant_beliefs+similarity + predicate + (1|workerid) + (1|item), data=subset(cd_critical,orientation!="N"& similarity!="NA"), REML="F")

anova(bothBeliefs.full,bothBeliefs.1)
#interaction is n.s. 


bothBeliefs.1a <- lmer(projection ~ spBelief_rating + participant_beliefs+similarity + (1|workerid) + (1|item), data=subset(cd_critical,orientation!="N"& similarity!="NA"), REML="F")

anova(bothBeliefs.full,bothBeliefs.1a)
#predicate is significant

bothBeliefs.1b <- lmer(projection ~  participant_beliefs+similarity + predicate + (1|workerid) + (1|item), data=subset(cd_critical,orientation!="N"& similarity!="NA"), REML="F")

anova(bothBeliefs.1,bothBeliefs.1b)
#speaker beliefs is significant

bothBeliefs.1c<- lmer(projection ~ spBelief_rating + similarity + predicate + (1|workerid) + (1|item), data=subset(cd_critical,orientation!="N"& similarity!="NA"), REML="F")

anova(bothBeliefs.1,bothBeliefs.1c)
#participant beliefs are n.s. (marginal)

bothBeliefs.1d <- lmer(projection ~ spBelief_rating + participant_beliefs+ predicate + (1|workerid) + (1|item), data=subset(cd_critical,orientation!="N"& similarity!="NA"), REML="F")

anova(bothBeliefs.1,bothBeliefs.1d)
#similarity is n.s. 


bothBeliefs.final <- lmer(projection ~ spBelief_rating + predicate + (1|workerid) + (1|item), data=subset(cd_critical,orientation!="N"& similarity!="NA"), contrasts=list(predicate=named_contr_sum(cd$predicate)),REML="F")


summary(bothBeliefs.final)
saveModelAsRTF(bothBeliefs.final, '../models/Projection_from_bothBeliefs.rtf',3.5)


################################################################################
#H.2.3.a for political items are speaker belief ratings predicted by socialInfo x orientation interaction?
###############################################################################

#would not converge with participant raneff.
speaker_beliefs_interaction.full<- lmer(spBelief_rating ~ socialInfo * orientation + (1|item) ,data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

speaker_beliefs_interaction.1<- lmer(spBelief_rating ~ socialInfo + orientation + (1|item) ,data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)


anova(speaker_beliefs_interaction.full,speaker_beliefs_interaction.1)
#interaction is significant
#  6 -423.49 -386.81  217.74  -435.49 792.29  1  < 2.2e-16 ***

speaker_beliefs_interaction.final<- lmer(spBelief_rating ~ socialInfo * orientation + (1|topic/item),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

summary(speaker_beliefs_interaction.final)
#(Intercept)                       0.36993    0.01163   31.70054   31.81   <2e-16 ***
#  socialInfoS                       0.22881    0.01102 3293.16772   20.77   <2e-16 ***
#  orientationliberal                0.21575    0.01219  697.14527   17.70   <2e-16 ***
#  socialInfoS:orientationliberal   -0.46654    0.01559 3291.45944  -29.93   <2e-16 ***

saveModelAsRTF(speaker_beliefs_interaction.final, '../models/SpBeliefsBySpAccentAndOrientation.rtf',2.5)

################################################################################
###H.2.3b: are projection ratings positively correlated with speaker belief ratings? CCs#########################################################
#5.2
speaker_beliefs.full <- lmer(projection ~ (spBelief_rating + predicate + orientation)^3 + (1|item)+(1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)


speaker_beliefs.1 <- lmer(projection ~ (spBelief_rating + predicate + orientation)^2 + (1|item)+(1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

anova(speaker_beliefs.full,speaker_beliefs.1)
#3-way interaction is significant

#add more complex raneffs
speaker_beliefs.final <- lmer(projection ~ spBelief_rating * predicate * orientation + (1|topic/item)+(1+spBelief_rating|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),contrasts=list(predicate=named_contr_sum(cd$predicate)),REML=F)

summary(speaker_beliefs.final)
saveModelAsRTF(speaker_beliefs.final, '../models/Exp3Projection_bySpeakerBeliefs.rtf',8)

cd_political_critical <- subset(cd, orientation!="N" & item_type=="critical")

speaker_beliefs.final.sum <- lmer(projection ~ spBelief_rating* predicate * orientation + (1|topic/item)+(1+spBelief_rating|workerid),data=subset(cd_political_critical),contrasts=list(predicate=named_contr_sum(cd_political_critical$predicate),orientation=named_contr_sum(cd_political_critical$orientation)),REML=F)

summary(speaker_beliefs.final.sum)


####check MCs - can't do mixed effects since I only used one topic
speaker_beliefs.MCs.full <- lm(projection ~ spBelief_rating * orientation ,data=subset(cd,item_type=="MC" & orientation !="N"))

speaker_beliefs.MCs.1 <- lm(projection ~ spBelief_rating + orientation ,data=subset(cd,item_type=="MC" & orientation !="N"))

anova(speaker_beliefs.MCs.full,speaker_beliefs.MCs.1 )
#interaction is n.s.
#275 2.1917 -1 -0.0075572 0.948 0.331

speaker_beliefs.MCs.1a <- lm(projection ~ spBelief_rating ,data=subset(cd,item_type=="MC" & orientation !="N"))


anova(speaker_beliefs.MCs.1,speaker_beliefs.MCs.1a)
#orientation is n.s.

speaker_beliefs.MCs.1b <- lm(projection ~ orientation,data=subset(cd,item_type=="MC" & orientation !="N"))

anova(speaker_beliefs.MCs.1,speaker_beliefs.MCs.1b )
#speaker beliefs are not signifciant

speaker_beliefs.MCs.final <- lm(projection ~ 1,data=subset(cd,item_type=="MC" & orientation !="N"))

summary(speaker_beliefs.MCs.final)

################################################################################
###H.2.3c: are Southerners more likely to be perceived as Republicans etc.? #########################################################

####

#For ELM

speaker_beliefs.elm.full <- lmer(projection ~ spBelief_rating + predicate + (1|item)+(1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

speaker_beliefs.elm.1 <- lmer(projection ~  predicate + (1|item)+(1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

anova(speaker_beliefs.elm.full,speaker_beliefs.elm.1)
#16 446.43 544.23 -207.21   414.43 19.544  1   9.83e-06 ***

speaker_beliefs.elm.2 <- lmer(projection ~  spBelief_rating + (1|item)+(1|workerid),data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

anova(speaker_beliefs.elm.full,speaker_beliefs.elm.2)
#16 446.43 544.23 -207.21   414.43 541.64 11  < 2.2e-16 ***

summary(speaker_beliefs.elm.full)
#Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)             0.87804    0.01985  442.28568  44.243  < 2e-16 ***
#  spBelief_rating         0.07519    0.01698 3167.88894   4.429 9.77e-06 ***
#  predicateacknowledge   -0.08325    0.02437  268.18440  -3.416 0.000734 ***
#  predicateadmit         -0.11175    0.02451  270.90911  -4.559 7.78e-06 ***
#  predicatebe_annoyed    -0.18272    0.02476  287.85466  -7.379 1.72e-12 ***
#  predicatebe_right      -0.69859    0.02421  269.04999 -28.860  < 2e-16 ***
 ## predicateconfess       -0.19988    0.02368  241.45208  -8.443 2.90e-15 ***
 # predicatehear          -0.08754    0.02439  278.87672  -3.589 0.000392 ***
#  predicatepretend       -0.50258    0.02469  277.01467 -20.358  < 2e-16 ***
#  predicatesay           -0.37731    0.02416  260.69329 -15.615  < 2e-16 ***
#  predicatesee           -0.08719    0.02464  283.42888  -3.538 0.000471 ***
#  predicatesuggest       -0.44197    0.02406  255.57857 -18.371  < 2e-16 ***
#  predicatethink         -0.28351    0.02460  281.44458 -11.524  < 2e-16 ***
