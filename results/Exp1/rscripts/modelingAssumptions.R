########################
###PROJECTION RATINGS####
######################

#primary graphs for Exp. 1
#based on Judith Tonhauser's code


# set working directory to directory of script
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
library(ggforce)
library(gridExtra)
library(lme4)
library(broom)
library(car)



# load helper functions
source('../../helpers.R')

cd = read.csv("../data/data_preprocessed.csv")
cd$orientation<-as.factor(cd$orientation)



length(unique(cd$workerid))

###################################
##1. Checking distribution of responses 
###################################

#################
## 1A. projection#
##################
certaintyRatingDist <- ggplot(subset(cd,item_type=="critical" & projection!="NA" & orientation!="N"), aes(x=projection)) +
  geom_histogram(bins=50) +
  xlab("Certainty rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=.1))


certaintyRatingDist
#ggsave(certaintyRatingDist,filename="../graphs/modelingAssumptions/certaintyRatingDist.png")

ggplot(subset(cd,item_type=="critical" & projection!="NA" & socialInfo=="D" & orientation =="liberal"), aes(x=projection)) +
  geom_histogram(bins=50) +
  xlab("Certainty rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=.1))

ggplot(subset(cd,item_type=="critical" & projection!="NA" & socialInfo=="D" & orientation =="conservative"), aes(x=projection)) +
  geom_histogram(bins=50) +
  xlab("Certainty rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=.1))

ggplot(subset(cd,item_type=="critical" & projection!="NA" & socialInfo=="R" & orientation =="liberal"), aes(x=projection)) +
  geom_histogram(bins=50) +
  xlab("Certainty rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=.1))

ggplot(subset(cd,item_type=="critical" & projection!="NA" & socialInfo=="R" & orientation =="conservative"), aes(x=projection)) +
  geom_histogram(bins=50) +
  xlab("Certainty rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=.1))


socialInfo.labs <- c("D", "R")
names(socialInfo.labs) <- c("Democrat","Republican")
orientation.labs <- c("conservative", "liberal")
names(orientation.labs) <- c("conservative","liberal")

ggplot(subset(cd,item_type=="critical" & projection!="NA" &orientation!="N"), aes(x=projection)) +
  geom_histogram(bins=50) +
  xlab("Certainty rating") +
  ylab("Number of ratings") +
  facet_grid(socialInfo~orientation,
    #         labeller = labeller(socialInfo = socialInfo.labs)
             )+
  scale_x_continuous(breaks=seq(0,1,by=.1))

ggsave("../graphs/histogramSocialInfo.png")


#look across participants
certaintyRatingDistByParticipant <- certaintyRatingDist+
  facet_wrap_paginate(~workerid, ncol = 6, nrow = 8, page = 1)

n <- n_pages(certaintyRatingDistByParticipant)
n

pdf('../graphs/modelingAssumptions/certaintyByParticipant.pdf',paper ="letter")

for(i in 1:n){
  print(certaintyRatingDistByParticipant + facet_wrap_paginate(~workerid, ncol = 6, nrow = 8, page = i))
}

dev.off()
dev.off()

################################
# 1B. speaker belief ratings#####
##############################

spBeliefRatingDist <- ggplot(subset(cd,(item_type=="critical" & orientation!="N")), aes(x=spBelief_rating)) +
  geom_histogram(bins=50) +
  xlab("Speaker belief rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=.1))

spBeliefRatingDist
#ggsave(spBeliefRatingDist,filename="../graphs/modelingAssumptions/spBeliefRatingDist.png")


#look across participants
spBeliefRatingDistByParticipant <- spBeliefRatingDist +
  facet_wrap_paginate(~workerid, ncol = 6, nrow = 8, page = 1)

n <- n_pages(spBeliefRatingDistByParticipant)
n

pdf('../graphs/modelingAssumptions/spBeliefByParticipant.pdf',paper ="letter")

for(i in 1:n){
  print(spBeliefRatingDistByParticipant+ facet_wrap_paginate(~workerid, ncol = 6, nrow = 8, page = i))
}

dev.off()
dev.off()


######################################
# 1C. participant belief ratings######
######################################

participantBeliefRatingDist <- ggplot(subset(cd,(item_type=="critical" & orientation!="N" & participant_beliefs!="NA")), aes(x=participant_beliefs)) +
  geom_histogram(bins=50) +
  xlab("Participant belief rating") +
  ylab("Number of ratings") +
  scale_x_continuous(breaks=seq(0,1,by=.1))

participantBeliefRatingDist
#ggsave(participantBeliefRatingDist,filename="../graphs/modelingAssumptions/participantBeliefRatingDist.png")


#look across participants
participantBeliefRatingDistByParticipant <- participantBeliefRatingDist +
  facet_wrap_paginate(~workerid, ncol = 6, nrow = 8, page = 1)

n <- n_pages(participantBeliefRatingDistByParticipant)
n

pdf('../graphs/modelingAssumptions/partBeliefByParticipant.pdf',paper ="letter")

for(i in 1:5){
  print(participantBeliefRatingDistByParticipant+ facet_wrap_paginate(~workerid, ncol = 6, nrow = 8, page = i))
}

dev.off()
dev.off()


#################################
##2. checking model assumptions##
################################


######################
## 2A. intial model####
#####################

###constancy of error variance####
political.1<-lmer(projection~ (socialInfo +  orientation + predicate)^3+ (1|item) + (1|workerid),
                  data=subset(cd,item_type=="critical" & orientation !="N"),REML=F)

#plot fitted vs. standardized residuals for the whole model
plot(political.1, type = c("p", "smooth"))

#fittedVStandardizedResidualsFullModel.png

#interpretation
#fitted value - these are the predicted values from the model.  
#residuals: these are the difference between the predicted value and the actual value. So for example, a point at (.5,0) would mean that there is NO difference between the predicted and fitted value when the fitted value is .5. Any point along the y=0 line means that when the fitted value is x, the diff. between the fitted value and predicted value is y.  Points above y=0 mean that the actual value is above the predicted value. Points below y=0 mean that the actual value is below the predicted value. 

#what we want is for the variance of the residuals to be relatively constant across the fitted values - so we would see basically sort of a horizontal band with points bouncing randomly around. This would be a good sign that the data is linearly distributed. 
# what we actually see is not that!

#It seems that perhaps we should also consider each predictor separately. 
#https://online.stat.psu.edu/stat462/node/146/

###normality of errors#####
#Q-Q plot: shows the match of observed distribution w/ theoretical (here normal) distribution. 

qqnorm(resid(political.1))
qqline(resid(political.1))

#Q-Qplot_fullModel.png

#histogram
hist(resid(political.1))
#residualsHistogram_fullModel.png

###collinearity of predictors###
vif(political.1)
#                       GVIF        Df    GVIF^(1/(2*Df))
#socialInfo             1.260313e+01  1        3.550089
#orientation            1.234922e+01  1        3.514145
#predicate              1.595710e+05 11        1.723844
#socialInfo:orientation 2.909002e+00  1        1.705580
#socialInfo:predicate   1.772256e+04 11        1.559965
#orientation:predicate  7.941220e+03 11        1.504068

#https://stats.stackexchange.com/a/96584

############################
## 2B. stepped down model###
###########################
###constancy of error variance####
political.final <-lmer(projection~ socialInfo +  orientation + predicate+
                         socialInfo:orientation +
                         orientation:predicate +
                         (1|item) + (1|workerid),
                       data=subset(cd,item_type=="critical" & orientation !="N"),REML=F) 

#plot fitted vs. standardized residuals for the whole model
plot(political.final, type = c("p", "smooth"))
#fittedVStandardizedResidualsFullModel.png

#normality of errors
qqnorm(resid(political.final))
qqline(resid(political.final))
#Q-Qplot_finalModel.png


#histogram
hist(resid(political.final))
#residualsHistogram_fullModel


#collinearity of predictors
vif(political.final)
#                              GVIF Df GVIF^(1/(2*Df))
#socialInfo                1.893283  1        1.375966
#orientation              12.348625  1        3.514061
#predicate              1232.119260 11        1.381924
#socialInfo:orientation    2.905710  1        1.704614
#orientation:predicate  7913.943528 11        1.503833

#maybe the high value for orientation is b/c it is collinear w/orientation:predicate?  What happens when I take that out?
political.final.temp <-lmer(projection~ socialInfo +  orientation + predicate+
                         socialInfo:orientation +
                         (1|topic) + (1+socialInfo|workerid),
                       data=subset(cd,item_type=="critical" & orientation !="N"),REML=F) 

vif(political.final.temp)
#                           GVIF Df GVIF^(1/(2*Df))
#socialInfo             1.895406  1        1.376738
#orientation            2.004047  1        1.415644
#predicate              1.008274 11        1.000375
#socialInfo:orientation 2.891180  1        1.700347

