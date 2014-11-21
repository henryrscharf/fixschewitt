#########################
## HENRY SCHARF        ##
## UPDATED: 11/20/2014 ##
#########################

source("~/Google Drive/statistics/bicycle_sharing/citibike_new_york/EDA/subsetting_arrivals_attributes.R")


library(lme4)

system.time(
    bike.glmm <- glmer(arrivals ~
                       bs(hour, degree = 4)*weekend
                       + (1|id),
                       data = arrivals.sub,
                       family = "poisson")
    )
summary(bike.glmm)

## ~ 3-5 minutes
system.time(
    bike.big.glmm <- glmer(arrivals ~
                           bs(hour, degree = 4)*weekend
                           + (1|id)
                           + (bs(hour, degree = 4)|id),
                           data = arrivals.sub,
                           family = "poisson")
    )
summary(bike.big.glmm)



