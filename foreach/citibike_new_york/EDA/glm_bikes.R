#########################
## HENRY SCHARF        ##
## UPDATED: 11/20/2014 ##
#########################
source("~/git/fixschewitt/foreach/citibike_new_york/EDA/subsetting_arrivals_attributes.R")

sml.glm <- glm(arrivals ~
               bs(hour, degree = 4)
               + weekend
               + as.factor(id),
               data = arrivals.sub,
               family = "poisson")
med.glm <- glm(arrivals ~
               bs(hour, degree = 4)*weekend
               + as.factor(id),
               data = arrivals.sub,
               family = "poisson")
big.glm <- glm(arrivals ~
               bs(hour, degree = 4)*weekend*as.factor(id),
               data = arrivals.sub,
               family = "poisson")
