# title: "Moving from for to foreach"
# author: "Henry Scharf"
# date: "May 14, 2015"

###########################################
## load the data and creat the test sets ##
###########################################

load(url("http://www.stat.colostate.edu/~scharfh/CSP_parallel/data/arrivals_subset.RData"))
K <- 50
N <- dim(arrivals.sub)[1]

## for convenience kill off 8 observations (we have 5208) and make cv test sets
set.seed(1985)
discarded <- sample(1:N, size = 8)
cv.test.sets <- matrix(sample((1:N)[-discarded], size = N - 8), ncol = K)

####################
## for each fold, ## 
## fit the model  ##    
## and get the    ##
## training error ##
####################
library(splines)
lq.loss <- function(y, y.hat, q = 1) {(abs(y - y.hat))^q}
get.errs <- function(test.set = NULL,
                     discarded = NULL,
                     q = 1) {
    sml.glm <- glm(arrivals ~
                       bs(hour, degree = 4)
                   + weekend
                   + as.factor(id),
                   data = arrivals.sub[-c(discarded, test.set), ],
                   family = "poisson")
    med.glm <- glm(arrivals ~
                       bs(hour, degree = 4)*weekend
                   + as.factor(id),
                   data = arrivals.sub[-c(discarded, test.set), ],
                   family = "poisson")
    big.glm <- glm(arrivals ~
                       bs(hour, degree = 4)*weekend*as.factor(id),
                   data = arrivals.sub[-c(discarded, test.set), ],
                   family = "poisson")
    sml.err <- mean(lq.loss(predict(object = sml.glm,
                                    newdata = arrivals.sub[test.set, -7],
                                    type = "response"),
                            arrivals.sub[test.set, 7],
                            q = q))
    med.err <- mean(lq.loss(predict(object = med.glm,
                                    newdata = arrivals.sub[test.set, -7],
                                    type = "response"),
                            arrivals.sub[test.set, 7],
                            q = q))
    big.err <- mean(lq.loss(predict(object = big.glm,
                                    newdata = arrivals.sub[test.set, -7],
                                    type = "response"),
                            arrivals.sub[test.set, 7],
                            q = q))
    return(c(sml.err, med.err, big.err))
}

#################################
## K-fold CV a variety of ways ##
#################################
err.for <- NULL
system.time(
    for (i in 1:K) {
        err.for <- cbind(err.for, get.errs(test.set = cv.test.sets[, i],
                                           discarded = discarded,
                                           q = 1))
    }
)

system.time(
    err.apply <- sapply(X = 1:K, 
                        FUN = function(i) {
                            get.errs(test.set = cv.test.sets[, i],
                                     discarded = discarded,
                                     q = 1)
                        }
    )
)

## the foreach way!
library(foreach)
library(doParallel)
registerDoParallel(cl = 2)
system.time(
    err.foreach <- foreach(i=1:K,
                           .inorder = FALSE,
                           .combine = "cbind",
                           .packages = "splines") %dopar% {
                               get.errs(test.set = cv.test.sets[, i],
                                        discarded = discarded,
                                        q = 1)
                           }
)

## with iterators instead
library(iterators)
registerDoParallel(cl = 2)
system.time(
    err.foreach.iter <- foreach(x = iter(cv.test.sets, by = "col"),
                                .inorder = FALSE,
                                .combine = "cbind",
                                .packages = "splines") %dopar% {
                                    get.errs(test.set = x,
                                             discarded = discarded,
                                             q = 1)
                                }
)

#################################
## foreach with random numbers ##
#################################
library(doRNG)
registerDoParallel(cl = 2)
blah1 <- foreach(x = 1:10, 
                 .options.RNG = 1985,
                 .combine = 'c') %dorng% {
                     rnorm(1)
                 }
## or 
registerDoParallel(cl = 2)
registerDoRNG(seed = 1985)
blah2 <- foreach(x = 1:10,
                 .combine = 'c') %dopar% {
                     rnorm(1)
                 }

############################
## An exercise (solution) ##
############################
sum.arrivals <- function(date = NULL,
                         id = NULL){
    sum.arr <- sum(arrivals.sub$arrivals[arrivals.sub$date == date &
                                             arrivals.sub$id == id])
    return(sum.arr)
}
registerDoParallel(cl = 4)
system.time(
    busiest <- foreach(date = 1:31,
                       .inorder = FALSE,
                       .combine = 'c') %:%
        foreach(id = unique(arrivals.sub$id),
                .inorder = FALSE,
                .combine = '+') %dopar% {
                    sum.arrivals(date = date,
                                 id = id)
                })
which(busiest==max(busiest))    
