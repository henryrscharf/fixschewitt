data(airquality)
N <- 10^4
## make a function to be used in all three instances
boot.func <- function(i) {
    set.seed(1985)
    train.set <- sample(1:153, size = 153, replace = T)
    fit <- lm(Ozone ~ .^2, data = airquality[train.set, ])
    coef.mse <- summary(fit)$coef[, 2]
    return(coef.mse)
}

## for version ~ 41 seconds
coef.mse.for <- NULL
system.time(
    for (i in 1:N) {
        coef.mse.for <- cbind(coef.mse.for, boot.func(i))
    }
    )
## mean.coef.mse.for <- apply(coef.mse.for, 1, mean)

## apply version ~ 30 seconds
system.time(
    coef.mse.sapply <- sapply(X = 1:N, FUN = boot.func)
    )
## mean.coef.mse.sapply <- apply(coef.mse.sapply, 1, mean)

## foreach version ~ 18 seconds
library(foreach)
library(doParallel)
library(doRNG)
registerDoParallel(cores = 4) ## multi-threading issues with cores = 2
system.time(
    coef.mse.foreach <- foreach(i = 1:N,
                                .inorder = FALSE,
                                .combine = 'cbind',
                                .options.RNG = 1985) %dopar% boot.func(i)
    )

## mean.coef.mse.foreach <- apply(coef.mse.foreach, 1, mean)

## ## iterators
## registerDoParallel(cores = 4) ## multi-threading issues with cores = 2
## system.time(
##     coef.mse.foreach <- foreach(i = icount(N),
##                                 .inorder = FALSE,
##                                 .combine = 'cbind') %dopar% boot.func(i)
##     )

## ## they really all do the same thing
## sum(mean.coef.mse.for == mean.coef.mse.sapply)
## sum(mean.coef.mse.for == mean.coef.mse.foreach)

## ## this is a terrible motivation here...
## ## error calculations without bootstrap
## full.fit <- lm(Ozone ~ .^2, data = airquality)
## full.fit.coef.mse <- summary(full.fit)$coef[, 2]
## ## plot
## par(mfrow = c(4, 4))
## for (i in 1:16) {
##     hist(mse[i, ], breaks = 30, main = rownames(mse)[i])
##     abline(v = full.fit.coef.mse[i], col = "red")
## }
