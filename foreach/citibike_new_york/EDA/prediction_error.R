#########################
## HENRY SCHARF        ##
## UPDATED: 11/20/2014 ##
#########################
source("~/Google Drive/statistics/bicycle_sharing/citibike_new_york/EDA/subsetting_arrivals_attributes.R")

get.errs <- function(test.set = NULL,
                     discarded = NULL) {
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
                   bs(hour, degree = 4)*weekend
                   + bs(hour, degree = 4)*as.factor(id),
                   data = arrivals.sub[-c(discarded, test.set), ],
                   family = "poisson")
    sml.err <- mean((predict(object = sml.glm,
                             newdata = arrivals.sub[test.set, -7],
                             type = "response") -
                     arrivals[test.set, 7])^2)
    med.err <- mean((predict(object = med.glm,
                             newdata = arrivals.sub[test.set, -7],
                             type = "response") -
                     arrivals[test.set, 7])^2)
    big.err <- mean((predict(object = big.glm,
                             newdata = arrivals.sub[test.set, -7],
                             type = "response") -
                     arrivals[test.set, 7])^2)
    return(sqrt(c(sml.err, med.err, big.err)))
}

K <- 50
N <- dim(arrivals.sub)[1]

## kill off 8 observations and make cv test sets
set.seed(1985)
discarded <- sample(1:N, size = 8)
cv.test.sets <- matrix(sample((1:N)[-discarded], size = N - 8), ncol = K)

## 13ish seconds
library(foreach)
registerDoParallel(cl = 4)
system.time(
    err <- foreach(i=1:K,
                   .inorder = FALSE,
                   .combine = "cbind") %dopar%
           get.errs(test.set = cv.test.sets[, i],
                    discarded = discarded)
    )

mean.err <- apply(err, 1, mean)
var.err <- apply(err, 1, var)
err <- data.frame(t(err))
names(err) <- c("sml", "med", "big")
xx <- seq(min(mean.err) - 3*max(sqrt(var.err)),
          max(mean.err) + 3*max(sqrt(var.err)),
          length.out = 500)

library(RColorBrewer)
pal <- brewer.pal(n = 3, name = "Dark2")
plot(xx,
     dgamma(xx, shape = mean.err[1]^2/var.err[1], rate = mean.err[1]/var.err[1]),
     type = "l",
     col = pal[1], lwd = 2,
     ylim = c(0, 0.75),
     main = "distribution of absolute errors",
     xlab = "size of error",
     ylab = "approximate normal density")
lines(xx,
      dgamma(xx, shape = mean.err[2]^2/var.err[2], rate = mean.err[2]/var.err[2]),
      col = pal[2], lwd = 2)
lines(xx,
      dgamma(xx, shape = mean.err[3]^2/var.err[3], rate = mean.err[3]/var.err[3]),
      col = pal[3], lwd = 2)
legend("topright",
       legend = c("sml", "med", "big"),
       lty = 1, lwd = 2,
       col = pal)
dev.copy2pdf(file = "errors_sml_med_big.pdf")

## ## gg version sucks
## err.gg <- data.frame("err" = c(err$sml, err$med, err$big),
##                      "model" = c(rep("sml", K), rep("med", K), rep("big", K)))
## ggplot(data = err.gg,
##        aes(x = err)) +
##     geom_histogram() +
##     facet_grid(facets = model ~ .)
