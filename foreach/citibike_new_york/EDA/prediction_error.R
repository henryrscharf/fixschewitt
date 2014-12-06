#########################
## HENRY SCHARF        ##
## UPDATED: 12/02/2014 ##
#########################
source("~/git/fixschewitt/foreach/citibike_new_york/EDA/subsetting_arrivals_attributes.R")

lq.loss <- function(y, y.hat, q = 1){(abs(y - y.hat))^q}
get.errs <- function(test.set = NULL,
                     discarded = NULL,
                     q = q) {
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

K <- 50
N <- dim(arrivals.sub)[1]

## kill off 8 observations and make cv test sets
set.seed(1985)
discarded <- sample(1:N, size = 8)
cv.test.sets <- matrix(sample((1:N)[-discarded], size = N - 8), ncol = K)

## 13ish seconds
library(foreach)
library(doParallel)
registerDoParallel(cl = 2)
system.time(
    err.foreach <- foreach(i=1:K,
                           .inorder = FALSE,
                           .combine = "cbind") %dopar% {
                               get.errs(test.set = cv.test.sets[, i],
                                        discarded = discarded,
                                        q = 1)
                           }
    )

err <- data.frame(t(err.foreach))
names(err) <- c("sml", "med", "big")
library(RColorBrewer)
pal <- brewer.pal(n = 3, name = "Dark2")
## png(filename = "~/git/fixschewitt/foreach/fig/errors_sml_med_big_density.png")
plot(density(err$sml), col = pal[1], lwd = 2,
     main = "distribution of absolute errors",
     xlab = "size of error",
     ylab = "density estimate")
lines(density(err$med), col = pal[2], lwd = 2)
lines(density(err$big), col = pal[3], lwd = 2)
legend("topright",
       legend = c("sml", "med", "big"),
       lty = 1, lwd = 2,
       col = pal)
## dev.off()

## png(filename = "~/git/fixschewitt/foreach/fig/errors_boxplots.png")
## boxplot(err)
## dev.off()

## gg version
err.gg <- data.frame("errors" = c(t(err.foreach)),
                     "model" = c(rep("sml", K), rep("med", K), rep("big", K)))
ggplot(data = err.gg, aes(x = errors, color = model, group = model)) +
    geom_line(stat = "density", lwd = 0.8)
ggsave(filename = "~/git/fixschewitt/foreach/fig/error_densities.png",
       width = 10, height = 6)


## mean.err <- apply(err.foreach, 1, mean)
## var.err <- apply(err.foreach, 1, var)
## err <- data.frame(t(err.foreach))
## names(err) <- c("sml", "med", "big")
## xx <- seq(min(mean.err) - 3*max(sqrt(var.err)),
##           max(mean.err) + 3*max(sqrt(var.err)),
##           length.out = 500)

## library(RColorBrewer)
## pal <- brewer.pal(n = 3, name = "Dark2")
## png(filename = "~/git/fixschewitt/foreach/fig/errors_sml_med_big.png")
## plot(xx,
##      dgamma(xx, shape = mean.err[1]^2/var.err[1], rate = mean.err[1]/var.err[1]),
##      type = "l",
##      col = pal[1], lwd = 2,
##      ylim = c(0, 0.75),
##      main = "distribution of absolute errors",
##      xlab = "size of error",
##      ylab = "approximate normal density")
## lines(xx,
##       dgamma(xx, shape = mean.err[2]^2/var.err[2], rate = mean.err[2]/var.err[2]),
##       col = pal[2], lwd = 2)
## lines(xx,
##       dgamma(xx, shape = mean.err[3]^2/var.err[3], rate = mean.err[3]/var.err[3]),
##       col = pal[3], lwd = 2)
## legend("topright",
##        legend = c("sml", "med", "big"),
##        lty = 1, lwd = 2,
##        col = pal)
## dev.off()

## ## investigate what kinds of errors each model makes
## sml.glm <- glm(arrivals ~
##                bs(hour, degree = 4)
##                + weekend
##                + as.factor(id),
##                data = arrivals.sub,
##                family = "poisson")
## med.glm <- glm(arrivals ~
##                bs(hour, degree = 4)*weekend
##                + as.factor(id),
##                data = arrivals.sub,
##                family = "poisson")
## big.glm <- glm(arrivals ~
##                bs(hour, degree = 4)*weekend
##                + bs(hour, degree = 4)*as.factor(id),
##                data = arrivals.sub,
##                family = "poisson")

## ## plot |residuals| against data
## plot(arrivals.sub[, 7],
##      abs(predict(object = sml.glm,
##                  type = "response") -
##          arrivals.sub[, 7]), col = alpha(pal[1], 0.3))
## points(arrivals.sub[, 7],
##        abs(predict(object = med.glm,
##                    type = "response") -
##            arrivals.sub[, 7]), col = alpha(pal[2], 0.3))
## points(arrivals.sub[, 7],
##        abs(predict(object = big.glm,
##                    type = "response") -
##            arrivals.sub[, 7]), col = alpha(pal[3], 0.3))
## legend("bottomright", legend = c("sml", "med", "big"), pch = 1, col = pal)

new.loss <- function(y, y.hat, y.med){
    abs(y - y.hat)*abs(y - y.med)
}
get.new.errs <- function(test.set = NULL,
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
    sml.err <- mean(new.loss(y = arrivals.sub[test.set, 7],
                             y.hat = predict(object = sml.glm,
                                 newdata = arrivals.sub[test.set, -7],
                                 type = "response"),
                             y.med = median(arrivals.sub[test.set, 7])))
    med.err <- mean(new.loss(y = arrivals.sub[test.set, 7],
                             y.hat = predict(object = med.glm,
                                 newdata = arrivals.sub[test.set, -7],
                                 type = "response"),
                             y.med = median(arrivals.sub[test.set, 7])))
    big.err <- mean(new.loss(y = arrivals.sub[test.set, 7],
                             y.hat = predict(object = big.glm,
                                 newdata = arrivals.sub[test.set, -7],
                                 type = "response"),
                             y.med = median(arrivals.sub[test.set, 7])))
    return(c(sml.err, med.err, big.err))
}
registerDoParallel(cl = 4)
system.time(
    new.err.foreach <- foreach(i=1:K,
                               .inorder = FALSE,
                               .combine = "cbind") %dopar% {
                                   get.new.errs(test.set = cv.test.sets[, i],
                                                discarded = discarded)
                               }
    )

new.err <- data.frame(t(new.err.foreach))
names(new.err) <- c("sml", "med", "big")
library(RColorBrewer)
pal <- brewer.pal(n = 3, name = "Dark2")
png(filename = "~/git/fixschewitt/foreach/fig/new_errors_sml_med_big.png")
plot(density(new.err$sml, bw = "SJ"), col = pal[1], lwd = 2,
     main = "distribution of absolute errors",
     xlab = "size of error",
     ylab = "density estimate")
lines(density(new.err$med, bw = "SJ"), col = pal[2], lwd = 2)
lines(density(new.err$big, bw = "SJ"), col = pal[3], lwd = 2)
legend("topright",
       legend = c("sml", "med", "big"),
       lty = 1, lwd = 2,
       col = pal)
dev.off()

png(filename = "~/git/fixschewitt/foreach/fig/new_errors_boxplots.png")
boxplot(new.err)
dev.off()



mean.err <- apply(new.err.foreach, 1, mean)
var.err <- apply(new.err.foreach, 1, var)
err <- data.frame(t(new.err.foreach))
names(err) <- c("sml", "med", "big")
xx <- seq(min(mean.err) - 3*max(sqrt(var.err)),
          max(mean.err) + 3*max(sqrt(var.err)),
          length.out = 500)

library(RColorBrewer)
pal <- brewer.pal(n = 3, name = "Dark2")
png(filename = "~/git/fixschewitt/foreach/fig/new_errors_sml_med_big.png")
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
dev.off()
