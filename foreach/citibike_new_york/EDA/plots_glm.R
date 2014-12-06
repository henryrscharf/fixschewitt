#########################
## HENRY SCHARF        ##
## UPDATED: 11/20/2014 ##
#########################

source(file = "~/git/fixschewitt/foreach/citibike_new_york/EDA/glm_bikes.R")
N <- 5208
## Fitted values (sml)
ggplot(data = arrivals.sub,
       aes(x = hour,
           y = fitted(sml.glm),
           color = as.factor(id))) +
    geom_line(aes(lty = weekend)) +
    facet_grid(facets = id ~ .) +
    scale_color_brewer(palette = "Dark2")
ggsave(filename = "~/git/fixschewitt/foreach/fig/predictions_sml.png")

dev.new()
## fitted values (med)
ggplot(data = arrivals.sub,
       aes(x = hour,
           y = fitted(med.glm),
           color = as.factor(id))) +
    geom_line(aes(lty = weekend)) +
    facet_grid(facets = id ~ .) +
    scale_color_brewer(palette = "Dark2")
ggsave(filename = "~/git/fixschewitt/foreach/fig/predictions_med.png")

dev.new()
## fitted values (big)
ggplot(data = arrivals.sub,
       aes(x = hour,
           y = fitted(big.glm),
           color = as.factor(id))) +
    geom_line(aes(lty = weekend)) +
    facet_grid(facets = id ~ .) +
    scale_color_brewer(palette = "Dark2")
ggsave(filename = "~/git/fixschewitt/foreach/fig/predictions_big.png")

## all fitted together

fitted.all <- c(fitted(sml.glm), fitted(med.glm), fitted(big.glm))
model <- c(rep("small", N), rep("medium", N), rep("big", N))
model <- factor(model, levels = c("small", "medium", "big"))
fit.gg <- data.frame("fit" = fitted.all,
                     model)
fit.gg <- cbind(arrivals.sub, fit.gg)
rownames(fit.gg) <- NULL

ggplot(fit.gg,
       aes(x = hour,
           y = fit,
           color = as.factor(id))) +
    geom_point(aes(y = arrivals),
               alpha = 0.1) +
    geom_line(aes(lty = weekend),
              color = "black") +
    facet_grid(facets = id ~ model) +
    scale_color_brewer(palette = "Dark2",
                       guide = FALSE)
ggsave(filename = "~/git/fixschewitt/foreach/fig/fit_all.png",
       width = 10, height = 6)
