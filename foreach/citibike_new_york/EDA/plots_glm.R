#########################
## HENRY SCHARF        ##
## UPDATED: 11/20/2014 ##
#########################

source(file = "~/Google Drive/statistics/bicycle_sharing/citibike_new_york/EDA/glm_bikes.R")

## ## residuals (sml)
## ggplot(data = arrivals.sub,
##        aes(x = hour,
##            y = abs(fitted(sml.glm) - arrivals))) +
##     geom_point(alpha = 0.1) +
##     geom_smooth(aes(color = weekend))

## dev.new()
## ## residuals (big)
## ggplot(data = arrivals.sub,
##        aes(x = hour,
##            y = abs(fitted(big.glm) - arrivals))) +
##     geom_point(alpha = 0.1) +
##     geom_smooth(aes(color = weekend))

## fitted values (sml)
ggplot(data = arrivals.sub,
       aes(x = hour,
           y = fitted(sml.glm),
           color = as.factor(id))) +
    geom_line(aes(lty = weekend)) +
    facet_grid(facets = id ~ .) +
    scale_color_brewer(palette = "Dark2")
ggsave(filename = "predictions_sml.pdf")

dev.new()
## fitted values (med)
ggplot(data = arrivals.sub,
       aes(x = hour,
           y = fitted(med.glm),
           color = as.factor(id))) +
    geom_line(aes(lty = weekend)) +
    facet_grid(facets = id ~ .) +
    scale_color_brewer(palette = "Dark2")
ggsave(filename = "predictions_med.pdf")

dev.new()
## fitted values (big)
ggplot(data = arrivals.sub,
       aes(x = hour,
           y = fitted(big.glm),
           color = as.factor(id))) +
    geom_line(aes(lty = weekend)) +
    facet_grid(facets = id ~ .) +
    scale_color_brewer(palette = "Dark2")
ggsave(filename = "predictions_big.pdf")
