#########################
## HENRY SCHARF        ##
## UPDATED: 12/09/2014 ##
#########################

source("~/Google Drive/statistics/bicycle_sharing/citibike_new_york/EDA/subsetting_arrivals_attributes.R")

library(ggmap)
theme_set(theme_bw(16))
lower.manhat <- qmap(location = c(mean(arrivals.sub$longitude),
                         mean(arrivals.sub$latitude)),
                     ## source = "stamen",
                     ## maptype = "toner",
                     zoom = 13,
                     color = "bw")
lower.manhat +
    geom_point(aes(x = unique(arrivals.sub$longitude),
                   y = unique(arrivals.sub$latitude),
                   color = as.factor(unique(id)),
                   size = 3),
               data = arrivals.sub) +
    guides(size = FALSE,
           color = FALSE)
ggsave(filename = "map_top7.pdf")

## this one is terrific
ggplot(data = arrivals.sub,
       aes(x = hour, y = arrivals,
           color = as.factor(id),
           group = weekend)) +
    geom_point(alpha = 0.3) +
    scale_color_brewer(palette = "Dark2", guide = FALSE) +
        facet_grid(facets = id ~ weekend)
ggsave(filename = "~/git/fixschewitt/foreach/fig/top7_average_day.png",
       width = 8, height = 8)

## ggplot(data = arrivals.sub,
##        aes(x = hour, y = arrivals,
##            color = as.factor(id),
##            group = weekend)) +
##     geom_jitter(alpha = 0.2) +
##     geom_smooth(aes(group = as.factor(id)))
