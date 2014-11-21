library(ggplot2)
library(ggmap)
load("~/Google Drive/statistics/bicycle_sharing/citibike_new_york/newyork_may_2014.RData")


## pick a few stations to deal with and look at number of arrivals by hour
station.ids <- sort(unique(newyork$end.station.id))
weights.ids <- table(newyork$end.station.id)/dim(newyork)[1]
ids <- sort(sample(station.ids, size = 5, prob = weights.ids))
newyork.ids <- newyork[newyork$end.station.id == ids, ]
lat <- unique(newyork.ids$end.station.latitude)
long <- unique(newyork.ids$end.station.longitude)
loc <- data.frame("lat" = lat,
                  "long" = long,
                  "ids" = ids)

## see where these are
dev.new()
nymap <- qmap(location = "new york city", zoom = 12)
nymap +
    geom_point(aes(x = long, y = lat, color = as.character(ids), cex = 2),
               data = loc)

## make arrivals df
arrivals <- NULL
for (id in ids) {
    arrivals.new <- sapply(1:31,
                           FUN = function(x) {
                               out <- NULL
                               for (y in 0:23) {
                                   out <- c(out,
                                            dim(newyork.ids[newyork.ids$startdate == x &
                                                            newyork.ids$starthour == y &
                                                            newyork.ids$end.station.id == id, ])[1])
                               }
                               return(out)
                           })
    arrivals <- c(arrivals, arrivals.new)
}
arrivals.df <- data.frame("arrivals" = as.numeric(arrivals),
                          "id" = sort(rep(ids, 31*24)),
                          "date" = rep(sort(rep(1:31, 24)), 5),
                          "hour" = rep(1:24, 31*5))
weekends <- (arrivals.df$date %% 7 == 3 |
             arrivals.df$date %% 7 == 4)
arrivals.df <- cbind(arrivals.df, weekends)
save(arrivals.df,
     file = "~/Google Drive/statistics/bicycle_sharing/citibike_new_york/arrivals.df.RData")

library(splines)
## weekDAYS by date or by hour - definite bimodal trends
smooth.arrivals.hr <- qplot(x = hour,
                            y = arrivals,
                            data = arrivals.df[arrivals.df$weekends == FALSE, ],
                            color = as.factor(id)) +
    geom_smooth(method = "lm", formula = y ~ ns(x, 7))
smooth.arrivals.date <- qplot(x = date,
                              y = arrivals,
                              data = arrivals.df[arrivals.df$weekends == FALSE, ],
                              color = as.factor(id)) +
    geom_smooth(method = "lm", formula = y ~ 1)

dev.new()
## date
smooth.arrivals.hr + facet_wrap(~date)

## hour
smooth.arrivals.date + facet_wrap(~hour)

## weekENDS - bimodality is mostly gone!
smooth.arrivals.hr <- qplot(x = hour,
                            y = arrivals,
                            data = arrivals.df[arrivals.df$weekends == TRUE, ]) +
    geom_smooth(method = "lm", formula = y ~ ns(x, 7))
smooth.arrivals.date <- qplot(x = date,
                              y = arrivals,
                              data = arrivals.df[arrivals.df$weekends == TRUE, ]) +
    geom_smooth(method = "lm", formula = y ~ 1)
## date
smooth.arrivals.hr + facet_wrap(~date)

## hour
smooth.arrivals.date + facet_wrap(~hour)

## want to look at a heat map of arrivals by station, maybe eventually
## turn this into an animation.
