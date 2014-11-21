library(rgcvpack)
load("~/Google Drive/statistics/bicycle_sharing/citibike_new_york/newyork_may_2014.RData")
load("~/Google Drive/statistics/bicycle_sharing/citibike_new_york/arrivals.RData")

qplot(x = arrivals,
      y = hour,
      data = arrivals,
      color = id) +
    geom_point()

