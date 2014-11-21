#########################
## HENRY SCHARF        ##
## UPDATED: 11/18/2014 ##
#########################

## ## takes about 30 seconds to load this dataset
## setwd("~/Google Drive/statistics/bicycle_sharing/citibike_new_york")
## newyork <- read.csv(file = "~/Google Drive/statistics/bicycle_sharing/citibike_new_york/2014-05 - Citi Bike trip data.csv")

## ## some data massaging/arranging
## tripduration.min <- newyork$tripduration/60
## ## newyork <- newyork[newyork$birth.year != "\\N", ]
## newyork$birth.year <- as.numeric(newyork$birth.year)
## ## about 15 seconds
## newyork$starttime <- as.POSIXct(newyork$starttime, tz = "America/New_York")
## startdate <- as.numeric(format(newyork$starttime, "%d"))
## startday <- format(newyork$starttime, "%A")
## startday.no <- format(newyork$starttime, "%w")
## starthour <- as.numeric(format(newyork$starttime, "%H"))
## newyork <- cbind(newyork,
##                  tripduration.min,
##                  startdate,
##                  startday,
##                  startday.no,
##                  starthour)

## save(newyork, file = "newyork_may_2014.RData")

load(file = "~/Google Drive/statistics/bicycle_sharing/citibike_new_york/newyork_may_2014.RData")
ids <- sort(unique(newyork$end.station.id))
date <- 1:31
hour <- 0:23

## i = hour, j = date, k = id
index <- data.frame("id" = rep(ids, rep(24*31, length(ids))),
                    "date" = rep((rep(1:31, rep(24, 31))), length(ids)),
                    "hour" = rep(0:23, 31*length(ids)))

## WARNING! THIS TAKES A LONG TIME!n ~8 hours on 3 cachat processors
library(parallel)
cl <- makeCluster(3)
clusterExport(cl = cl, varlist = "newyork")
arrivals <- parRapply(cl = cl,
                      x = index,
                      FUN = function(x) {
                          dim(newyork[newyork$starthour == x[3] &
                                      newyork$startdate == x[2] &
                                      newyork$end.station.id == x[1], ])[1]
                      })

arrivals <- cbind(index, arrivals)

## station attributes
ids <- unique(newyork$end.station.id)
latitude <- sapply(X = ids,
                   FUN = function(x) {
                       unique(newyork$end.station.latitude
                              [newyork$end.station.id == x])
                   })
longitude <- sapply(X = ids,
                   FUN = function(x) {
                       unique(newyork$end.station.longitude
                              [newyork$end.station.id == x])
                   })
attributes <- data.frame("ids" = rep(ids, 24*31),
                         "latitude" = rep(latitude, 24*31),
                         "longitude" = rep(longitude, 24*31))
attributes <-attributes[order(attributes$ids), ]
arrivals <- cbind(attributes[, 2:3], arrivals)
## weekends
weekend <- arrivals$date %% 7 == 3 | arrivals$date %% 7 == 4
arrivals <- cbind(weekend, arrivals)

save(arrivals, file = "~/Google Drive/statistics/bicycle_sharing/citibike_new_york/arrivals_attributes.RData")
