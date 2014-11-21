library(ggmap)
## takes about 30 seconds to load this dataset
newyork <- load("newyork_may_2014.RData")

dim(newyork)
## subsetting for ease of use
N <- 2000
subsamp <- sort(sample(0:nrow(newyork), size = N))
ny <- newyork[subsamp, ]

###############################
## EDA for bike sharing data ##
###############################

## how old are folks
hist(newyork$birth.year)


plot(newyork$starttime[newyork$startday ],
     newyork$tripduration.min[newyork$startdate < 8])

hist(newyork$tripduration[startday==1 & starthour==17], breaks = 30)

plot(newyork$starttime[startday==1 & starthour==17],
     newyork$tripduration[startday==1 & starthour==17])

## probably unsurprisingly, distribution of trip duration seems mostly constant
## in time for weekdays
qplot(tripduration.min,
      ..density..,
      data = newyork[newyork$startday == "Thursday", ],
      geom = "histogram",
      binwidth = 5,
      xlim = c(0, 180)) +
    facet_wrap(~ starthour)
## and weekends. This may be an enforced effect from the nature of the bike
## share system.
qplot(tripduration.min,
      ..density..,
      data = newyork[newyork$startday == "Saturday", ],
      geom = "histogram",
      binwidth = 5,
      xlim = c(0, 180)) +
    facet_wrap(~ starthour)

## want to look at average vectors for given times, locations
northing <- newyork$end.station.latitude - newyork$start.station.latitude
easting <- newyork$end.station.longitude - newyork$start.station.longitude
newyork <- cbind(newyork, northing, easting)

northing.plot <- qplot(starthour,
                       northing,
                       data = newyork[newyork$startday == "Thursday", ])
northing.plot + geom_smooth() + facet_wrap( ~ startdate)

easting.plot <- qplot(starthour,
                      easting,
                      data = newyork[newyork$startday == "Thursday", ])
easting.plot + geom_smooth() + facet_wrap( ~ startdate)

qplot(newyork$starthour[newyork$startday == "Saturday"],
      northing[newyork$startday == "Saturday"],
      geom = c("point", "smooth"))
qplot(newyork$starthour[newyork$startday == "Saturday"],
      easting[newyork$startday == "Saturday"],
      geom = c("point", "smooth"))

## individual bike paths
bikeid.range <- range(newyork$bikeid)
id <- sample(bikeid.range[1]:bikeid.range[2], size = 1)
newyork.id <- newyork[newyork$bikeid == id, ]
rides <- dim(newyork.id)[1]

library(RolorBrewer)
library(ggmap)
plot(range(c(newyork.id$start.station.longitude, newyork.id$end.station.longitude)),
     range(c(newyork.id$start.station.latitude, newyork.id$end.station.latitude)),
     type = "n",
     xlab = "east",
     ylab = "north")

qmap("manhattan", zoom = 11, source = "google") +
    geom_segment(data = newyork.id,
                 aes(x = start.station.longitude,
                     y = start.station.latitude,
                     xend = end.station.longitude,
                     yend = end.station.latitude,
                     color = startdate)
                 )
