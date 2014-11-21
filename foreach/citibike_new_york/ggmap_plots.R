library(ggmap)
newyork <- read.csv(file = "~/Google Drive/statistics/bicycle_sharing/citibike_new_york/2014-05 - Citi Bike trip data.csv")
tripduration.min <- newyork$tripduration/60
newyork <- cbind(newyork, tripduration.min)
newyork <- newyork[newyork$birth.year != "\\N", ]
newyork$birth.year <- as.numeric(newyork$birth.year)
dim(newyork)
N <- 200
subsamp <- sample(0:nrow(newyork), size = N)
plot(newyork[subsamp, c(16, 14)], xlim = c(0, 60))

hist(newyork$birth.year)

qmap(location = 'tucson',
     zoom = 12,
     source = '')
