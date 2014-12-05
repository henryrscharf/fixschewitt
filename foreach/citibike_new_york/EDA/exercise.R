load(url("http://www.stat.colostate.edu/~scharfh/CSP_parallel/data/arrivals_subset.RData"))

sum.arrivals <- function(date = NULL,
                         id = NULL){
    sum.arr <- sum(arrivals.sub$arrivals[arrivals.sub$date == date &
                                         arrivals.sub$id == id])
    return(sum.arr)
}

registerDoParallel(cl = 4)
system.time(
    busiest <- foreach(date = 1:31,
                       .inorder = FALSE,
                       .combine = 'c') %:%
                           foreach(id = unique(arrivals.sub$id),
                                   .inorder = FALSE,
                                   .combine = '+') %dopar% {
                                       sum.arrivals(date = date,
                                                    id = id)
                                   }
    )

busiest.for <- rep(0, 31)
system.time(
    for(date in 1:31){
        for(id in unique(arrivals.sub$id)){
            busiest.for[date] <- sum(busiest.for[date],
                                     sum.arrivals(date = date, id = id))
        }
    }
    )
