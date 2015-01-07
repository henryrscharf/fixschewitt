#--------------------------------------#
#--------------------------------------#
# Introduction to the parallel package #
#--------------------------------------#
#--------------------------------------#

#-------------------------#
# Set up parallel backend #
#-------------------------#

require(doParallel) # This will also load other packages we need

# Determine number of CPU cores in your machine
nCores = detectCores()

# Create cluster with desired number of cores
cl = makeCluster(nCores)

# Register cluster
registerDoParallel(cl)

#------------------------------#
# Analogues of apply functions #
#------------------------------#

# A simple function to run 100 times
# (Calculate the mean of a million random numbers ~ Unif(0,i))
# Note: We'll come back to issues with random number generation later!

input = 1:100
input.list = as.list(input)

testFun = function(i){
  mu = mean(runif(1e+06, 0, i))
  return(mu)
}

# How would we do this with foreach?
system.time(
  mu.foreach <- foreach(i=1:100,
                        .combine = "c") %dopar% {
                          testFun(i)
                        }
)

# Using sapply and parSapply
system.time(sapply(input, testFun))

system.time(parSapply(cl, input, testFun))

# Using lapply and parLapply
system.time(lapply(input.list, testFun))

system.time(parLapply(cl, input.list, testFun))

# Using mclapply (not available on Windows)
system.time(mclapply(input.list, testFun, mc.cores=nCores))

#------------------------------#
# Vector map functions         #
#------------------------------#

# If you have a function that takes a vector as its input,
# you can parallelize that with `pvec`, which splits
# the vector into pieces that get sent to the different processors
# Note: pvec assumes that c(FUN(x[1]),FUN(x[2])) = FUN(x[1:2])

# Example: evaluate Matern covariance function for 5 million distances
require(fields)
d = runif(5e+06, 0.1, 10)
system.time(Matern(d))

system.time(pvec(d, Matern, mc.cores=nCores))

#------------------------------#
# Random number generation     #
#------------------------------#

# Back to the first example with testFun
clusterSetRNGStream(cl, iseed=0)
res1 = parSapply(cl, input, testFun)

clusterSetRNGStream(cl, iseed=0)
res2 = parSapply(cl, input, testFun)

identical(res1, res2)

#----------------------------------#
# Bootstrapping the bikeshare data #
#----------------------------------#

# Predict number of arrivals at NOHO station at 5pm on weekdays
# Use bootstrapping to get a 95% CI of this prediction
# Note: We need to be careful about random number generation!

# Method 1 - send this function to each of the processors

run1 = function(...){
  require(boot); require(splines)
  load(url("http://www.stat.colostate.edu/~scharfh/CSP_parallel/data/arrivals_subset.RData"))
  bikePred = function(data, indices){
    d = data[indices,]
    big.glm <- glm(arrivals ~
                     bs(hour, degree = 4)*weekend
                   + bs(hour, degree = 4)*as.factor(id),
                   data = d, family = "poisson")
    mynewdat = data.frame(weekend=FALSE, id=293, hour=17)
    return(predict(object=big.glm, newdata=mynewdat, type="response"))
  }
  boot(data=arrivals.sub, statistic=bikePred, R=250)
}

# First try without parallelizing
system.time(
  bike.boot <- do.call(c, lapply(seq_len(nCores), run1))
)

# Now try it in parallel!
clusterSetRNGStream(cl, iseed=123)
system.time(
  bike.boot2 <- do.call(c, parLapply(cl, seq_len(nCores), run1))
)

# Results
hist(bike.boot2$t)
boot.ci(bike.boot2, type="perc")

# Method 2 - use the built-in parallel support from the boot package

require(boot); require(splines)
load(url("http://www.stat.colostate.edu/~scharfh/CSP_parallel/data/arrivals_subset.RData"))
mynewdat = data.frame(weekend=FALSE, id=293, hour=17)

bikePred = function(data, indices){
  d = data[indices,]
  big.glm <- glm(arrivals ~
                   bs(hour, degree = 4)*weekend + bs(hour, degree = 4)*as.factor(id),
                 data = d, family = "poisson")
  return(predict(object=big.glm, newdata=mynewdat, type="response"))
}

nBoot = nCores*250
set.seed(123, kind="L'Ecuyer")

system.time(
  bike.boot3 <- boot(data=arrivals.sub, statistic=bikePred, R=nBoot, 
                     parallel="multicore", ncpus=4)
)

#--------------------------------------------#
# Stop the cluster when you're done using it #
#--------------------------------------------#

stopCluster(cl)





