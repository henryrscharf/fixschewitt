#--------------------------------------#
#--------------------------------------#
# Introduction to the parallel package #
#--------------------------------------#
#--------------------------------------#

#-------------------------#
# Setup                   #
#-------------------------#

require(parallel)

# Determine number of CPU cores in your machine
nCores = detectCores()

# Create cluster with desired number of cores
cl = makeCluster(nCores)

# stopCluster(cl) # Always stop your cluster when you are done

  #----------------------------#
  #     ALTERNATELY...         #
  #----------------------------#
  # If you also want to be able to use foreach,
  # it is necessary to register the cluster, for example:

  # require(doParallel)
  # nCores = detectCores()
  # cl = makeCluster(nCores)
  # registerDoParallel(cl)

#---------------------------------------#
# Quick refresher on apply functions    #
#---------------------------------------#
input = 1:3
# ?sapply # to see R help file
sapply(input, sqrt) # output is a vector
lapply(input, sqrt) # output is a list

#---------------------------------------#
# Parallel analogues of apply functions #
#---------------------------------------#

# A simple function to apply to the numbers 1 through 100
# (Calculate the mean of a million random numbers ~ Unif(0,i))
# Note: We'll come back to issues with random number generation later!

input = 1:100

testFun = function(i){
  mu = mean(runif(1e+06, 0, i))
  return(mu)
}

  # How would we do this with foreach?
  # (Note: to run this you will need to register the cluster, see "ALTERNATELY" above)
      # system.time(
      #   mu.foreach <- foreach(i=1:100,
      #                         .combine = "c") %dopar% {
      #                           testFun(i)
      #                         }
      # )

# Using sapply and parSapply
system.time(sapply(input, testFun))

system.time(parSapply(cl, input, testFun))

# Using lapply and parLapply
system.time(lapply(input, testFun))

system.time(parLapply(cl, input, testFun))

# Using mclapply (not available on Windows)
# Note: this can be run without setting up a cluster first
system.time(mclapply(input, testFun, mc.cores=nCores))

#------------------------------#
# Variable scope               #
#------------------------------#

# Keep in mind you need to specify all the variables, packages, etc.
# that the parallel function needs.

# For example, This would return an error message:
  # base = 2
  # parLapply(cl, 1:3, function(x){base^x})

# You can use clusterExport to get around this:
base = 2
clusterExport(cl, "base")
parLapply(cl, 1:3, function(x){base^x})

#------------------------------#
# Random number generation     #
#------------------------------#

# Back to the first example with testFun
# How to properly set up the random number streams

clusterSetRNGStream(cl, iseed=2015)
res1 = parSapply(cl, input, testFun)

clusterSetRNGStream(cl, iseed=2015)
res2 = parSapply(cl, input, testFun)

identical(res1, res2) # Setting the seed enables reproducibility!

#----------------------------------#
# Bootstrapping the bikeshare data #
#----------------------------------#

# Predict number of arrivals at NOHO station at 5pm on weekdays
# Use bootstrapping to get a 95% CI of this prediction
# Note: We need to be careful about random number generation!

# Method 1 - send this function to each of the 'cores'

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
# Note: do.call simply passes the list of arguments to a function call
# (in this case combining the results into one boot object)

set.seed(2015)
system.time(
  bike.boot <- do.call(c, lapply(seq_len(nCores), run1))
)

# Now try it in parallel!
clusterSetRNGStream(cl, iseed=2015)
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
set.seed(2015, kind="L'Ecuyer")

system.time(
  bike.boot3 <- boot(data=arrivals.sub, statistic=bikePred, R=nBoot, 
                     parallel="multicore", ncpus=4)
)

#--------------------------------------------#
# Stop the cluster when you're done using it #
#--------------------------------------------#

stopCluster(cl)





