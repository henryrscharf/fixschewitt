# parallel
<!--
% CSP course: using the parallel package
% Miranda Fix
% 12/11/14
-->





# Why parallel?
We've already seen the `doParallel` package, which acts as an interface between `foreach` and `parallel`

The `parallel` package is essentially a merger of `multicore` and `snow`

- Provides drop-in replacements for most of the functionality of those packages, with integrated handling of random-number generation
- By default, `doParallel` uses `multicore` functionality on Unix-like systems and `snow` functionality on Windows (you can also use the `snow` functionality to execute on a cluster of computers for both systems)

Whereas `foreach` was a parallel analogue of a `for` loop, the `parallel` package provides handy analogues of `apply` functions

# Getting started

## Parallel backend
To get started, we must first make the desired number of cores available to R by registering a parallel backend. We can do this with the `doParallel` package.


```r
library(doParallel)

# Determine number of CPU cores in your machine
nCores = detectCores()

# Create cluster with desired number of cores
cl = makeCluster(nCores)

# Register cluster
registerDoParallel(cl)
```

## Parallel apply
We've seen how we can move from a traditional `for` loop to `foreach`. Similarly, we can speed up traditional `apply` functions by using their parallel analogues. Let's start with a simple example (for now ignoring potential issues with random number generation).


```r
N = 100
input = seq_len(N) # same as 1:N but more robust
input.list = as.list(input)

testFun = function(i){
  mu = mean(runif(1e+06, 0, i))
  return(mu)
}

system.time(sapply(input, testFun))
```

```
##    user  system elapsed 
##   5.126   0.108   5.244
```

```r
system.time(parSapply(cl, input, testFun))
```

```
##    user  system elapsed 
##   0.004   0.000   2.289
```

```r
system.time(lapply(input.list, testFun))
```

```
##    user  system elapsed 
##   4.935   0.110   5.071
```

```r
system.time(parLapply(cl, input.list, testFun))
```

```
##    user  system elapsed 
##   0.002   0.000   2.246
```

```r
system.time(mclapply(input.list, testFun, mc.cores=nCores)) # not available on Windows
```

```
##    user  system elapsed 
##   5.326   0.368   2.626
```

## pvec
We can also parallelize a vector map function using `pvec` (not available on Windows). This splits the vector and submits each part to one core, so it only works for functions such that `c(FUN(x[1]), FUN(x[2]))` is equivalent to `FUN(x[1:2])`. It is often only worthwhile on very long vectors and for computationally intensive calculations, such as this example of evaluating the Matern covariance function:


```r
library(fields)

d = runif(5e+06, 0.1, 10)

system.time(Matern(d))
```

```
##    user  system elapsed 
##   3.418   0.077   3.559
```

```r
system.time(pvec(d, Matern, mc.cores=nCores))
```

```
##    user  system elapsed 
##   2.951   0.296   2.083
```

# Random number generation
Once again, we need to be careful when parallelizing a process which generates (psuedo-)random numbers. We want the different processes to run independent (and preferably reproducible) random-number streams. We can do this using `clusterSetRNGStream`. Let's go back and fix our first example:


```r
N = 100
input = seq_len(N)
testFun = function(i){
  mu = mean(runif(1e+06, 0, i))
  return(mu)
}

# RNGkind("L-Ecuyer-CMRG") # Automatically uses L'Ecuyer's algorithm

clusterSetRNGStream(cl, iseed=0)
res1 = parSapply(cl, input, testFun)

clusterSetRNGStream(cl, iseed=0)
res2 = parSapply(cl, input, testFun)

identical(res1, res2)
```

```
## [1] TRUE
```

# Bootstrapping

Bootstrapping is another example of an embarassingly parallel task. In the example below, we use bootstrapping to generate a 95% confidence interval for R-squared of the linear regression of miles per gallon on car weight and displacement. Using `parLapply` we can split the n*1000 bootstrap replicates between the n cores.


```r
library(boot)
```

```
## 
## Attaching package: 'boot'
## 
## The following object is masked from 'package:survival':
## 
##     aml
## 
## The following object is masked from 'package:lattice':
## 
##     melanoma
```

```r
run1 = function(...){
  library(boot)
  rsq = function(formula, data, indices){
    d = data[indices,]
    fit = lm(formula, data=d)
    return(summary(fit)$r.square)
  }
  boot(data=mtcars, statistic=rsq, R=1000, formula=mpg~wt+disp)
}

system.time(car.boot <- do.call(c, lapply(seq_len(nCores), run1)))
```

```
##    user  system elapsed 
##   7.947   0.059   8.105
```

```r
clusterSetRNGStream(cl, iseed=123)
system.time(car.boot2 <- do.call(c, parLapply(cl, seq_len(nCores), run1)))
```

```
##    user  system elapsed 
##   0.004   0.001   3.405
```

```r
boot.ci(car.boot2, type="bca")
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 4000 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = car.boot2, type = "bca")
## 
## Intervals : 
## Level       BCa          
## 95%   ( 0.6510,  0.8564 )  
## Calculations and Intervals on Original Scale
```

Note that the `boot` package has built-in parallel support, so we can also simply run the following:


```r
rsq = function(formula, data, indices){
  d = data[indices,]
  fit = lm(formula, data=d)
  return(summary(fit)$r.square)
}

nBoot = nCores*1000
set.seed(123, kind="L'Ecuyer")
system.time(
  car.boot3 <- boot(data=mtcars, statistic=rsq, R=nBoot, formula=mpg~wt+disp, 
                    parallel="multicore", ncpus=4)
)
```

```
##    user  system elapsed 
##  13.340   0.512   4.379
```

```r
boot.ci(car.boot3, type="bca")
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 4000 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = car.boot3, type = "bca")
## 
## Intervals : 
## Level       BCa          
## 95%   ( 0.6389,  0.8527 )  
## Calculations and Intervals on Original Scale
```

# Stop cluster
It's a good idea to stop your cluster when you are done using it.


```r
stopCluster(cl)
```

