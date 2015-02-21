# Moving from for to foreach
Henry Scharf  
February 21, 2015  



# Why?
## Embarassingly parallel tasks {.build}
### <span class = 'eleven'>**parallel**</span> processes: 

- Bootstrapping
- Cross-validation 
- Simulating independent random variables (`dorng`)

### <span class = 'eleven'>**non-parallel**</span> processes:

- MCMC algorithms 
- Several types of model selection (e.g.: `step()` or the LARS algorithm for LASSO)

## What to do {.build}
### Options 
- Changing from a for loop to one of the `apply()` functions can help, but still doesn't use multiple processors.
- Use the `parallel` package (thanks, Miranda!).
- Don't use R.
- Use the `foreach` package! [@Manual]

### Why foreach?
- Make use of our whole computer
- Without having to invest large amounts of time in learning new programming languages
- <span class = 'ten'>**Our goal**</span>: transform a for loop into a foreach loop

# <span class = "ten">Example</span>: data and research question
## citibike nyc
<div class='columns-2'>
**Goal**: predict arrival volume to inform management of bike stations

- 7 busiest locations from May 2014
- response: # of arrivals each hour of every day in the month
- covariates: hour of the day and whether the day is a weekend

<img 
src = "../fig/map_top7.png" 
alt = "locations" 
width = 550>
</div>

## {.centered}
<img src = "../fig/top7_average_day.png" 
alt = "busiest 7 stations"
height = 615>

# Fitting GLMs and extracting prediction error | the Goldilocks approach 
##
<div class = "centered"">
<img src="../fig/fit_all.png"
alt = "fit all"
height = 615>
</div>

## Use K-fold cross validation to compare prediction error.
Make our K-fold test sets (and implicitly, our training sets)

```r
load(url("http://www.stat.colostate.edu/~scharfh/CSP_parallel/data/arrivals_subset.RData"))
K <- 50
N <- dim(arrivals.sub)[1]

## for convenience kill off 8 observations (we have 5208) and make cv test sets
set.seed(1985)
discarded <- sample(1:N, size = 8)
cv.test.sets <- matrix(sample((1:N)[-discarded], size = N - 8), ncol = K)
```


## For each fold, fit the model...

```r
library(splines)
lq.loss <- function(y, y.hat, q = 1) {(abs(y - y.hat))^q}
get.errs <- function(test.set = NULL, discarded = NULL, q = 1) {
    sml.glm <- glm(arrivals ~
                   bs(hour, degree = 4)
                   + weekend
                   + as.factor(id),
                   data = arrivals.sub[-c(discarded, test.set), ],
                   family = "poisson")
    med.glm <- glm(arrivals ~
                   bs(hour, degree = 4)*weekend
                   + as.factor(id),
                   data = arrivals.sub[-c(discarded, test.set), ],
                   family = "poisson")
    big.glm <- glm(arrivals ~
                   bs(hour, degree = 4)*weekend*as.factor(id),
                   data = arrivals.sub[-c(discarded, test.set), ],
                   family = "poisson")
```

## ...and get the training error

```r
    sml.err <- mean(lq.loss(predict(object = sml.glm,
                                    newdata = arrivals.sub[test.set, -7],
                                    type = "response"),
                            arrivals.sub[test.set, 7],
                            q = q))
    med.err <- mean(lq.loss(predict(object = med.glm,
                                    newdata = arrivals.sub[test.set, -7],
                                    type = "response"),
                            arrivals.sub[test.set, 7],
                            q = q))
    big.err <- mean(lq.loss(predict(object = big.glm,
                                    newdata = arrivals.sub[test.set, -7],
                                    type = "response"),
                            arrivals.sub[test.set, 7],
                            q = q))
    return(c(sml.err, med.err, big.err))
}
```

## K-fold CV with a for loop {.build}
Using a naive for loop, we could implement this as:

```r
err.for <- NULL
system.time(
    for (i in 1:K) {
        err.for <- cbind(err.for, get.errs(test.set = cv.test.sets[, i],
                                           discarded = discarded,
                                           q = 1))
        }
    )
```

```
##    user  system elapsed 
##  29.865   1.230  31.407
```

## K-fold CV with an apply function {.build}
If you're good with `apply()` functions you might upgrade to

```r
system.time(
    err.apply <- sapply(X = 1:K, 
                        FUN = function(i) {
                            get.errs(test.set = cv.test.sets[, i],
                                     discarded = discarded,
                                     q = 1)
                            }
                        )
    )
```

```
##    user  system elapsed 
##  28.686   1.134  30.167
```

Both of these assume a single processor architecture. We want to chop the job into halves, fourths, etc. and use the _whole_ computer!

## K-fold CV with a foreach loop {.build}

```r
library(foreach)
library(doParallel)
detectCores()
```


```r
registerDoParallel(cl = 2)
system.time(
    err.foreach <- foreach(i=1:K,
                           .inorder = FALSE,
                           .combine = "cbind") %dopar% {
                               get.errs(test.set = cv.test.sets[, i],
                                        discarded = discarded,
                                        q = 1)
                               }
    )
```

```
##    user  system elapsed 
##  16.791   0.738  18.064
```

# The breakdown
## Components of a foreach loop
- `registerDoParallel(cl = 2)`
- `%___%`
    - `%do%` performs the calculations in order and uses only one processor.
    - `%dopar%` is what we generally wish to use.
    - `%dorng%` will be discussed later, and is required for procedures that use randomly generated numbers.
    - `%:%` can be used for nesting loops, which we'll see in an example at the end of the tutorial.
    
## `foreach()` is a function {.build}
### Arguments

- `.combine` can take on the intuitive values `'c'`, `'cbind'`, or `'+'` as well as more complex functions and tells `foreach` how to combine the outputs from each iteration. The default is to return a list.    
- `.inorder` is a `TRUE`/`FALSE` argument. In general, it is better to change this from the default of `TRUE` to `FALSE` whenever possible.

### Notice that unlike `apply()` functions, the `foreach` takes an expression (in braces after `%dopar%`) instead of a function.

# Results
<!--
have knitr do the results - sucks 

-->

## {.centered}
<img src = "../fig/error_boxplots.png"
alt = "error densities"
height = 615
align = 'middle'>

# Additional topics

## Iterators: smart memory use {.build}
<!--
Sometimes the list or vector that we are iterating over (in the above case, the vector `1:K`) can be a very large object. In our case the vector is quite reasonable in size, but the object we are iterating over could instead be a multi-dimensional object with many values and a high level of precision. In this case, we'd be storing a complete copy of this massive object for each processor, which could potentially fill up our memory and slow things down. We could save memory by only keeping the piece of our list we need for a given calculation as they are computed, and dumping the rest. This is the idea behind the `iterators` package in R.
-->

Here is our same example with an iterator instead of a list. 

```r
library(iterators)
registerDoParallel(cl = 2)
system.time(
    err.foreach.iter <- foreach(x = iter(cv.test.sets, by = "col"),
                               .inorder = FALSE,
                               .combine = "cbind") %dopar% {
                                   get.errs(test.set = x,
                                            discarded = discarded,
                                            q = 1)
                                   }
    )
```

```
##    user  system elapsed 
##  32.779   1.418  16.908
```
<!--
Iterators can also be used to keep from ever having to store even a single copy of the object. For more on these, see [Using the foreach package](http://cran.r-project.org/web/packages/foreach/vignettes/foreach.pdf) and [Using the iterators package](http://cran.r-project.org/web/packages/iterators/vignettes/iterators.pdf).
-->

## Random numbers {.build}

There are many ways to implement `doRNG`, the two easiest of which are:

```r
library(doRNG)
registerDoParallel(cl = 2)
foo <- foreach(x = 1:10, 
               .options.RNG = 1985,
               .combine = 'c') %dorng% {
                   rnorm(1)
                   }
```
and 

```r
registerDoParallel(cl = 2)
registerDoRNG(seed = 1985)
bar <- foreach(x = 1:10,
               .combine = 'c') %dopar% {
                   rnorm(1)
                   }
```
## Random numbers
Note that this gives reproducible results!

```r
sum(foo != bar) 
```

```
## [1] 0
```

## Packages that support foreach
Some packages come with parallel functionality built in via `foreach`.

- `glmnet`
- `gam`
- `ggmcmc` 
- `plyr`
- Many others: see [reverse suggests](http://cran.r-project.org/web/packages/foreach/index.html) on `foreach`'s CRAN page.

# <span class="ten">Exercise</span>

## A summary statistic
The following calculation wouldn't typically require parallelization because it isn't a huge task, however we use it for practice's sake.

Suppose we wish to figure out which day in May had the most combined arrivals across these seven stations. Here's a function to get started, you're welcome to scrap it for your own, or use it and fill in the gaps in what follows.

```r
sum.arrivals <- function(date = NULL,
                         id = NULL){
    sum.arr <- sum(arrivals.sub$arrivals[arrivals.sub$date == date &
                                         arrivals.sub$id == id])
    return(sum.arr)
}
```
## Fix up the following code  {.build}

The following code needs some help. When nesting `foreach` loops ([see this vignette](http://cran.r-project.org/web/packages/foreach/vignettes/nested.pdf)), we need to use `%:%`. Fill in the missing parts to make the code work.


```r
library(doParallel)
registerDoParallel(cl = _)
system.time(
    busiest <- foreach(date = ___,
                       ._______ = FALSE,
                       .combine = ___) %:%
                           foreach(id = unique(arrivals.sub$id),
                                   .inorder = _____,
                                   .combine = ___) %_____% {
                                       sum.arrivals(date = date,
                                                    id = id)
                                       }
    )
which(busiest==max(busiest))    
```

## One answer

```r
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
                                   })
```

```
##    user  system elapsed 
##   0.305   0.104   0.263
```

```r
which(busiest==max(busiest))    
```

```
## [1] 20
```

## Too much overhead

```r
busiest.for <- rep(0, 31)
system.time(
    for(date in 1:31){
        for(id in unique(arrivals.sub$id)){
            busiest.for[date] <- sum(busiest.for[date],
                                     sum.arrivals(date = date, id = id))
        }
    }
    )
```

```
##    user  system elapsed 
##   0.074   0.006   0.080
```

```r
which(busiest.for==max(busiest.for))    
```

```
## [1] 20
```

## Thanks!
<img src = "../fig/parallel_puppies.png" 
alt = "parallel puppies"
width = 1000>

## <span class = "nine">References</span>
### Other tutorials
[Getting Started with doParallel and foreach](http://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf)

[Using the foreach package](http://cran.r-project.org/web/packages/foreach/vignettes/foreach.pdf)

[Using the iterators package](http://cran.r-project.org/web/packages/iterators/vignettes/iterators.pdf)

[Nesting foreach loops](http://cran.r-project.org/web/packages/foreach/vignettes/nested.pdf)

### Data and a neat tool
[citibike system data](https://www.citibikenyc.com/system-data)

[menu meters](http://www.ragingmenace.com/software/menumeters/)
