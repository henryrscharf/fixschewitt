========================================================
title: Using foreach
author: Henry Scharf
css: style_rainbow_brewer_11.css
font-family: 'Lato'
date: November 14, 2014
navigation: slides
incremental: true


THOUGHTS: 
========================================================
id: thoughts
type: section_c

- this example is a lot of reps of teeny calculations. Might see better speed up if the reps were larger things. MCMC?
- need something more compelling for `iterators`
- more complicated example for `.combine`? mean?
- do their own code? fix some code?
- images for heavy analysis?
- model search? random forest? svm? other ML topics?
- references
- dorng in my bootstrap?



Why?
========================================================
type: section

Embarassingly parallel tasks
========================================================
type: sub-section

- computational tasks which involve many, separate, independently executable calculations

- common statistical examples of embarassingly parallel processes: 
    - bootstrapping
    - cross-validation 
    - simulating independent random variables (`dorng`)

- non-parallel processes: 
    - MCMC algorithms 
    - stepwise model selection (e.g.: `step()`)

- For loops that do not explicitly involve dependent calculations are wasteful, especially if we have multiple processors available
- Perhaps even worse, the time cost of using a wasteful approach can put some useful statistical tools beyond our reach!

Options in R
========================================================
type: section_css

- changing from a for loop to one of the `apply()` functions can help, but still doesn't use multiple processors
- `parallel` package (thanks, Miranda!)
- don't use R


Why foreach?
========================================================

We would like to find a way to make use of our whole computer, and make useful tasks like bootstrapping available to use, but without having to invest large amounts of time in learning new programming languages. Enter `foreach`, which keeps the structure of a for loop, but allows us to drop two key assumptions: 

- sequentiality
- single processor architecture

## Goal: transform a traditional for loop into a foreach loop
We will begin with a simple chunk of R code involving a for loop, and transform it. Along the way, we'll take a look at the equivalent computation done with an `apply()` function, and see that using foreach and multiple processors outperforms this too.


Bootstrapping with a for loop
========================================================

We now suppose that we are conducting an analysis wherein we'd like to fit a linear regression, and then obtain estimates of standard errors for each slope parameter through bootstrapping. 

Here's some familiar code:

```r
data(airquality)
dim(airquality)
```

```
[1] 153   6
```

```r
names(airquality)
```

```
[1] "Ozone"   "Solar.R" "Wind"    "Temp"    "Month"   "Day"    
```

```r
N <- 10^1
## make a function to be used in all three instances
boot.func <- function(i) {
    set.seed(1985)
    train.set <- sample(1:153, size = 153, replace = T)
    fit <- lm(Ozone ~ .^2, data = airquality[train.set, ])
    coef.mse <- summary(fit)$coef[, 2]
    return(coef.mse)
}
## 'for' version
coef.mse.for <- NULL
system.time(
    for (i in 1:N) {
        coef.mse.for <- cbind(coef.mse.for, boot.func(i))
    }
    )[3]
```

```
elapsed 
  0.044 
```


# Bootstrapping with an apply function
If you're good with `apply()` functions you might upgrade to

```r
## apply version
system.time(
    coef.mse.sapply <- sapply(X = 1:N, FUN = boot.func)
    )[3]
```

```
elapsed 
  0.028 
```

## Why do we think we can do better?
A fun anecdote. There's a common experience among kids when they first learn to use a handsaw. Perhaps a young girl is attempting to cut a 2 x 4 in half, under the watchful eye of her grandfather. Likley, the pair are constructing a treehouse or some other charming project. As the girl grapples with her grandpa's tool, one made for a full grown adult, she begins to move the teeth of the saw back and forth, and in an effort to maintain control of the cumbersome object moves the saw maybe an inch or two in each direction, making steady but meager progress. After a minute or so of this, the grandpa inevitably interjects: "I bought the _whole_ saw!"

Neither of the first two methods take advantage of multiple processors. While `apply()` functions avoid the inherently sluggish nature of for loops in `R`, they are still ignorant of the processor structure. We want to chop the job into halves, fourths, etc. and use the _whole_ computer!


#  Bootstrapping with a foreach loop
Here is the same computation written with a `foreach()` loop

```r
## foreach version
library(foreach)
library(doRNG)
library(doParallel)
registerDoParallel(cores = 4) ## multi-threading issues with cores = 2
system.time(
    coef.mse.foreach <- foreach(i = 1:N,
                                .inorder = FALSE,
                                .combine = 'cbind') %dopar% boot.func(i)
    )[3]
```

```
elapsed 
  0.062 
```


# Components of a foreach loop
* `%do%` and `%dopar%`
* arguments to consider
    - `.combine` can take on the intuitive values `c`, `cbind`, 
    - `.inorder`
    

# iterators
Sometimes the list or vector that we are iterating over (in the above case, the vector 1:N) can be a very large object. In our case, the vector is quite reasonable in size, but perhaps the object we were iterating over was a multi-dimensional object, with many values, and a high level of precision. In this case, we'd be storing a massive object, which could potentiall fill up our useable memory and slow things down. We could save memory by only keeping the piece of our list we need for a given calculation, and dumping the rest. This is the idea behind the `iterators` package in R.


### Our same example with an iterator instead of a list

First Slide
========================================================

Would you like to see my [thoughts](#/thoughts)?

Want to see what an ~~overstrike~~ looks like?

The color I called <span class="five">five</span>?

Slide With Code
========================================================


```r
summary(cars)
```

```
     speed           dist    
 Min.   : 4.0   Min.   :  2  
 1st Qu.:12.0   1st Qu.: 26  
 Median :15.0   Median : 36  
 Mean   :15.4   Mean   : 43  
 3rd Qu.:19.0   3rd Qu.: 56  
 Max.   :25.0   Max.   :120  
```

Slide With Plot
========================================================

![plot of chunk unnamed-chunk-2](foreach_slides_presentation-figure/unnamed-chunk-2.png) 
