#
# reference data, including some true coefs for regression function
#

demographics.age = data.frame( age = c('16-19', '20-34', '35-54', '55-64', '65+'),
                               age.prob = c(22, 21+21+19, 20+20+22+22, 19+16, 37),
                               marriage.prob = 100 - c(99, 68, 39, 50, 50 ),
                               homeOwner.prob = c(0, 17, 90, 80, 30)/100,
                               carOwner.prob = c(50, 84, 90, 86, 80)/100,
                               male.prob = c(11, 11+10+9, 10+10+11+10, 9+8, 16),
                               policies.avg = c(1, 1, 2, 2, 1),
                               male.avgmiles = c(8206, 17976, 18858, 15859, 10304),
                               male.ticketrate = c(855, 500, 300, 180, 100),
                               female.avgmiles = c(6873, 12004, 11464, 7780, 4785),
                               female.ticketrate = c(855, 500, 300, 180, 100)/2
)
demographics.age$male.prob = demographics.age$male.prob / demographics.age$age.prob
demographics.age[,c(2:3)] = scale(demographics.age[,c(2:3)], 
                                  center = F, 
                                  scale = colSums(demographics.age[,c(2:3)]))

demographics.gender = data.frame(occupation.type = c('Professional', 'Education', 'Food', 'Healthcare', 'Construction', 'Arts/Design'),
                                 male.prob = c(22+29+34+26+9, 12, 13, 12, 22+23+15+11, 5),
                                 female.prob = c(62+29+20+17+10, 23+6, 16, 29, 5+4, 5))
demographics.gender[,c(2:3)] = scale(demographics.gender[,c(2:3)], 
                                     center = F, 
                                     scale = colSums(demographics.gender[,c(2:3)]))

demographics.regions = data.frame(region.list = c('Northeast', 'Midwest', 'South', 'West'),
                                  region.prob = c(55, 66, 114, 71)
)
demographics.regions$region.prob = demographics.regions$region.prob/sum(demographics.regions$region.prob)

cars = data.frame( type = c('Sedan', 'Sports', 'Truck', 'Van'),
                   prob = c(50, 10, 20, 10),
                   age = c(9.5, 7, 11, 8)
)
cars$prob = cars$prob/sum(cars$prob)


  
rPolicy = function(n) {
  # generate record information for n random policy holders
  
  rep.na = rep(NA, n)
  randomCustomers = data.frame( age = sample(demographics.age$age, n, replace = T, prob = demographics.age$age.prob ),
                                gender = rep.na,
                                married = rep.na,
                                profession.type = rep.na,
                                own.car = rep.na,
                                own.home = rep.na,
                                region = sample(demographics.regions$region.list, n, replace = T, prob = demographics.regions$region.prob),
                                drivers.insured = rep.na,
                                use.business = rbinom(n, 1, .25),
                                miles.annual = rep.na,
                                tickets = rep.na,
                                safety.features = sample(c('Low', 'Med', 'High'), n, replace = T, c(.25, .525, .225)),
                                car.age = rep.na,
                                car.type = sample(cars$type, n, replace = T, cars$prob)
                              )
  randomCustomers$car.age = rpois(n, cars[randomCustomers$car.type,'age'])
  randomCustomers$own.car = rbinom(n, 1, demographics.age[randomCustomers$age, 'carOwner.prob'])
  randomCustomers$own.home = rbinom(n, 1, demographics.age[randomCustomers$age, 'homeOwner.prob'])
  randomCustomers$married = rbinom(n, 1, demographics.age[randomCustomers$age, 'marriage.prob'])
  randomCustomers$gender = ifelse(rbinom(n, 1, demographics.age[randomCustomers$age, 'male.prob']), 'M', 'F')
  randomCustomers[randomCustomers$gender=='F', 'profession.type'] = as.character(
    sample(demographics.gender$occupation.type, 
           sum(randomCustomers$gender=='F'), 
           replace = T,
           prob = demographics.gender$female.prob )   )
  randomCustomers[randomCustomers$gender=='M', 'profession.type'] = as.character(
    sample(demographics.gender$occupation.type, 
           sum(randomCustomers$gender=='M'), 
           replace = T,
           prob = demographics.gender$male.prob )   )
  randomCustomers$drivers.insured = ceiling((rgamma(n, shape=demographics.age[randomCustomers$age, 'policies.avg'],
                                                    rate=1.5)))
  randomCustomers[randomCustomers$gender=='F', 'miles.annual'] = round(rnorm(sum(randomCustomers$gender=='F'),
                                                                       demographics.age[randomCustomers$age, 'female.avgmiles'],
                                                                       sqrt(demographics.age[randomCustomers$age, 'female.avgmiles'])))
  randomCustomers[randomCustomers$gender=='M', 'miles.annual'] = round(rnorm(sum(randomCustomers$gender=='M'),
                                                                             demographics.age[randomCustomers$age, 'male.avgmiles'],
                                                                             sqrt(demographics.age[randomCustomers$age, 'male.avgmiles'])))
  randomCustomers[randomCustomers$gender=='F', 'tickets'] = rpois(sum(randomCustomers$gender=='F'),
                                                                  4/demographics.age[randomCustomers$age, 'female.ticketrate'] )
  randomCustomers[randomCustomers$gender=='M', 'tickets'] = rpois(sum(randomCustomers$gender=='M'),
                                                                  4/demographics.age[randomCustomers$age, 'male.ticketrate'] )
  
  randomCustomers
}



rResponses = function(policies, sigma = .5) {
  # given a data frame of policies, computes the logit model and randomizes responses
  #
  # Parameters:
  #  sigma - variance of noise added to logits
  
  n = dim(policies)[1]
  
  demographics.age.effects = data.frame(age = demographics.age$age, 
                                        age.effect = c(2, 4, 10, 10, 12)/20,
                                        homeOwner.effect = rep(1, 5))
  cars.effects = data.frame(type = cars$type, 
                            type.effect = c(3,1,1.3, 2))
  
  logits = demographics.age.effects[policies$age, 'age.effect'] +
           demographics.age.effects[policies$age, 'homeOwner.effect'] * policies$own.home +
           rnorm(n, sd=sigma)
  
  
  plot(logits)
  plot(plogis(logits))
  
  rbinom(n, 1, plogis(logits))
}

#
# simulate training data and fit model
#

policies.train = rPolicy(100)
policies.train.full = cbind(stay=rResponses(policies.train), policies.train)


# fit full model
policies.train.fit = glm(stay~., binomial, policies.train.full)
summary(policies.train.fit)
# stepwise regression to reduce model
policies.train.fit.reduced = stepAIC(policies.train.fit)
summary(policies.train.fit.reduced)

#
# simulate test data and probability predictions
#

policies.test = rPolicy(1000)

policies.test.preds = plogis(predict.glm(policies.train.fit.reduced, policies.test))


# post analyze data

summary(policies.test.preds)
sum(policies.test.preds>.5)/1000

#
# save data to hdfs
#

library(rmr2)
library(rhdfs)

# establish "connection" to hadoop
Sys.setenv(HADOOP_CMD='/usr/bin/hadoop')
Sys.setenv(HADOOP_STREAMING='/usr/lib/hadoop-0.20-mapreduce/contrib/streaming/hadoop-streaming.jar')

# clear old data
dfs.rmr('/CSP/data/insurance/customer_profiles.csv')
dfs.rmr('/CSP/data/insurance/pilotProgram_results.csv')
dfs.rmr('/CSP/data/insurance/columnNames.csv')


# write new data
policies.train.full.hdfs = to.dfs(policies.train.full, output='/CSP/data/insurance/pilotProgram_results.csv', format='csv')
policies.test.hdfs = to.dfs(policies.test, output='/CSP/data/insurance/customer_profiles.csv', format='csv')
policies.colnames = to.dfs(colnames(policies.train.full), output='/CSP/data/insurance/columnNames.csv', format='csv')