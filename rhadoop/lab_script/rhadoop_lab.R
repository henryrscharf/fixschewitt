#
# BEFORE BEGINNING LAB
#
# RHadoop must be run remotely, on a computer that has direct access to Hadoop.
# Before running any R code, you must log in to a remote machine and open R
# through the terminal.
#
#  Steps:
#   1) Open a terminal/shell/PuTTY session
#   2) Log in to the RHadoop server: ssh cloudera@192.168.1.105  (password: cloudera)
#   3) Start R once logged in: R


#
# R SCRIPT FOR LAB:
#

#
# Connect R with Hadoop
#

library(rmr2)

Sys.setenv(HADOOP_CMD='/usr/bin/hadoop')
Sys.setenv(HADOOP_STREAMING='/usr/lib/hadoop-0.20-mapreduce/contrib/streaming/hadoop-streaming.jar')


#
# Load and fit training data
#

library(MASS)

# load pilot study data
pilot.path = '/CSP/data/insurance/pilotProgram_results.csv'
pilot = from.dfs(pilot.path, format='csv')
pilot.data = pilot$val

# re-add column names to pilot study data
colnames.path = '/CSP/data/insurance/columnNames.csv'
data.colnames = from.dfs(colnames.path, format='csv')
colnames(pilot.data) = t(data.colnames$val)

# fit and reduce logistic regression model
pilot.fitted = glm(stay ~ ., binomial, pilot.data)
pilot.fitted.reduced = stepAIC(pilot.fitted)

# view model
summary(pilot.fitted.reduced)


#
# Use RHadoop to apply model to all customer records
#

# "bookkeeping" information
predictor.names = t(data.colnames$val)[-1]
predictor.levels = lapply(pilot.data, levels)[-1]
predictors.count = length(predictor.levels)
not.null = function(x) { !is.null(x) }
columns.forFactors = (1:predictors.count)[sapply(predictor.levels, not.null)]


# define map function... function is applied to each customer record
prediction.mapper = function(key, customer){ 
  # re-add column names to customer record
  colnames(customer) = predictor.names
  
  # set values of factor variables to NA if their observed level wasn't present in training data
  for(factorCol in columns.forFactors) {
    unseenLevels = which(!(customer[,factorCol] %in% predictor.levels[[factorCol]]))
    customer[unseenLevels, factorCol] = NA
  }
  
  # predict probability that customer will like new program
  customer.pred = plogis(predict.glm(pilot.fitted.reduced, customer))
  
  # format data for output
  groupingKey =  paste(as.character(customer$region), as.character(customer$gender))
  predictionValue = ifelse(customer.pred>.5, 1, 0)
  
  # send data to reducers for summarization: group by region and gender, extract 1/0 for like/dislike predictions
  keyval(groupingKey, predictionValue)
}

# define reducer... function is applied to all predictions in each region/gender group
counting.reducer = function(groupingKey, predictions) { 
  # estimate proportion of people in group that like the new program
  estimatedProportion = sum(predictions, na.rm=T)/length(predictions)
  
  # output analytic results
  keyval(groupingKey, estimatedProportion) 
}


# run the map and reduce functions on data!
mapred.result = mapreduce(
  input = '/CSP/data/insurance/customer_profiles.csv',
  input.format = 'csv',
  output.format = 'csv',
  map = prediction.mapper,
  reduce = counting.reducer
)

# NOTE: mapred.result() is a function that returns the location of the analytic results

# retrieve analytic results
mapred.result.data = from.dfs(mapred.result(), format='csv')