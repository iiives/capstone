##
##---------------------------------
### Prepare Workspace and libraries 
##---------------------------------


#rm(list=ls())
#setwd("Foundations/capstone")

# data wrangling functions
library(tidyr)
library(dplyr)
library(stringr)

# data exploration and analysis functions
library(ggplot2)
library(GGally)
library(car)
require(RColorBrewer)

# randomnForest modelling functions
library(randomForest)

# model evaluation functions
library(caTools)
library(ROCR)

# cross validation functions
library(caret)
library(e1071)


### Load data 
##-----------
## There are 6 datasets 
## train, logfeat, resourcetype, eventtype, sevtype, test

train <-
  read.csv("dataSets/train.csv", stringsAsFactors = FALSE )

log_feature <-
  read.csv2("dataSets/log_feature.csv", stringsAsFactors = FALSE)

resource_type <-
  read.csv("dataSets/resource_type.csv", stringsAsFactors = FALSE)

event_type <-
  read.csv("dataSets/event_type.csv", stringsAsFactors = FALSE)

sev_type <-
  read.csv("dataSets/severity_type.csv", stringsAsFactors = FALSE )

test <-
  read.csv("dataSets/test.csv", stringsAsFactors = FALSE )



##
## ---------------------------------------------------
### Data cleaning - data tidy and data type correction
## ---------------------------------------------------
## All datasets but log_feature had tidy data.
## Then for each table, missing values are checked,
## string values are trimmed to named numbers and converted as integer.


# Tidy log_feature by spreading data

str(log_feature)

log_feature <- 
  log_feature %>% 
  separate(id.log_feature.volume , c("id", "log_feature", "volume"), sep = ",")


# log_feature data cleaning

summary(log_feature) # Check for missing values
log_feature$log_feature <- sub("feature ", "", log_feature$log_feature)
str(log_feature)
log_feature$id <- as.integer(log_feature$id)
log_feature$volume <- as.integer(log_feature$volume)
log_feature$log_feature <- as.integer(log_feature$log_feature)
str(log_feature)

# event_type data cleaning

summary(event_type) # Check for missing values
str(event_type)
event_type$event_type <- sub("event_type ", "", event_type$event_type)
event_type$event_type <- as.integer(event_type$event_type)
str(event_type)

# resource_type data cleaning

summary(resource_type) # Check for missing values
str(resource_type)
resource_type$resource_type <- sub("resource_type ","", resource_type$resource_type)
resource_type$resource_type <- as.integer(resource_type$resource_type)
str(resource_type)

# sev_type data cleaning

summary(sev_type) # Check for missing values
str(sev_type)
sev_type$severity_type <- sub("severity_type ", "", sev_type$severity_type)
sev_type$severity_type <- as.integer(sev_type$severity_type)
str(sev_type)

# train data cleaning

summary(train) # Check for missing values
str(train)
train$location <- sub("location ","",train$location)
train$location <- as.integer(train$location)
train$fault_severity <- as.integer(train$fault_severity)
str(train)

# test data cleaning

summary(test) # Check for missing values
str(test)
test$location <- sub("location ","",test$location)
test$location <- as.factor(test$location)
str(test)

## Order tables to prepare Joining

arrange(train, id)
arrange(log_feature, id)
arrange(resource_type, id)
arrange(event_type, id)
arrange(sev_type, id)
arrange(test, id)


## Full join 
# Evaluate where and which dataset is short of matching entries

networki <- full_join(train, log_feature, by = c("id"="id"))
networki <- full_join(networki, resource_type, by = c("id"="id"))
networki <- full_join(networki, event_type, by = c("id"="id"))
networki <- full_join(networki, sev_type, by = c("id"="id"))

head(networki)
summary(networki) 
# There are some logs with only location details missing
# Remove rows with NULL value
network <- na.omit(networki)
summary(network)

str(network)
# Create new table all variables but location
incident_log <- full_join(log_feature, resource_type, by = c("id"="id"))
incident_log <- full_join(incident_log, event_type, by = c("id"="id"))
incident_log <- full_join(incident_log, sev_type, by = c("id"="id"))
summary(incident_log)

# create data info table
variable <- colnames(network)
definition <- c("Record id",
                "Report location",
                "Level of impact to users",
                "Assumption: Network faulting feature",
                "Assumption: Unit measure of Network faulting feature",
                "Network servicing equipment type",
                "Assumption: Network feature’s fault behaviour",
                "severity type according to warning message received from monitoring machines" )

data_info <- as.matrix(cbind(variable, definition))
data_info


##
##-------------------------
### Preliminary exploration
##-------------------------

networkG <- network
str(networkG)

## Parallel plot Matrix - id base GGally::ggpairs(network)
ggpairs(data=networkG, # data.frame with variables
        title="Scatterplot Matrix of Network fault data", # title of the plot
        colour = "fault_severity")

###Histograms

# Location
ggplot(train, aes(
  x = id, y = location, col = as.factor(fault_severity), size = as.factor(fault_severity) )) + 
  geom_point(alpha = 0.3)
#   Comment: There are some locations that have continuous faults reported

# Log_feature
ggplot(log_feature, aes(x = id, y = log_feature, size = volume)) + geom_point(alpha = 0.05)
#   Comment: This confirms the train dataset that some log features have continous feature logs,
# the right is where volume is above 10. This pattern may correlate to machine type found at 
# particular locations.

# Resource_type
ggplot(resource_type, aes(x = id, y = resource_type)) + geom_point(alpha = 0.2)
#    Comment: This is a cleaner plot with some resource being continuous like Log_feature and 
# Train datasets.

# Event_type
ggplot(event_type, aes(x = id, y = event_type)) + geom_point(alpha = 0.05)
ggplot(event_type, aes(x = id, y = event_type)) + geom_jitter(alpha = 0.05)
#    Comment: There are certain event types that are frequently occuring or being recorded. 
# Reviewing with jitter does not show much gaps in frequency

# Severity_type
ggplot(sev_type, aes(x = id, y = severity_type)) + geom_jitter(alpha = 0.2)
ggplot(networkG, aes(
  x = id, y = severity_type, col = as.factor(fault_severity), size = as.factor(fault_severity))) + 
  geom_jitter(alpha = 0.1) 
#   Comment: The distinct categories have clear proportion clear distributed in the logs. 
# Severity_type no.1 has the most severity impact on users. Severity_type no.3 only gives a 
# low level of severity on rare occasions.


###Scatterplot - "id" baseline
#~Location
ggplot(train, aes(
  x = id, y = location, col = as.factor(fault_severity), size = as.factor(fault_severity) )) + 
  geom_point(alpha = 0.3)
# Comment: There are some locations that have continuous faults reported

#~Log_feature
ggplot(log_feature, aes(x = id, y = log_feature, size = volume)) + geom_point(alpha = 0.05)
# Comment: This confirms the train dataset that some log features have continous feature logs, 
# the right is where volume is above 10. This pattern may correlate to machine type found at 
# particular locations.

#~Resource_type
ggplot(resource_type, aes(x = id, y = resource_type)) + geom_point(alpha = 0.2)
# Comment: This is a cleaner plot with some resource being continuous like Log_feature and 
# Train datasets.

#~Event_type
ggplot(event_type, aes(x = id, y = event_type)) + geom_point(alpha = 0.05)
ggplot(event_type, aes(x = id, y = event_type)) + geom_jitter(alpha = 0.05)
# Comment: There are certain event types that are frequently occuring or being recorded.
# Reviewing with jitter does not show much gaps in frequency

#~Severity_type
ggplot(sev_type, aes(x = id, y = severity_type)) + geom_jitter(alpha = 0.2)
ggplot(networkG, aes(
  x = id, y = severity_type, col = as.factor(fault_severity), size = as.factor(fault_severity))) +
  geom_jitter(alpha = 0.1) 
# Comment: The distinct categories have clear proportion clear distributed in the logs. 
# Severity_type no.1 has the most severity impact on users. Severity_type no.3 only gives 
# a low level of severity on rare occasions.


###Scatterplot - "location" baseline

#~Log_feature
ggplot(networkG, aes(x = location, y = log_feature, size = volume)) + 
  geom_jitter(alpha = 0.05)
#~Log_feature WITH
ggplot(networkG, aes(
  x = location, y = log_feature, col= as.factor(resource_type)) ) + 
  geom_jitter(alpha = 0.05)
#~Resource_type
ggplot(networkG, aes(x = location, y = resource_type)) + geom_point(alpha = 0.2)
#~Event_type
ggplot(networkG, aes(x = location, y = event_type, col = as.factor(resource_type))) + 
  geom_point(alpha = 0.05)
#~Severity_type
ggplot(train, aes(
  x = location, y = fault_severity, col = as.factor(fault_severity), size = as.factor(fault_severity) )) + 
  geom_point(alpha = 0.3)
#~Log_feature WITH jitter
ggplot(networkG, aes(x = location, y = log_feature ) ) + 
  geom_jitter(alpha = 0.05)


###Scatterplot - "event" & "feature" relationship with "resource"
ggplot(networkG, aes(
  x = log_feature, y = event_type, size = volume, shape = as.factor(resource_type))) + 
  geom_point(alpha = 0.05) 
# The shape palette can deal with a maximum of 6 discrete values because more than 6 becomes
# difficult to discriminate; you have 10. Consider specifying shapes manually if you must have
# them.
# There are some event types carried over to multiple log_feature, with various event types 
# have varying degrees of log_feature volumes, hence there may not be any correction there, 
# certain fault events are more highly probable with particular resources as evidence of
# clusters. At this point, volume in reference with log_feature may be telling of the 
# quantity of equipment and not severity.

#~further look at relationship of event_type with log_feature, sized in Volume, 
# faceted by resource_type
ggplot(networkG, aes(
  x = log_feature, y = event_type, size = volume, col = resource_type)) + 
  geom_point(alpha = 0.1) + facet_grid( . ~ resource_type )


###Scatterplot - resource behaviours via "event" & "feature" & "severity_type"

#~ fixed events by resource
ggplot(networkG, aes(x = resource_type, y = event_type)) + 
  geom_point(alpha = 0.05) 
# Some resource share the same event_type but the plot further shows that resource_type 
# is a discrete variable of range 10.

#~composition of resource on severity
ggplot(networkG, aes(x = resource_type, y = severity_type)) + geom_jitter(alpha = 0.05)


###Scatterplot - Volume vs fault_severity on "severity_type" & "resource"

#~fault_severity
ggplot(networkG, aes(
  x = event_type, y = fault_severity, col = as.factor(fault_severity), size = as.factor(fault_severity) )) + 
  geom_point(alpha = 0.3)
#~volume
ggplot(networkG, aes(
  x = resource_type, y = severity_type, col = volume, size = volume)) + 
  geom_point(alpha = 0.01)


###Approach to the problem

# From this analysis, different combinations will present different fault_severity 
# predictions. The data is highly structural with many discrete variables.
# A categorical approach to determine the different variable combinations and 
# probability to predict the fault_severity is recommended. By categorising and walking 
# through a decision tree will be best, such as randomForest methods.


##
##-------------------------
### Random Forest Modelling
##-------------------------

#------------------------------------------------
## Feature Engineering to reduce variables

# `Approach reduce combinations`
# Each resource type exhibits 
# certain particular severity_type it will be good to quickly eliminate these
# to simplify the model by having less variables
# as well as facilitate randomForest features

# 1 - location & resource combinations (network tbl)
# 2 - log_feature & event_type = service call (incident_log tbl)
# 3 - service_call & resource (incident_log tbl)
# 4 - Extract count of log_features at each location
# 5 - Extract count of events present at each location
# 6 - Extract count of resource at each location
# 7 - Get probability of fault_severity by resource (proportion)


# Currently these are the rows in table network - incident_log
dim(network)
# 61839     8
dim(incident_log)
# 146423      6


# Ready data set for feature engineering

#dataframes for feature engineering
networkE <- 
  network %>% 
  arrange(location, resource_type, log_feature, event_type, severity_type, fault_severity)

incident_logE <-
  incident_log %>% 
  arrange(resource_type, log_feature, event_type, severity_type, volume)

# converting variables to factors
networkE$log_feature <- as.factor(networkE$log_feature)
networkE$event_type <- as.factor(networkE$event_type)
networkE$resource_type <- as.factor(networkE$resource_type)
networkE$severity_type <- as.factor(networkE$severity_type)
networkE$location <- as.factor(networkE$location)
networkE$fault_severity <- as.factor(networkE$fault_severity)

incident_logE$log_feature <- as.factor(incident_logE$log_feature)
incident_logE$resource_type <- as.factor(incident_logE$resource_type)
incident_logE$event_type <- as.factor(incident_logE$event_type)
incident_logE$severity_type <- as.factor(incident_logE$severity_type)

isZero <- sapply(networkE$fault_severity, 
                 function(x) { if (x == 1 | x == 2){ 0 }else{ 1}})

isOne <- sapply(networkE$fault_severity, 
                function(x) { if (x == 0 | x == 2){ 0 }else{ 1}})

isTwo <- sapply(networkE$fault_severity, 
                function(x) { if (x == 1 | x == 0){ 0 }else{ 1}})
networkE <-
  networkE %>% 
  mutate( isZero = isZero ) %>% 
  mutate( isOne = isOne ) %>% 
  mutate( isTwo = isTwo)


#~~~~~
# 1 - location & resource combinations (network tbl)

loc.res <- networkE %>% 
  select(location, resource_type) %>%
  transmute( loc.res = paste(location,resource_type, sep="-"))
loc.res_freq <- loc.res %>% group_by(loc.res) %>% count()
summary(loc.res_freq) #1439 combinations in network

# Add feature to table

networkE <- 
  networkE %>% 
  arrange(location, resource_type) %>%
  mutate(loc.res = paste(location,resource_type, sep="-"))
networkE$loc.res <- as.factor(networkE$loc.res)
str(networkE)

#~~~~~
# 2 - log_feature & event_type = service call (incident_log tbl)

feat.evnt <- networkE %>% 
  select(log_feature, event_type) %>%
  transmute( feat.evnt = paste(log_feature, event_type, sep="x"))
feat.evnt_freq <- feat.evnt %>% group_by(feat.evnt) %>% count()
summary(feat.evnt_freq) #1690 combinations in network


# Add feature to table

networkE <- 
  networkE %>% 
  mutate( feat.evnt = paste(log_feature, event_type, sep="x"))
networkE$feat.evnt <- as.factor(networkE$feat.evnt)
str(networkE)


#examine events in incident_log table

feat.evntI <- incident_logE %>% 
  select(log_feature, event_type) %>%
  transmute( feat.evntI = paste(log_feature, event_type, sep="x"))
feat.evntI_freq <- feat.evntI %>% group_by(feat.evntI) %>% count()
summary(feat.evntI_freq) #2290 combinations in incident_log


# Add feature to incident_log table

incident_logE <- 
  incident_logE %>% 
  mutate( feat.evntI = paste(log_feature, event_type, sep="x"))

str(incident_logE)
head(incident_logE)


#~~~~~
# 3 - service_call & resource (incident_log tbl)
#     add resource to feat.evntI

res.featevnt <- 
  networkE %>% 
  select(resource_type, feat.evnt) %>%
  transmute( res.featevnt = paste(resource_type, feat.evnt, sep="-"))
res.featevnt_freq <- res.featevnt %>% group_by(res.featevnt) %>% count()
summary(res.featevnt_freq) #4505 combinations in network

# examine events in incident_log table xx

res.featevntI <- incident_logE %>% 
  select(resource_type, feat.evntI) %>%
  transmute( res.featevntI = paste(resource_type, feat.evntI, sep="-"))
res.featevntI_freq <- res.featevntI %>% group_by(res.featevntI) %>% count()
summary(res.featevntI_freq) #6130 combinations in incident_log

# Add feature to incident_log table

incident_logE <- 
  incident_logE %>% 
  mutate( res.featevntI = paste(resource_type, feat.evntI, sep="-"))
#incident_log$res.feat.evntI <- as.factor(incident_log$res.feat.evntI)
str(incident_logE)
head(incident_logE)

ggplot(incident_logE, aes(res.featevntI, severity_type, col = resource_type)) + geom_point()


#~~~~~
# 4 - Extract count of log_features at each location

n_loc.feat <- 
  networkE  %>% 
  select(location, log_feature)  %>% 
  group_by(location)  %>% 
  summarise(n_distinct(log_feature))
colnames(n_loc.feat) <- c("location", "t_feature")
colnames(n_loc.feat)

# Add feature to table

networkE <- merge(networkE, n_loc.feat, by = c("location"), all.x = TRUE)
head(networkE)


#~~~~~
# 5 - Extract count of events present at each location

n_loc.event <- 
  networkE  %>% 
  select(location, event_type)  %>% 
  group_by(location)  %>% 
  summarise(n_distinct(event_type))
colnames(n_loc.event) <- c("location", "t_event")
colnames(n_loc.event)

# Add feature to table

networkE <- merge(networkE, n_loc.event, by = c("location"), all.x = TRUE)
head(networkE)

#~~~~~
# 6 - Extract count of resource_type at each location

n_loc.res <- 
  networkE  %>% 
  select(location, resource_type)  %>% 
  group_by(location)  %>% 
  summarise(n_distinct(resource_type)) 
colnames(n_loc.res) <- c("location", "t_resource")
colnames(n_loc.res)
# Add feature to table

networkE <-merge(networkE, n_loc.res, by = c("location"), all.x = TRUE)
head(networkE)


#~~~~~
# 7 - Get probability of fault_severity by resource (proportion) (incident_log table)

# Total entries
n_iEntries <- dim(incident_logE)[1]
n_iEntries
# 146423

# Total of severity entries by type
total_sev<- 
  incident_logE %>% 
  select(severity_type, resource_type) %>% 
  group_by(severity_type) %>% 
  summarise(n()) 

# Rename column
colnames(total_sev) <- c("severity_type", "sev_count")

# Add rel freq
total_sev$rf <- (total_sev$sev_count / n_iEntries )
total_sev

# Total of severity entries by resource_type
t_res.sev <-
  incident_logE%>% 
  select(severity_type, resource_type) %>% 
  group_by(severity_type, resource_type) %>% 
  summarise(n()) %>% 
  rename(count_res.sev = `n()`) %>% 
  mutate( rf = count_res.sev / n_iEntries)

# Building contingency table rows
a1<- (t_res.sev %>% filter(severity_type == 1))#$count_res.sev
a2<- (t_res.sev %>% filter(severity_type == 2))#$count_res.sev
a3<- (t_res.sev %>% filter(severity_type == 3))#$count_res.sev
a4<- (t_res.sev %>% filter(severity_type == 4))#$count_res.sev
a5<- (t_res.sev %>% filter(severity_type == 5))#$count_res.sev

# Total of resource_type entries
total_res<- 
  incident_logE %>% 
  select(severity_type,resource_type) %>% 
  group_by(resource_type) %>% 
  summarise(n()) 
total_res

# Add ratio of resource_type to severity
#   1st - Join sev & resource columns to then merge with incident table
merge_res <-
  t_res.sev %>% 
  mutate( sev.res = paste(severity_type, resource_type, sep="-"))
merge_res <- as.data.frame(merge_res)
merge_res <- merge_res %>% select(sev.res, rf)
colnames(merge_res) <- c("sev.res", "sev_res.rf")

incident_logE <-
  incident_logE %>% 
  mutate( sev.res = paste(severity_type, resource_type, sep="-"))

incident_logE <- merge(incident_logE, merge_res, by = c("sev.res"), all.x = TRUE)

networkE <-
  networkE %>% 
  mutate( sev.res = paste(severity_type, resource_type, sep="-"))

networkE <- merge(networkE, merge_res, by = c("sev.res"), all.x = TRUE)
str(networkE)

#------------------------------------------------
# Before building model
# Step 1 : split data to training and testing set

set.seed(1000)
split = sample.split(networkE$fault_severity, SplitRatio = 0.7)
train = subset(networkE, split == T )
test = subset(networkE, split == F )


#^^^^^^^^^^^^^^^^
## Baseline model - no feature engineering
# 

## Using randomForest
# NOTE: Cannot handle categorical predictors with more than 53 categories.
# The response has five or fewer unique values.  Are you sure you want to do regression?

#-------------------
##### Basemodel1
testForest = randomForest(fault_severity ~ resource_type + severity_type, 
                          data = train,
                          nodesize = 25, 
                          ntree = 4000)

# Generate predictions 
predForest = predict(testForest, newdata = test)

# Build matrix to evaluate model vs predictions
table(test$fault_severity, predForest)
#  predForest
#      0     1       2
#0 10872     107     0
#1  4750     207     0
#2  2587     0      28    # Overall_accuracy = 0.5987278

##### Lower nodesize = 12
testForest = randomForest(fault_severity ~ resource_type + severity_type, 
                          data = train,
                          nodesize = 12, 
                          ntree = 4000)

# Generate predictions 
predForest = predict(testForest, newdata = test)

# Build matrix to evaluate model vs predictions
table(test$fault_severity, predForest)
#  predForest
#      0     1       2
#0 10872     107     0
#1  4750     207     0    # Lower nodesize -> 12
#2  2587     0      28    # Overall_accuracy = 0.5987278 - no change


##### Add event_type as a variable

testForest = randomForest(fault_severity ~ event_type + resource_type + severity_type, 
                          data = train,
                          nodesize = 12, 
                          ntree = 4000)

# Generate predictions 
predForest = predict(testForest, newdata = test)

# Build matrix to evaluate model vs predictions
table(test$fault_severity, predForest)
#  predForest
#      0     1       2
#0 10872     107     0
#1  4750     207     0    # event variable has no impact
#2  2587     0      28    # Overall_accuracy = 0.5987278 - no change

##### Add volume as a variable
testForest = randomForest(fault_severity ~ volume + resource_type + severity_type, 
                          data = train,
                          nodesize = 12, 
                          ntree = 4000)

# Generate predictions 
predForest = predict(testForest, newdata = test)

# Build matrix to evaluate model vs predictions
table(test$fault_severity, predForest)
#  predForest
#      0     1     2
#0 10771    75   133
#1  4699   183    75
#2  2379     0   236     

# Model evaluation
#(10771 + 183 + 236) / ( 10771 + 183 + 236 + 75 + 133 + 75 + 4699 + 2379 + 0 )
# Overall_accuracy = 0.603202 @ ntree = 4000  ; nodesize = 12

#Results from randomForest parameter adjustments
#
#(10772 + 186 + 234) / ( 10772 + 186 + 234 + 78 + 129 + 75 + 4696 + 2381 + 0 )
# Overall_accuracy = 0.6033098 @ ntree = 8000  ; nodesize = 12 # better !!!

#(10771 + 182 + 236) / ( 10771 + 182 + 236 + 75 + 133 + 75 + 4700 + 2379 + 0 )
# Overall_accuracy = 0.6031481 @ ntree = 8000  ; nodesize = 25 # worst x

#(10774 + 183 + 235) / ( 10774 + 183 + 235 + 75 + 130 + 75 + 4699 + 2380 + 0 )
# Overall_accuracy = 0.6033098 @ ntree = 10000  ; nodesize = 12 # same =

#(10770 + 186 + 236) / ( 10770 + 186 + 236 + 78 + 131 + 75 + 4696 + 2379 + 0 )
# Overall_accuracy = 0.6033098 @ ntree = 4000  ; nodesize = 15 # same =


#-------------------
## Model#1 - with engineered features
# 

# - - - - - 
##### with t_feature
modForest1a = randomForest(fault_severity ~ t_feature + resource_type + severity_type, 
                          data = train,
                          nodesize = 12, 
                          ntree = 8000)

# Generate predictions 
predmForest = predict(modForest1a, newdata = test)

# Build matrix to evaluate model vs predictions
table(test$fault_severity, predmForest)

#  predmForest
#      0     1     2
#0 10401   574     4
#1  3745  1191    21
#2  2026   433   156

# Model evaluation with t_feature
#(10401 + 1191 + 156) / ( 10401 + 1191 + 156 + 574 + 4 + 21 + 3745 + 2026 + 433 )
# Overall_accuracy = 0.6332812 @ ntree = 8000  ; nodesize = 12


# - - - - - 
##### with t_resource
modForest1b = randomForest(fault_severity ~ t_resource + resource_type + severity_type, 
                          data = train,
                          nodesize = 12, 
                          ntree = 8000)

# Generate predictions 
predmForest = predict(modForest1b, newdata = test)

# Build matrix to evaluate model vs predictions
table(test$fault_severity, predmForest)

#  predmForest
#      0     1     2
#0 10497   461    21
#1  4155   788    14
#2  2297   189   129

# Model evaluation with t_resource
#(10497 + 788 + 129) / ( 10497 + 788 + 129 + 461 + 21 + 14 + 4155 + 2297 + 189 )
# Overall_accuracy = 0.6152768 @ ntree = 8000  ; nodesize = 12

# - - - - - 
##### with t_event
modForest1c = randomForest(fault_severity ~ t_event + resource_type + severity_type, 
                           data = train,
                           nodesize = 12, 
                           ntree = 8000)

# Generate predictions 
predmForest = predict(modForest1c, newdata = test)

# Build matrix to evaluate model vs predictions
table(test$fault_severity, predmForest)

#  predmForest
#      0     1     2
#0 10792   137    50
#1  4591   351    15
#2  2402     9   204

# Model evaluation with t_event
#(10792 + 351 + 204) / ( 10792 + 351 + 204 + 137 + 50 + 15 + 4591 + 2402 + 9 )
# Overall_accuracy = 0.6116651 @ ntree = 8000  ; nodesize = 12

# - - - - - 
##### with sev-res.rf
modForest1d = randomForest(fault_severity ~ sev_res.rf + resource_type + severity_type, 
                           data = train,
                           nodesize = 12, 
                           ntree = 8000)

# Generate predictions 
predmForest = predict(modForest1d, newdata = test)

# Build matrix to evaluate model vs predictions
table(test$fault_severity, predmForest)

#  predmForest
#      0     1     2
#0 10463   516     0
#1  4218   739     0
#2  2364   223    28

# Model evaluation
#(10463 + 739 + 28) / ( 10463 + 739 + 28 + 516 + 0 + 0 + 4218 + 2364 + 223 )
# Overall_accuracy = 0.6053582 @ ntree = 8000  ; nodesize = 12


# - - - - - 
##### with t_feature & sev_res.rf & t_resource
modForest2a = randomForest(fault_severity ~ t_feature + sev_res.rf + t_resource, 
                          data = train,
                          nodesize = 12, 
                          ntree = 8000)

# Generate predictions 
predmForest = predict(modForest2a, newdata = test)

# Build matrix to evaluate model vs predictions
table(test$fault_severity, predmForest)

#  predmForest
#    0     1    2
#0 9648  876  455
#1 2866 1656  435
#2 1196  508  911

# Model evaluation
#(9648 + 1656 + 911) / ( 9648 + 1656 + 911 + 876 + 455 + 435 + 2866 + 1196 + 508 )
# Overall_accuracy = 0.6584551 @ ntree = 8000  ; nodesize = 12

# - - - - - ***best Overall_accuracy***
##### with t_feature & sev_res.rf & t_resource
modForest2b = randomForest(fault_severity ~ t_feature + sev_res.rf + volume, 
                          data = train,
                          nodesize = 12, 
                          ntree = 8000)

# Generate predictions 
predmForest = predict(modForest2b, newdata = test)

# Build matrix to evaluate model vs predictions
table(test$fault_severity, predmForest)

#  predmForest
#    0     1    2
#0 9997  679  303
#1 3177 1514  266
#2 1243  518  854

# Model evaluation
#(9997 + 1514 + 854) / ( 9997 + 1514 + 854 + 679 + 303 + 266 + 3177 + 1243 + 518 )
# Overall_accuracy = 0.6665409 @ ntree = 8000  ; nodesize = 12

# - - - - - 
##### with t_feature & sev_res.rf & t_resource
modForest2c = randomForest(fault_severity ~ t_feature + t_resource + volume, 
                          data = train,
                          nodesize = 12, 
                          ntree = 8000)

# Generate predictions 
predmForest = predict(modForest2c, newdata = test)

# Build matrix to evaluate model vs predictions
table(test$fault_severity, predmForest)

#  predmForest
#    0     1    2
#0 9666 1017  296
#1 2871 1810  276
#2 1203  685  727

# Model evaluation
#(9666 + 1810 + 727) / ( 9666 + 1810 + 727 + 1017 + 296 + 276 + 2871 + 1203 + 685 )
# Overall_accuracy = 0.6578082 @ ntree = 8000  ; nodesize = 12

##### Best model tuning - found:later, no tuning improved
modForest2bi = randomForest(fault_severity ~ t_feature + sev_res.rf + volume, 
                         data = train,
                         nodesize = 12, 
                         ntree = 4000)

# Generate predictions 
predmForest = predict(modForest2bi, newdata = test)

# Build matrix to evaluate model vs predictions
table(test$fault_severity, predmForest)
#  predmForest
#    0     1    2
#0 9966  701  312
#1 3164 1522  271
#2 1247  524  844      # !rate to beat = 0.6665409!

# Model evaluation
#(9666 + 1522 + 844) / ( 9666 + 1522 + 844 + 701 + 312 + 271 + 3164 + 1247 + 524 )
# Overall_accuracy = 0.6592515 @ ntree = 4000  ; nodesize = 12 # worst x

#(9994 + 1501 + 866) / ( 9994 + 1501 + 866 + 665 + 320 + 273 + 3183 + 1218 + 531 )
# Overall_accuracy = 0.6663253 @ ntree = 6000  ; nodesize = 12 # worst x

#(10012 + 1483 + 841) / ( 10012 + 1483 + 841 + 656 + 311 + 269 + 3205 + 1252 + 522 )
# Overall_accuracy = 0.6649776 @ ntree = 8000  ; nodesize = 15 # worst x

#(9950 + 1543 + 854) / ( 9950 + 1543 + 854 + 731 + 298 + 272 + 3142 + 1217 + 544 )
# Overall_accuracy = 0.6655706 @ no set parameters # worst x

##### Best model + t_event
modForest3a = randomForest(fault_severity ~ t_feature + sev_res.rf + volume + t_event, 
                         data = train,
                         nodesize = 12, 
                         ntree = 8000)

# Generate predictions 
predmForest = predict(modForest3a, newdata = test)

# Build matrix to evaluate model vs predictions
table(test$fault_severity, predmForest)
#  predmForest
#    0     1    2
#0 9638  913  428
#1 2051 2461  445
#2  608  368 1639      # !rate to beat = 0.6665409!

# Model evaluation
#(9638 + 2461 + 1639) / ( 9638 + 2461 + 1639 + 913 + 428 + 445 + 2051 + 608 + 368 )
# Overall_accuracy = 0.7405531 @ ntree = 4000  ; nodesize = 12

##### Best model + t_resource
modForest3b = randomForest(fault_severity ~ t_feature + sev_res.rf + volume + t_resource, 
                           data = train,
                           nodesize = 12, 
                           ntree = 8000)

# Generate predictions 
predmForest = predict(modForest3b, newdata = test)

# Build matrix to evaluate model vs predictions
table(test$fault_severity, predmForest)
#  predmForest
#    0     1    2
#0 9607  953  419
#1 2180 2316  461
#2  612  459 1544      # !rate to beat = 0.7405531!

# Model evaluation
#(9607 + 2316 + 1544) / ( 9607 + 2316 + 1544 + 953 + 419 + 461 + 2180 + 612 + 459 )
# Overall_accuracy = 0.7259447 @ ntree = 8000  ; nodesize = 12


##### Best model + severity_type
modForest3c = randomForest(fault_severity ~ t_feature + sev_res.rf + volume + severity_type, 
                           data = train,
                           nodesize = 12, 
                           ntree = 8000)

# Generate predictions 
predmForest = predict(modForest3c, newdata = test)

# Build matrix to evaluate model vs predictions
table(test$fault_severity, predmForest)
#  predmForest
#    0     1    2
#0 9519 1089  371
#1 2497 2142  318
#2  874  682 1059      # !rate to beat = 0.7405531!

# Model evaluation
#(9519 + 2142 + 1059) / ( 9519 + 2142 + 1059 + 1089 + 371 + 318 + 2497 + 874 + 682 )
# Overall_accuracy = 0.6856773 @ ntree = 8000  ; nodesize = 12

##### Best model + resource_type
modForest3d = randomForest(fault_severity ~ t_feature + sev_res.rf + volume + resource_type, 
                           data = train,
                           nodesize = 12, 
                           ntree = 8000)

# Generate predictions 
predmForest = predict(modForest3d, newdata = test)

# Build matrix to evaluate model vs predictions
table(test$fault_severity, predmForest)
#  predmForest
#    0     1    2
#0 9525 1077  377
#1 2540 2069  348
#2  887  631 1097      # !rate to beat = 0.7405531!

# Model evaluation
#(9525 + 2069 + 1097) / ( 9525 + 2069 + 1097 + 1077 + 377 + 348 + 2540 + 887 + 631 )
# Overall_accuracy = 0.6841141 @ ntree = 8000  ; nodesize = 12

##### t_feature + sev_res.rf + t_resource + t_event
modForest4a = randomForest(fault_severity ~ t_feature + sev_res.rf + t_resource + t_event, 
                           data = train,
                           nodesize = 12, 
                           ntree = 8000)

# Generate predictions 
predmForest = predict(modForest4a, newdata = test)

# Build matrix to evaluate model vs predictions
table(test$fault_severity, predmForest)
#  predmForest
#    0     1    2
#0 9643  841  495
#1 2179 2209  569
#2  748  445 1422      # !rate to beat = 0.7405531!

# Model evaluation
#(9643 + 2209 + 1422) / ( 9643 + 2209 + 1422 + 841 + 495 + 569 + 2179 + 748 + 445 )
# Overall_accuracy = 0.7155409 @ ntree = 8000  ; nodesize = 12

##### t_feature + t_resource + t_event + volume
modForest4b = randomForest(fault_severity ~ t_feature + t_resource + t_event + volume, 
                           data = train,
                           nodesize = 12, 
                           ntree = 8000)

# Generate predictions 
predmForest = predict(modForest4b, newdata = test)

# Build matrix to evaluate model vs predictions
table(test$fault_severity, predmForest)
#  predmForest
#    0     1    2
#0 9688  775  516
#1 2235 2225  497
#2  764  281 1570      # !rate to beat = 0.7405531!

# Model evaluation
#(9688 + 2225 + 1570) / ( 9688 + 2225 + 1570 + 775 + 516 + 497 + 2235 + 764 + 281 )
# Overall_accuracy = 0.7268072 @ ntree = 8000  ; nodesize = 12



##-------------------------
### Comments
##-------------------------
##

#
# Cross-validation 
# When using random forest the multiple bagging variables to produce and training 
# random forest prevents over-fitting.


#rpart is not used because it only evaluates binary yes or no outcomes
#library(e1071)
#library(caret)
# get cp value
# define number of folds using trainControl function
#fitcontrol = trainControl(method = "cv", number = 5) # use cross validation method, use 10 folds
# setup possible paramater values for cp (complex Parameter)
#cartGRid = expand.grid( .cp = (1:20)*0.005) # the (1:50) =  1 through to 50 ie 0.01 -> 0.5

#train(fault_severity ~ t_feature + sev_res.rf + volume, #resource_type + severity_type ,
#      data = train, 
#      method = "rpart",
#      trControl = fitcontrol, 
#      tuneGrid = cartGRid)

# Building the model
#tree1 <- rpart(isZero ~ t_feature + sev_res.rf + volume, #resource_type + severity_type , 
#               data = train,
#               method = "class", 
#               control = rpart.control(cp = 0.005))
#prp(tree1)
#plot(tree1)

##---------------------------------
### Other graphs & Initial Findings
##---------------------------------
##

#------------------------------------------------------------------------------------------
#####train

## frequency at each location
ggplot(train, aes(x = location)) + geom_histogram(bins = 100)

ggplot(networkG, aes(x = location)) + geom_histogram(bins = 100)
## Removed 84584 rows containing non-finite values (stat_bin).
##-Conclusion: some ids from joining other tables together have missing logs.
##-Some locations have more frequent fault reporting (any correlation with resource type?)

## relationship of location with id
ggplot(train, aes(x = id, y = location, col = as.factor(fault_severity), size = as.factor(fault_severity) )) + geom_point(alpha = 0.3)
##-There is a pattern of faults to have the same severity type hence the linear repetition across the id scale

ggplot(networkG, aes(x = id, y = location)) + geom_point() + facet_grid( . ~ faulty_severity)
##Error in layout_base(data, cols, drop = drop) : 
##  At least one layer must contain all variables used for facetting

##relationship of feature with fault_severity
ggplot(train, aes(x = location, y = fault_severity, col = as.factor(fault_severity), size = as.factor(fault_severity) )) + geom_point(alpha = 0.3)



#####log_feature

## frequency at each location
ggplot(log_feature, aes(x = log_feature, y = volume, col = log_feature)) + geom_point(alpha = 0.2) + coord_cartesian(ylim = c(0, 1000)) # removed outlier
##-The Volume variable here is a continuous measure with some log_feature more frequent than others

## relationship of log_feature with id
ggplot(log_feature, aes(x = id, y = log_feature, size = volume)) + geom_point(alpha = 0.05)
#-There are no missing values in this dataset, but reveals some common error log_features logged regularly

## Volume of the faults 
volume_above10 <- log_feature %>% filter(volume > 10)
ggplot(volume_above10, aes(x = id, y = log_feature, size = volume)) + geom_point(alpha = 0.05)
##-The Volume depicted is fairly uniform to the type of feature the fault being recorded with, showing a uniform match with the fault feature recorded

##-Question is of there is a relation of these log_features with location.
ggplot(network, aes(x = location, y = log_feature, size = volume)) + geom_jitter(alpha = 0.05)
##-Log features appear distinctly at grouped location numbers, showing where the features regularly appear like a map


##relationship of log_feature with fault_severity
ggplot(network, aes(x = log_feature, y = fault_severity, col = as.factor(fault_severity), size = as.factor(fault_severity) )) + geom_point(alpha = 0.3)

##relationship of log_feature X location with fault_severity
ggplot(network, aes(x = location, y = log_feature, col = as.factor(fault_severity))) + geom_point(alpha = 0.1) + facet_grid( . ~ resource_type)



#####resource_type

## frequency at each location
ggplot(resource_type, aes(x= resource_type)) + geom_histogram(bins = 100)
##There are 10 distinct resource types

## relationship of resource_type with id
ggplot(resource_type, aes(x = id, y = resource_type)) + geom_point(alpha = 0.2)
##-Shows some resource types are more likely to be logged more frequently

## relationship of resource_type with location
ggplot(networkG, aes(x = location, y = resource_type)) + geom_point(alpha = 0.2)
ggplot(networkG, aes(x = location, y = resource_type)) + geom_jitter(alpha = 0.2) 
##-In comparing the locations, there is clustering on certain resource type by location number
##-As the dataset records errors, it is unclear whether all locations have all resource types 
##-but some location's equipment may be more prone to fault then others, this may be due to the
##-location's position in a high traffic hub or that there particular resource is due for replacement

## relationship of resource_type with log_feature, as the resource type is a discrete variable
ggplot(networkG, aes(x = location, y = log_feature, col= as.factor(resource_type)) ) + geom_jitter(alpha = 0.05)
##-Here we can tell that by location, error features logged correspond to certain resource type 
##-at that location, with each resource having distinct log features.

##-Question here will be is there any correlation of the resource type, log feature to 
##-severity_type to produce the level of fault_severity.
##-In plain english this maybe the pinpoint of a particular machine that location that 
##-works as a main artery of the network to publish repair urgency.

##- "location" > "resource" > "log_feature" > "Volume" ~ fault occurance

## compare resource_type with fault_severity
ggplot(networkG, aes(x = resource_type, y = fault_severity)) + geom_jitter(alpha = 0.05)



#####event_type
## frequency at each location
ggplot(event_type, aes(event_type)) + geom_histogram(bins = 100)
ggplot(networkG, aes(x = event_type)) + geom_histogram(bins = 100) # There are no missing values
##-There appears to be distinct but also popular events.

##-Question is, do these events correlation with particular resource (machine) that can 
##-interpret level of fault severity?

## relationship of event_type with id
ggplot(event_type, aes(x = id, y = event_type)) + geom_point(alpha = 0.05)
ggplot(event_type, aes(x = id, y = event_type)) + geom_jitter(alpha = 0.05)
##-There are certain event types that are frequently occuring or being recorded. Reviewing with jitter does not show much gaps in frequency

## relationship of event_type with resource_type
ggplot(networkG, aes(x = resource_type, y = event_type)) + geom_point(alpha = 0.05) 
## Some resource share the same event_type but the plot further shows that resource_type is a discrete variable of range 10.

## relationship of event_type with log_feature where Volume is displayed differentiated by resource_type
ggplot(networkG, aes(x = log_feature, y = event_type, size = volume, shape = as.factor(resource_type))) + geom_point(alpha = 0.05) 
## The shape palette can deal with a maximum of 6 discrete values because more than 6 becomes
## difficult to discriminate; you have 10. Consider specifying shapes manually if you must have
## them.

##-There are some event types carried over to multiple log_feature, with various event types 
##-have varying degrees of log_feature volumes, hence there may not be any correction there, 
##-certain fault events are more highly probable with particular resources as evidence of clusters

##- At this point, volume in reference with log_feature may be telling of the quantity of equipment
##- and not severity

## further look at relationship of event_type with log_feature, sized in Volume, faceted by resource_type
ggplot(networkG, aes(x = log_feature, y = event_type, size = volume, col = resource_type)) + geom_point(alpha = 0.1) + facet_grid( . ~ resource_type )
##-Resources 4 & 6 have very similar patterns

## relationship of event_type with location
ggplot(networkG, aes(x = location, y = event_type, col = as.factor(resource_type))) + geom_point(alpha = 0.05)
##-There is a similarity of pattern with "locationXresource_type" and "locationXlog_feature".

##relationship of event_type with fault_severity
ggplot(networkG, aes(x = event_type, y = fault_severity, col = as.factor(fault_severity), size = as.factor(fault_severity) )) + geom_point(alpha = 0.3)


######severity_type
## frequency at each location
ggplot(sev_type, aes(severity_type)) + geom_histogram(bins = 100)
ggplot(networkG, aes(x = severity_type)) + geom_histogram(bins = 100) # There are no missing values
##-There appears to be 5 distinct types.

## relationship of feature with id
ggplot(sev_type, aes(x = id, y = severity_type)) + geom_jitter(alpha = 0.2)
## The distinct categories have clear proportion clear distributed in the logs

## relationship of feature with id highlighted by fault_severity
ggplot(networkG, aes(x = id, y = severity_type, col = as.factor(fault_severity), size = as.factor(fault_severity))) + geom_jitter(alpha = 0.1) 
##-Most severe faults are categorized by severity_type no.1 .
##-Severity_type no.3 only attracts fault level of 1 .

##relationship of severity_type with fault_severity
ggplot(networkG, aes(x = severity_type, y = fault_severity, col = as.factor(fault_severity), size = as.factor(fault_severity) )) + geom_point(alpha = 0.3)
##-Severity_type no.3 only attracts fault level of 1 confirmed.

##relationship of feature with location highlighted with fault_severity
ggplot(networkG, aes(x = location, y = severity_type, col = as.factor(fault_severity), size = as.factor(fault_severity))) + geom_jitter(alpha = 0.1) 
##-High levels of faults appear at low number locations or locations numbered greater than 500

#relationship of feature with resource_type
ggplot(networkG, aes(x = resource_type, y = severity_type)) + geom_jitter(alpha = 0.05)

##relationship of feature with resource_type with fault_severity highlights
ggplot(networkG, aes(x = resource_type, y = severity_type, col = as.factor(fault_severity), size = as.factor(fault_severity))) + geom_point(alpha = 0.01)
##-all resources can display the first severity type with high fault level

## compare faulty severity highlights with volume
ggplot(networkG, aes(x = resource_type, y = severity_type, col = volume, size = volume)) + geom_point(alpha = 0.01)



#####Other exploration with fault_severity

##relationship of event_type X location with fault_severity
ggplot(networkG, aes(x = location, y = event_type, col = as.factor(fault_severity))) + geom_point(alpha = 0.1) + facet_grid( . ~ resource_type)

##relationship of event_type X log_feature with fault_severity
ggplot(networkG, aes(x = log_feature, y = event_type, col = as.factor(fault_severity))) + geom_point(alpha = 0.1) + facet_grid( . ~ resource_type)

## compare volume with fault_severity
ggplot(network, aes(x = volume, y = fault_severity, col = as.factor(fault_severity))) + geom_point(alpha = 0.2)

## compare event_type with fault_severity
ggplot(networkG, aes(x = event_type, y = fault_severity,col = as.factor(fault_severity))) + geom_jitter(alpha = 0.2)


## compare resource_type with fault_severity
ggplot(networkG, aes(x = resource_type, y = fault_severity, col = as.factor(fault_severity), size = as.factor(fault_severity))) + geom_jitter(alpha = 0.3)
##-Useful to interpret end prediction outcomes



###Initial Findings
# The fault_severity and volume reported seems to be highly correlated and looks to be 
# descriptors from resource’s fault event.
#
# This is because each resource type seems to adopt with particular fault events and 
# these faulty events are also sprung from particular log_features. Thus the data tells 
# us which log is has certain circumstances and issues develop fault.
#
# Since each resource\_type has particular locations it often logs issues with, it is 
# under these certain events that a prediction can be made regarding fault\_severity.
#
# There are 4 distinct characters arising from the log features where each has 
# different volume demands at different events. Some resource types require regular 
# attention yet the has little impact on the network.
#
# Meanwhile severity_types are selective depending on the type of resource, with types 
# 1 & 2 yielding higher probability to highly network disruption.


# The trend found is that towards fault\_severity, volume and severity is deterministic 
# to it. Meanwhile with location, particular log\_features describe resource_type

#To determine fault_severity = 2 the following events gives a high chance of outcome:
#1. Location > resource\_type >log\_feature / volume > severity_type (1 or 2)
#or 
#2.   Resource_type = 5

#--------------------------------------------------------------------------------------


