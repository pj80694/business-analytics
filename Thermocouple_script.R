#=======================================================================

# Rattle is Copyright (c) 2006-2018 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2019-05-08 12:00:25 x86_64-w64-mingw32 

# Rattle version 5.2.0 user 'User'

# This log captures interactions with Rattle as an R script. 

# For repeatability, export this activity log to a 
# file, like 'model.R' using the Export button or 
# through the Tools menu. Th script can then serve as a 
# starting point for developing your own scripts. 
# After xporting to a file called 'model.R', for exmample, 
# you can type into a new R Console the command 
# "source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 
 
# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access the weather dataset and utilities.
library(magrittr) # Utilise %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 42 

#=======================================================================
# Rattle timestamp: 2019-05-08 12:00:54 x86_64-w64-mingw32 

# Load a dataset from file.

fname         <- "file:///C:/Users/User/Desktop/svyasa/R/Datasets/Thermocouple.csv" 
crs$dataset <- read.csv(fname,
			na.strings=c(".", "NA", "", "?"),
			strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2019-05-08 12:00:55 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=24 train=17 validate=4 test=3

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("X.U.FEFF.Temp....C.", "Voltage..mV.")

crs$numeric   <- c("X.U.FEFF.Temp....C.", "Voltage..mV.")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 12:01:32 x86_64-w64-mingw32 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=24 train=17 validate=4 test=3

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- "Voltage..mV."

crs$numeric   <- "Voltage..mV."

crs$categoric <- NULL

crs$target    <- "X.U.FEFF.Temp....C."
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-05-08 12:01:36 x86_64-w64-mingw32 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

# The 'Hmisc' package provides the 'describe' function.

library(Hmisc, quietly=TRUE)

# Generate a description of the dataset.

describe(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

# The 'kurtosis' package provides the 'fBasics' function.

library(fBasics, quietly=TRUE)

# Summarise the kurtosis of the numeric data.

kurtosis(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)][,c(1:2)], na.rm=TRUE)

# The 'mice' package provides the 'md.pattern' function.

library(mice, quietly=TRUE)

# Generate a summary of the missing values in the dataset.

md.pattern(crs$dataset[,c(crs$input, crs$target)])

#=======================================================================
# Rattle timestamp: 2019-05-08 12:03:20 x86_64-w64-mingw32 

# Perform Test 

# Use the fBasics package for statistical tests.

library(fBasics, quietly=TRUE)

# Perform the test.

correlationTest(na.omit(crs$dataset[, "Voltage..mV."]), na.omit(crs$dataset[, "Voltage..mV."]))

#=======================================================================
# Rattle timestamp: 2019-05-08 12:04:13 x86_64-w64-mingw32 

# Perform Test 

# Use the fBasics package for statistical tests.

library(fBasics, quietly=TRUE)

# Perform the test.

locationTest(na.omit(crs$dataset[crs$dataset[["X.U.FEFF.Temp....C."]] == "10", "Voltage..mV."]), na.omit(crs$dataset[crs$dataset[["X.U.FEFF.Temp....C."]] == "20", "Voltage..mV."]))

#=======================================================================
# Rattle timestamp: 2019-05-08 12:04:41 x86_64-w64-mingw32 

# Perform Test 

# Use the fBasics package for statistical tests.

library(fBasics, quietly=TRUE)

# Perform the test.

varianceTest(na.omit(crs$dataset[crs$dataset[["X.U.FEFF.Temp....C."]] == "10", "Voltage..mV."]), na.omit(crs$dataset[crs$dataset[["X.U.FEFF.Temp....C."]] == "20", "Voltage..mV."]))

#=======================================================================
# Rattle timestamp: 2019-05-08 12:05:58 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(X.U.FEFF.Temp....C. ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs

#=======================================================================
# Rattle timestamp: 2019-05-08 12:06:10 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.

# Rattle provides evaluateRisk() and riskchart().

crs$pr <- predict(crs$glm, 
   type    = "response",
   newdata = crs$dataset[crs$validate, c(crs$input, crs$target)])

crs$eval <- evaluateRisk(crs$pr, crs$dataset[crs$validate, c(crs$input, crs$target)]$X.U.FEFF.Temp....C.)
print(riskchart(crs$pr, 
    crs$dataset[crs$validate, c(crs$input, crs$target)]$X.U.FEFF.Temp....C., 
    title="Performance Chart Linear Thermocouple.csv [validate] ", show.lift=FALSE, show.precision=FALSE, legend.horiz=FALSE))

