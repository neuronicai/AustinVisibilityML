austin.2015 <- data.frame(austin_weather$TempAvgF, austin_weather$DewPointAvgF, 
                          austin_weather$HumidityAvgPercent, 
                          austin_weather$SeaLevelPressureAvgInches, 
                          austin_weather$WindAvgMPH, 
                          austin_weather$PrecipitationSumInches, 
                          austin_weather$VisibilityAvgMiles)
#Defining only 2015 observations
austin.2015 <- austin.2015[377:741,]

#defining my response variable based on visibility measurements
visibility <- rep(0, 365)
visibility[austin.2015$austin_weather.VisibilityAvgMiles > 5] <- 1
austin.2015 <- data.frame(austin.2015, as.factor(visibility))

#redefining the headings in my data frame and then clearing the original visibility measurements
colnames(austin.2015) <- c("TempAvgF", "DewPointAvgF", "HumidityAvgPercent", "SeaLevelPressureAvgInches",
                           "WindAvgMPH", "PrecipitationSumInches", "VisibilityAvgMiles", "VisibilityFactor")
austin.2015$VisibilityAvgMiles <- NULL

#Cleaning up PrecipitationSumInches, since it is a factor and not numeric
precipitation.numeric <- austin.2015$PrecipitationSumInches
precipitation.numeric[precipitation.numeric == "T"] <- 0
precipitation.numeric <- as.numeric(as.character(precipitation.numeric))
austin.2015$PrecipitationSumInches <- precipitation.numeric

#Clearing out observations with NA values for any variable
austin.2015 <- austin.2015[complete.cases(austin.2015),]

#Now to do KNN
library(class)
#sample a k-fold of 80% into training set
set.seed(1)
data.sample = sample(362, 0.8*362)

#defining training and test sets. y denotes the response variables for each.
x.train <- scale(austin.2015[data.sample,c("TempAvgF", "DewPointAvgF", "HumidityAvgPercent", "SeaLevelPressureAvgInches",
                          "WindAvgMPH", "PrecipitationSumInches")])
y.train <- austin.2015[data.sample,'VisibilityFactor']
x.test <- scale(austin.2015[-data.sample, c("TempAvgF", "DewPointAvgF", "HumidityAvgPercent", "SeaLevelPressureAvgInches",
                                      "WindAvgMPH", "PrecipitationSumInches")])
y.test <- austin.2015[-data.sample, 'VisibilityFactor']


set.seed(1)
knn3.austin <- knn(train = x.train, test = x.test, cl = y.train, k=3)

set.seed(1)
knn5.austin <- knn(train = x.train, test = x.test, cl = y.train, k=5)

set.seed(1)
knn7.austin <- knn(train = x.train, test = x.test, cl = y.train, k=7)

set.seed(1)
knn9.austin <- knn(train = x.train, test = x.test, cl = y.train, k=9)

set.seed(1)
knn11.austin <- knn(train = x.train, test = x.test, cl = y.train, k=11)

test.error <- mean(knn3.austin != y.test)
print(test.error)
test.error <- mean(knn5.austin != y.test)
print(test.error)
test.error <- mean(knn7.austin != y.test)
print(test.error)
test.error <- mean(knn9.austin != y.test)
print(test.error)
test.error <- mean(knn11.austin != y.test)
print(test.error)

random_guess_check <- (length(visibility)-sum(visibility))/length(visibility)
print(random_guess_check)
#if we were simply guessing a single response, how accurate would we be?
#the answer: really close. knn isn't really effective at this.