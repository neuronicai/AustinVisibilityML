## classifying using method 1 - austin2015$VisibilityAvgMiles <- ifelse(austin2015$VisibilityAvgMiles == 10,1,0)
## using SVM 
##Janice Nguyen and Michael Saenz 

set.seed(1)
weather_raw <- read.csv("~/Desktop//austin_weather1.csv")

weather_raw$Date <- as.Date(weather_raw$Date)

weather_2015 <- weather_raw[(weather_raw$Date > "2014-12-31" & weather_raw$Date < "2016-01-01"), ]

keep_attr <- c("TempAvgF", "DewPointAvgF", "HumidityAvgPercent", "SeaLevelPressureAvgInches", 
               "VisibilityAvgMiles", "WindAvgMPH", "PrecipitationSumInches")

weather <- weather_2015[keep_attr]

weather[weather == '-'] <- NA

sum(is.na(weather))

weather <- na.omit(weather)

weather[weather == 'T'] <- 0

set.seed(1)

weather$VisibilityAvgMiles <- ifelse(weather$VisibilityAvgMiles == 10,1,0)
weather$VisibilityAvgMiles <- as.factor(weather$VisibilityAvgMiles)

set.seed(1)
n <- nrow(weather)
train <- sample(1:n, 0.8*n)
x.train <- weather[train,]
y.train <- weather[train, "VisibilityAvgMiles"]
x.test <- weather[-train,]
y.test <- weather[-train, "VisibilityAvgMiles"]


library(e1071)
set.seed(1)
tune.out <- tune(svm,
                VisibilityAvgMiles~.,
                 data = x.train,
                 kernel = "linear",
                 ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100)))
print(tune.out)

set.seed(1)
best.mod.pred <- predict(tune.out$best.model)
mean(best.mod.pred != y.train) #training error
mean(best.mod.pred != y.test) #test error

set.seed(1)
tune.out.poly <- tune(svm,
                 VisibilityAvgMiles~.,
                 data = x.train,
                 kernel = "polynomial",
                 ranges = list(cost=c(0.001,0.1,1,5,10,100),
                               degree=c(2,3)))
print(tune.out.poly)

set.seed(1)
best.mod.pred.poly <- predict(tune.out.poly$best.model)
mean(best.mod.pred.poly != y.train) #training error
mean(best.mod.pred.poly != y.test) #test error

set.seed(1)
tune.out.radial <- tune(svm,
                 VisibilityAvgMiles~.,
                 data = x.train,
                 kernel = "radial",
                 ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000),
                               gamma=c(1e-4,1e-3,1e-2,1e-1,1,10)))
print(tune.out.radial)

set.seed(1)
best.mod.pred.rad <- predict(tune.out.radial$best.model)
mean(best.mod.pred.rad != y.train) #training error
mean(best.mod.pred.rad != y.test) #test error
