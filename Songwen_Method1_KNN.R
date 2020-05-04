#This codes are going to use KNN along with LOOCV
#On 2015's data with the classification method 
#"equal to 10 OR not equal to 10"
#by Songwen Xue

#libraries import:
library(ISLR)
library(class)

#optimize the dataset: 
austin_weather[1,] = NULL
austin2015 = rbind(austin_weather[377:596,], austin_weather[600:638,], austin_weather[641:741,]) #only use the data from 2015 and remove the days with invalid data
attach(austin2015)
austin2015 = data.frame(cbind(TempAvgF, DewPointAvgF,
                          HumidityAvgPercent, 
                          SeaLevelPressureAvgInches,
                          VisibilityAvgMiles,WindAvgMPH,
                          PrecipitationSumInches))  #keep all the vars we want


#format the visibility column:
austin2015$VisibilityAvgMiles <- ifelse(austin2015$VisibilityAvgMiles == 10,1,0)

#scale the variable except for the VisibilityAvgMiles:
austin2015[, -c(5)] <- scale(austin2015[, -c(1)])

#keep consistent with the initial value:
set.seed(1)


#using the LOOCV to figure out the optimal 
K.set <- seq(10,201, by=10)
knn.test.err <- numeric(length(K.set))


#austin2015 = data.frame(scale(austin2015))
for (j in 1:length(K.set)){
  knn.cv.pred <- knn.cv(train = austin2015[,-5],
                  cl=austin2015$VisibilityAvgMiles,
                  k=K.set[j])
  knn.test.err[j] <- mean(knn.cv.pred != austin2015$VisibilityAvgMiles)
}

#the mean K value is:
min(knn.test.err)

#the k vale corresponds to it is:
K.set[which.min(knn.test.err)]
