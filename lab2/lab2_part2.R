
#Exercise 1 - Regression
my_data <- read.csv("lab2/dataset_multipleRegression.csv",header = TRUE, fileEncoding = "latin1")
my_data
attach(my_data)
names(my_data)

#create first linear model - Use UNEM and HGRAD to predict ROLL
firstModel <- lm(formula = ROLL ~ UNEM + HGRAD, data = my_data)
firstModel
#define new roll
new <- data.frame(UNEM=c(7), HGRAD=c(90000))
#use the fitted model to predict the roll for UNEM=7% and HGRAD=90,000
newRoll <- predict(firstModel, newdata=new)
newRoll

#create second linear model - Use UNEM, HGRAD and INC to predict ROLL
secondModel <- lm(formula = ROLL ~ UNEM + HGRAD+INC, data = my_data)
secondModel
#define new roll
new <- data.frame(UNEM=c(7), HGRAD=c(90000),INC=c(25000))
#use the fitted model to predict the roll for UNEM=7% and HGRAD=90,000
newRoll <- predict(secondModel, newdata=new)
newRoll

#Exercise 2 - Classification
abData <- read.csv(file.choose(),header = TRUE, fileEncoding = "latin1")
abData
attach(abData)
names(abData)

abData$Rings <- as.numeric(abData$Rings) 
abData$Rings
summary(abData$Rings)
View(abData)
# remove the "sex" variable in abalone, because KNN requires all numeric variables for prediction 
aba <- abData [,-1] 
View(aba)

#  normalize the data using min max normalization 
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) } 
aba_norm <- as.data.frame(lapply(aba[1:7], normalize)) 
summary(aba_norm) 
View(aba_norm)

# now split the data into training and testing sets. 
aba_train <- aba_norm[1:4000,]
aba_test <- aba_norm[4001:4177,]
library(class) 
# start KNN, k=64
KNNpred <- knn(aba_train, aba_test,aba[1:4000,8], k = 64) 
KNNpred 
table(KNNpred,aba[4001:4177,8]) 

# start KNN, k=64
KNNpred <- knn(aba_train, aba_test,aba[1:4000,8], k = 66) 
KNNpred 
table(KNNpred,aba[4001:4177,8]) 

# start KNN, k=64
KNNpred <- knn(aba_train, aba_test,aba[1:4000,8], k = 62) 
KNNpred 
table(KNNpred,aba[4001:4177,8]) 

# Excercise 3

# Loading data
irisData <- iris[,-5]
View(irisData)
iris_result <- kmeans(irisData,3)
iris_result
table(iris$Species,iris_result$cluster)

cent <- list(iris_result$centers)

# iter.max = the maximum number of iterations allowed 
max_iter = 1000

for (i in 2:max_iter){
  tryCatch({
    iris_result <- kmeans(irisData,3)
    done <- TRUE
  }, 
  warning=function(w) {done <- FALSE})
  
  cent[[i]] <- iris_result$centers
  if(done) break
}
cent

