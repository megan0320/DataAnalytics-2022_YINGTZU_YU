my_data <- read.csv(file.choose(),header = TRUE, fileEncoding = "latin1")


library(rpart) 
library(rpart.plot)

iris
dim(iris)

s_iris <- sample(150,100)
s_iris

iris_train <- iris[s_iris,]
iris_train
dim(iris_train)
iris_test <- iris[-s_iris,]
iris_test
dim(iris_test)

dectionTreeModel <- rpart(Species~., iris_train, method="class")
dectionTreeModel
rpart.plot(dectionTreeModel)


