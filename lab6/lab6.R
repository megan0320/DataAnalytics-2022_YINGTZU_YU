mtcars
dim(mtcars)
head(mtcars)
str(mtcars)
model1 <- lm(mpg ~cyl+wt,data=mtcars)
model1
??plot
# which https://stackoverflow.com/questions/29044055/plot-which-parameter-where-in-r

##
plot(model1,pch=18,col="red",which=c(4))

??outlierTest	
outlierTest(model1)
??cooks.distance
cooksDistance <- cooks.distance(model1)
cooksDistance

round(cooksDistance,5)


library(ISLR)
library(dplyr)

head(Hitters)
dim(Hitters)
is.na(Hitters)
HittersData <- na.omit(Hitters)
HittersData
dim(HittersData)
glimpse(HittersData)

head(HittersData)

SalaryPredictModel1 <- lm(Salary~.,data = HittersData)
summary(SalaryPredictModel1)

plot(SalaryPredictModel1,pch=18,col="red",which=c(4))

cooksD <- cooks.distance(SalaryPredictModel1)
cooksD

influential <- cooksD[(cooksD > (3*mean(cooksD, na.rm=TRUE)))]
influential

names_of_influential <- names(influential)
names_of_influential

outliers <- HittersData[names_of_influential,]
Hitters_Without_Outliers <- HittersData%>% anti_join(outliers)

SalaryPredictModel2 <- lm(Salary~.,data=Hitters_Without_Outliers)
SalaryPredictModel2
plot(SalaryPredictModel2,pch=18,col="red",which=c(4))

summary(SalaryPredictModel2)
