# Or, if .csv file, use this
my_data <- read.csv(file.choose(),header = TRUE, fileEncoding = "latin1")
my_num_data <-my_data[,c(141:150)]
my_num_data
class(my_num_data)
my_num_data[is.na(my_num_data)] <- 0
df2 <- data.frame(apply(my_num_data, 2, function(x) as.numeric(as.character(x))))
df2
class(df2)
sapply(df2, is.numeric)
res <- cor(df2)
round(res, 2)

install.packages("corrplot")
library(corrplot)
corrplot(res, method="number")

res['EPI']
