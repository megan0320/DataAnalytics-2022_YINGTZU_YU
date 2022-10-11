days <- c('Mon','Tue','Wed','Thu','Fri')
temp <- c(60,61,64,68,70)
snow <- c(TRUE,FALSE,TRUE,FALSE,FALSE)
help("data.frame")
RPI_WEEK_WEATHER <- data.frame(days,temp,snow)
RPI_WEEK_WEATHER

summary(RPI_WEEK_WEATHER)
str(RPI_WEEK_WEATHER)
summary(RPI_WEEK_WEATHER)
