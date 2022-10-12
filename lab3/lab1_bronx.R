library(gdata) 
#alternate
bronx1<-read.xls("C:\\Apps\\rollingsales_bronx.xls",pattern="BOROUGH",verbose=FALSE, stringsAsFactors=FALSE,sheet=1,header=TRUE,perl="C:\\Strawberry\\perl\\bin\\perl.exe")
bronx1<-bronx1[which(bronx1$GROSS.SQUARE.FEET!="0" & bronx1$LAND.SQUARE.FEET!="0" & bronx1$SALE.PRICE!="$0"),]
View(bronx1)
#
attach(bronx1) # If you choose to attach, leave out the "data=." in lm regression

SALE.PRICE
SALE.PRICE<-gsub(",","", SALE.PRICE)
SALE.PRICE
SALE.PRICE<-sub("\\$","",SALE.PRICE) 
SALE.PRICE<-as.numeric(SALE.PRICE)

#SALE.PRICE <- na.omit(SALE.PRICE) # delete rows with NA
GROSS.SQUARE.FEET<-as.numeric(gsub(",","", GROSS.SQUARE.FEET)) 
LAND.SQUARE.FEET<-as.numeric(gsub(",","", LAND.SQUARE.FEET)) 
LAND.SQUARE.FEET
plot(log(GROSS.SQUARE.FEET), log(SALE.PRICE)) 

GROSS.SQUARE.FEET
m1<-lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET))

summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))

# Model 2
#bronx1$LAND.SQUARE.FEET <-as.numeric(bronx1$LAND.SQUARE.FEET)
LAND.SQUARE.FEET
bronx1$NEIGHBORHOOD
m2<-lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(NEIGHBORHOOD))
summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(log(SALE.PRICE)~0+log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

# Model 3
m3<-lm(log(SALE.PRICE)~0+log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(NEIGHBORHOOD)+factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

# Model 4
m4<-lm(log(SALE.PRICE)~0+log(GROSS.SQUARE.FEET)+log(LAND.SQUARE.FEET)+factor(NEIGHBORHOOD)*factor(BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))
#
