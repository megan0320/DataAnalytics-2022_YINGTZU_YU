EPI_data <- read.csv(file.choose(),header = T)
View(EPI_data)
EPI_data
fix(EPI_data)#not work
summary(EPI_data)
fivenum(EPI_data,na.rm=T)
names(EPI_data)

attach(EPI_data)
EPI
tf <- is.na(EPI)
tf
E <- EPI[!tf]
E
summary(EPI)
fivenum(EPI)
fivenum(EPI,na.rm = T)
fivenum(EPI,na.rm = F)
stem(EPI)
hist(EPI)#figure margins too large

hist(EPI,seq(30.,95.,1.0),prob=T)
lines(density(EPI,na.rm = T,bw=1))
rug(EPI)

help(ecdf)
plot(ecdf(EPI), do.points=F, verticals=T) 
par(pty="s") 
qqnorm(EPI)
qqline(EPI) 
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df= 5), x, xlab= "Q-Q plot for t dsn") 
qqline(x)        

plot(ecdf(DALY), do.points=F, verticals=T) 
par(pty="s") 
qqnorm(DALY)
qqline(DALY) 

plot(ecdf(WATER_H), do.points=F, verticals=T) 
par(pty="s") 
qqnorm(WATER_H)
qqline(WATER_H) 


boxplot(EPI,DALY)
qqplot(EPI,DALY)

#EPI land
EPILand<-EPI[!Landlock] 
Eland <- EPILand[!is.na(EPILand)] 
Eland
hist(Eland) 
hist(Eland, seq(30., 95., 1.0), prob=TRUE) 
