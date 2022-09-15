EPI_data <- read.csv(file.choose(),header = T)#import 2010_EPI_data
EPI_data
names(EPI_data)
attach(EPI_data)

#remove Null value
tf <- is.na(EPI)
E <- EPI[!tf]
E

#exercise1 for EPI
summary(EPI)
fivenum(EPI)
fivenum(EPI,na.rm = T)
stem(EPI)
hist(EPI)
hist(EPI,seq(30.,95.,1.0),prob=T)
lines(density(EPI,na.rm = T,bw=1))
rug(EPI)

#compare EPI, ENVHEALTH, ECOSYSTEM, DALY, AIR_H, WATER_H, AIR_E, WATER_E, BIODIVERSITY
summary(ENVHEALTH)
fivenum(ENVHEALTH)
fivenum(ENVHEALTH,na.rm = T)
stem(ENVHEALTH)
hist(ENVHEALTH)
hist(ENVHEALTH,seq(30.,95.,1.0),prob=T)
lines(density(ENVHEALTH,na.rm = T,bw=1))
rug(ENVHEALTH)
boxplot(EPI, ENVHEALTH)
qqplot(EPI, ENVHEALTH)

summary(ECOSYSTEM)
fivenum(ECOSYSTEM)
fivenum(ECOSYSTEM,na.rm = T)
stem(ECOSYSTEM)
hist(ECOSYSTEM)
hist(ECOSYSTEM,seq(30.,95.,1.0),prob=T)
lines(density(ECOSYSTEM,na.rm = T,bw=1))
rug(ECOSYSTEM)
boxplot(EPI, ECOSYSTEM)
qqplot(EPI, ECOSYSTEM)

summary(DALY)
fivenum(DALY)
fivenum(DALY,na.rm = T)
stem(DALY)
hist(DALY)
hist(DALY,seq(30.,95.,1.0),prob=T)
lines(density(DALY,na.rm = T,bw=1))
rug(DALY)
boxplot(EPI, DALY)
qqplot(EPI, DALY)

summary(AIR_H)
fivenum(AIR_H)
fivenum(AIR_H,na.rm = T)
stem(AIR_H)
hist(AIR_H)
hist(AIR_H,seq(30.,95.,1.0),prob=T)
lines(density(AIR_H,na.rm = T,bw=1))
rug(AIR_H)
boxplot(EPI, AIR_H)
qqplot(EPI, AIR_H)

summary(WATER_H)
fivenum(WATER_H)
fivenum(WATER_H,na.rm = T)
stem(WATER_H)
hist(WATER_H)
hist(WATER_H,seq(30.,95.,1.0),prob=T)
lines(density(WATER_H,na.rm = T,bw=1))
rug(WATER_H)
boxplot(EPI, WATER_H)
qqplot(EPI, WATER_H)

summary(AIR_E)
fivenum(AIR_E)
fivenum(AIR_E,na.rm = T)
stem(AIR_E)
hist(AIR_E)
hist(AIR_E,seq(30.,80.,1.0),prob=T)
lines(density(AIR_E,na.rm = T,bw=1))
rug(AIR_E)
boxplot(EPI, AIR_E)
qqplot(EPI, AIR_E)

summary(WATER_E)
fivenum(WATER_E)
fivenum(WATER_E,na.rm = T)
stem(WATER_E)
hist(WATER_E)
hist(WATER_E,seq(30.,95.,1.0),prob=T)
lines(density(WATER_E,na.rm = T,bw=1))
rug(WATER_E)
boxplot(EPI, WATER_E)
qqplot(EPI, WATER_E)

summary(BIODIVERSITY)
fivenum(BIODIVERSITY)
fivenum(BIODIVERSITY,na.rm = T)
stem(BIODIVERSITY)
hist(BIODIVERSITY)
hist(BIODIVERSITY,seq(30.,95.,1.0),prob=T)
lines(density(BIODIVERSITY,na.rm = T,bw=1))
rug(BIODIVERSITY)
boxplot(EPI, BIODIVERSITY)

qqplot(EPI, BIODIVERSITY)

#exercise2 for EPI
Landlock
EPILand <-EPI[!Landlock]
ELand <- EPILand[!is.na(EPILand)]
ELand
fivenum(Eland)
stem(ELand)
hist(ELand)
hist(ELand,seq(30.,95.,1.0),prob=T)
lines(density(ELand,na.rm = T,bw=1))
rug(ELand)

#Exercise 2 filtering by geo_subregion
GEO_subregion
EPI[GEO_subregion=="South America"]
EPI[GEO_subregion=="Central Asia"]
EPI[GEO_subregion=="Western Europe"]
