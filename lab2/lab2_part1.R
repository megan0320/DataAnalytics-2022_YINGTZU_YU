EPI_data <- read.csv(file.choose(), header = TRUE)
EPI_data
attach(EPI_data)

names(EPI_data) = EPI_data[1,]
EPI_data = EPI_data[-1,]
EPI_data[] <- lapply(EPI_data, function(x) type.convert(as.character(x)))


#central tendency for EPI
EPI <- EPI_data$EPI
tf <- is.na(EPI)
E <- EPI[!tf]
E
View(E)

Emean <- mean(E)
Emean
EgeometricMean <- prod(E)^(1 / length(E))
EgeometricMean
EharmonicMean <- 1 / mean(1 / E)
EharmonicMean
Emedian <- median(E)
Emedian

#central tendency for DALY
DALY <- EPI_data$DALY
tf <- is.na(DALY)
D <- DALY[!tf]
D

Dmean <- mean(D)
Dmean
DgeometricMean <- prod(D)^(1 / length(D))
DgeometricMean
DharmonicMean <- 1 / mean(1 / D)
DharmonicMean
Dmedian <- median(D)
Dmedian

#Generate the Boxplots for EPI variable
boxplot(E) 
#Generate the Boxplots for DALY variable
boxplot(D)

#Generate Central Tendency values for NOX_pt variable
NOX_pt <- EPI_data$NOX_pt
tf <- is.na(NOX_pt)
N <- NOX_pt[!tf]
N

Nmean <- mean(N)
Nmean
NgeometricMean <- prod(N)^(1 / length(N))
NgeometricMean
NharmonicMean <- 1 / mean(1 / N)
NharmonicMean
Nmedian <- median(N)
Nmedian

#Generate Central Tendency values for SO2_pt variable
SO2_pt <- EPI_data$SO2_pt
tf <- is.na(SO2_pt)
S <- SO2_pt[!tf]
S

Smean <- mean(S)
Smean
SgeometricMean <- prod(S)^(1 / length(S))
SgeometricMean
SharmonicMean <- 1 / mean(1 / S)
SharmonicMean
Smedian <- median(S)
Smedian

#Generate the Boxplots for OZONE_pt variable
OZONE_pt <- EPI_data$OZONE_pt
tf <- is.na(OZONE_pt)
O <- OZONE_pt[!tf]
O

boxplot(O) 

#Generate the Boxplots for WQI_pt variable
WQI_pt <- EPI_data$WQI_pt
tf <- is.na(WQI_pt)
team$team1[team$team1!=""]
W <- WQI_pt[WQI_pt!="" & WQI_pt!=".."] 
W
class(W) <- "numeric"

boxplot(W) 

#Generate Central Tendency values for CLIMATE variable
CLIMATE <- EPI_data$CLIMATE
tf <- is.na(CLIMATE)
C <- CLIMATE[!tf]
C

Cmean <- mean(C)
Cmean
CgeometricMean <- prod(C)^(1 / length(C))
CgeometricMean
CharmonicMean <- 1 / mean(1 / C)
CharmonicMean
Cmedian <- median(C)
Cmedian

#Generate Central Tendency values for AGRICULTUREvariable
AGRICULTUREvariable <- EPI_data$AGRICULTUREvariable
tf <- is.na(AGRICULTUREvariable)
A <- AGRICULTUREvariable[!tf]
A

#Generate the Boxplots for FISHERIES_pt variable
FISHERIES_pt <- EPI_data$FISHERIES_pt
FISHERIES_pt

#Generate the Boxplots for NMVOC_pt variable
NMVOC_pt <- EPI_data$NMVOC_pt
tf <- is.na(NMVOC_pt)
Nm <- NMVOC_pt[!tf]
Nm
boxplot(Nm) 

#boxplot(ENVHEALTH,ECOSYSTEM)
boxplot(EPI_data$ENVHEALTH,EPI_data$ECOSYSTEM)
#qqplot(ENVHEALTH,ECOSYSTEM)
qqplot(EPI_data$ENVHEALTH,EPI_data$ECOSYSTEM)

# Using the EPI (under /EPI on web) dataset find the single most important factor in increasing the EPI in a given region
GEO_subregion <- EPI_data$GEO_subregion
names(EPI_data)

max_relation_index <-17
max_relation_value <-0

EPI_factor <- EPI_data[,14][!is.na(EPI_data[,14])]
EPI_data[,17][!is.na(EPI_data[,17])][GEO_subregion=="Central Asia"]
for (x in 17:23) {
  print(x)

  factor <- EPI_data[,x][!is.na(EPI_data[,x])]
  relation_value <- cor(EPI_factor[GEO_subregion=="Central Asia"],EPI_data[,x][!is.na(EPI_data[,x])][GEO_subregion=="Central Asia"])
  
  if (relation_value > max_relation_value) {
    max_relation_value <- relation_value
    max_relation_index <- x
  }
}
for (x in 25:26) {
  print(x)
  factor <- as.numeric(EPI_data[,x][!is.na(EPI_data[,x])])
  
  factor
  class(factor)
  relation_value <- cor(EPI_factor[GEO_subregion=="Central Asia"],factor[GEO_subregion=="Central Asia"])
  

  if (relation_value > max_relation_value) {
    max_relation_value <- relation_value
    max_relation_index <- x
  }
}
max_relation_index
max_relation_value
colnames(EPI_data)[max_relation_index]


#lmENVH<-lm(ENVHEALTH~DALY+AIR_H+WATER_H

EPI_data <- read.csv("EPI_Data.csv",header = TRUE) 
attach(EPI_data); 

lmENVH <-lm(EPI_data$ENVHEALTH~DALY+EPI_data$AIR_H+EPI_data$WATER_H)
lmENVH
summary(lmENVH)
cENVH<-coef(lmENVH)
cENVH

DALYNEW <-c(seq(5,95,5))
AIR_HNEW <-c(seq(5,95,5))
WATER_HNEW <-c(seq(5,95,5))

NEW <- data.frame(c(seq(5,95,5)),c(seq(5,95,5)),c(seq(5,95,5)))
predict(lmENVH, NEW, se.fit = TRUE)

pENV <- predict(lmENVH, NEW, interval = "prediction")
pENV
cENV <- predict(lmENVH, NEW, interval = "confidence")
cENV


