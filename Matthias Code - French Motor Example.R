################################################################################
# Main reference for this case study is the academic paper:
# "Case Study: French Motor Third-Party Liability Claims"
# by Alexander Noll, Robert Salzmann, Mario V. Wüthrich
################################################################################
# This R-file was written by Matthias Scherer; 
# partially using listings from the above paper
################################################################################


################################################################################
# 0. Installing resources
################################################################################

################################################################################
# Required packages
################################################################################
################################################################################
# Remark: Source from paper does not work, i.e.
# install.packages("CASdatasets",repos="http://dutangc.free.fr/pub/RRepos/", type="source")
################################################################################

# install.packages("xts")
# install.packages("sp")
# install.packages("zoo")
# install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")
# install.packages("rpart")
# install.packages("rpart.plot")

library(rpart)
library(rpart.plot)
library(zoo)
library(xts)
library(CASdatasets)
data(freMTPL2freq)
library(tidyverse)


################################################################################
# Store original data (odata)
# and create cleaned data (cdata) for later use
################################################################################

# original dataset stored in "odata"
odata<-freMTPL2freq 

# we later clean it and create "cdata"
cdata<-odata

################################################################################
# 1. Exploring the data
################################################################################

str(odata)
n <- nrow(odata)

################################################################################
# Explanation
################################################################################
# 1. IDpol: policy number ( unique identifier -> not relevant for us)
# 2. ClaimNb: number of claims for this policy (integer valued)
# 3. Exposure: total exposure in fractions of a year 
# 4. Area: area code (categorical, ordinal)
# 5. VehPower: power of the car (categorical, ordinal)
# 6. VehAge: age of the car in years
# 7. DrivAge: age of the (most common) driver in years
# 8. BonusMalus: bonus-malus level between 50 and 230 (with reference level 100, smaler = better)
# 9. VehBrand: car brand (categorical, nominal)
# 10. VehGas: "diesel" or "regular" (binary)
# 11. Density: density of inhabitants per km^2 in the city of the driver
# 12. Region: regions in France (categorical)
################################################################################

################################################################################
# 1.1 Exploring the (co-)variables
################################################################################

################################################################################
# 1.1.1 ClaimNb
################################################################################

ClaimNb <- odata$ClaimNb
summary(ClaimNb)
min(ClaimNb)
mean(ClaimNb)
max(ClaimNb)

par(mfrow=c(1,3))
boxplot(ClaimNb)
hist(ClaimNb,main="Number of claims",breaks = 16,xlab="")
hist(log(ClaimNb),main="(log-) Number of claims",breaks = 16,xlab="")

# What, if we assume (disregarding Exposure!) that ClaimNb ~ Poi(lambda) ?
# We can estimate lambda via ML from ...

sum(ClaimNb)/length(ClaimNb)

######################
# Cleaning unrealistic
# number of claims
######################

for (i in (1:n))
{
  if (odata$ClaimNb[i]>4)
  {
    cdata$ClaimNb[i]<-4
  }
}

mean(odata$ClaimNb)
mean(cdata$ClaimNb)
ClaimNb <- cdata$ClaimNb

################################################################################
# 1.1.2 Exposure
################################################################################

Exposure <- odata$Exposure

min(Exposure)
min(Exposure)*366

par(mfrow=c(1,2))
hist(Exposure*366)
boxplot(Exposure)

######################
# Cleaning unrealistic
# exposures (i.e. > 1)
# sum(odata$Exposure>1)
######################

for (i in (1:n))
{
  if (odata$Exposure[i]>1)
  {
    cdata$Exposure[i]<-1
  }
}

mean(odata$Exposure)
mean(cdata$Exposure)
Exposure <- cdata$Exposure
max(Exposure)

par(mfrow=c(1,2))
hist(Exposure*366)
boxplot(Exposure)


totalExposure <-  (0:4)*0
numberPolicies <- (0:4)*0
for (i in (1:n))
{
  dummy <- (ClaimNb[i]+1)           
  totalExposure[dummy] <- totalExposure[dummy] + Exposure[i]
  numberPolicies[dummy] <- numberPolicies[dummy] + 1 
}


################################################################################
# 1.1.2.b Frequency
################################################################################

Frequency <- sum(ClaimNb)/sum(Exposure)

######################
# Portfolio frequency
######################

sum(ClaimNb)/sum(totalExposure)

######################
# Frequency of sub-portfolios
######################

######################
# Splitting by Area 
# Using the "split"-command
######################

AreaSplit <- split(cdata, f = cdata$Area)
AreaFreq<-c(
  sum(AreaSplit$A$ClaimNb)/sum(AreaSplit$A$Exposure),
  sum(AreaSplit$B$ClaimNb)/sum(AreaSplit$B$Exposure),
  sum(AreaSplit$C$ClaimNb)/sum(AreaSplit$C$Exposure),
  sum(AreaSplit$D$ClaimNb)/sum(AreaSplit$D$Exposure),
  sum(AreaSplit$E$ClaimNb)/sum(AreaSplit$E$Exposure),
  sum(AreaSplit$F$ClaimNb)/sum(AreaSplit$F$Exposure))
par(mfrow=c(1,1))
plot((1:6),AreaFreq,ylim=c(0,0.2),main="Frequency for different areas",xlab="Area")
cbind(levels(cdata$Area))

######################
# Splitting by BonusMalus
# running over all groups
######################
hist(cdata$BonusMalus)

lb<-cbind(50, 60, 70, 80, 90, 100)
up<-cbind(60, 70, 80, 90, 100, max(cdata$BonusMalus)+1)
m<-length(lb)
ClaimNbBM <- (1:m)*0
ExposureBM <- (1:m)*0
FreqBM <- (1:m)*0

for (i in (1:n))
{
  cv <- cdata$BonusMalus[i]
  for (j in (1:m))
  {
    if ((cv>=lb[j])&&(cv<up[j]))
    {
      ClaimNbBM[j] <- ClaimNbBM[j] + cdata$ClaimNb[i]
      ExposureBM[j] <- ExposureBM[j] + cdata$Exposure[i]
    }
  }
}

for (j in (1:m))
{
  FreqBM[j]<-sum(ClaimNbBM[j])/sum(ExposureBM[j])
}
plot(FreqBM,main="Frequency per BM-class",ylim=c(0,0.35))


######################
# Splitting by VehGas
######################
hist(ClaimNb[cdata$VehGas=="Diesel"])
hist(ClaimNb[cdata$VehGas=="Regular"])

sum(ClaimNb[cdata$VehGas=="Diesel"])/sum(Exposure[cdata$VehGas=="Diesel"])
sum(ClaimNb[cdata$VehGas=="Regular"])/sum(Exposure[cdata$VehGas=="Regular"])


######################
# Splitting by Age of driver
# using "tidyverse"
######################

DrivAgeFreq <- cdata |>
  group_by(DrivAge) |>
  summarise(DrivAgeFreq = sum(ClaimNb) / sum(Exposure), .groups = "drop") 
plot(DrivAgeFreq,type="l",main="Frequency by age of driver")

######################
# Splitting by Age of car
# using "tidyverse"
######################

VehAgeFreq <- cdata |>
  group_by(VehAge) |>
  summarise(VehAgeFreq = sum(ClaimNb) / sum(Exposure), .groups = "drop") 
plot(VehAgeFreq,type="l",main="Frequency by age of car")


################################################################################
# 2. Learning / Testing data split
################################################################################

n <- nrow(cdata)
set.seed(100)
subsetlearn <- sample(c(1:n), round(0.9*n), replace=FALSE)
learn <- cdata[subsetlearn,]
test <- cdata[-subsetlearn,]

######################
# tests if done correctly
######################
n-(nrow(learn)+nrow(test))
nrow(learn)/n
nrow(test)/n

################################################################################
# 3. GLM
################################################################################

learn$AreaGLM <- as.integer(learn$Area)
learn$VehPowerGLM <- as.factor(pmin(learn$VehPower,9))
VehAgeGLM <- cbind( c(0:110), c(1, rep(2,10), rep(3,100)))
learn$VehAgeGLM <- as.factor(VehAgeGLM[learn$VehAge+1,2])
learn[ ,"VehAgeGLM"] <- relevel(learn[ ,"VehAgeGLM"], ref="2")
DrivAgeGLM <- cbind( c(18:100), c(rep(1,21-18), rep(2,26-21), rep(3,31-26), rep(4,41-31), rep(5,51-41), rep(6,71-51), rep(7,101-71)))
learn$DrivAgeGLM <- as.factor(DrivAgeGLM[ learn$DrivAge -17 ,2])
learn[ ,"DrivAgeGLM"] <- relevel(learn[ ,"DrivAgeGLM"], ref="5")
learn$BonusMalusGLM <- as.integer(pmin(learn$BonusMalus, 150))
learn$DensityGLM <- as.numeric(log(learn$Density))
learn[,"Region"] <- relevel(learn[,"Region"],ref="Centre")

ClaimNb         <- learn$ClaimNb
VehPowerGLM     <- learn$VehPowerGLM
VehAgeGLM       <- learn$VehAgeGLM
DrivAgeGLM      <- learn$DrivAgeGLM 
BonusMalusGLM   <- learn$BonusMalusGLM 
VehBrand        <- learn$VehBrand
VehGas          <- learn$VehGas
DensityGLM      <- learn$DensityGLM 
Region          <- learn$Region
AreaGLM         <- learn$AreaGLM

glm<-glm(formula=ClaimNb ~ VehPowerGLM + VehAgeGLM + DrivAgeGLM + BonusMalusGLM + VehBrand + VehGas + DensityGLM + Region + AreaGLM , family=poisson(), data = learn, offset = log( Exposure ))
learn$fit <- fitted(glm)

test$AreaGLM <- as.integer(test$Area)
test$VehPowerGLM <- as.factor(pmin(test$VehPower,9))
VehAgeGLM <- cbind( c(0:110), c(1, rep(2,10), rep(3,100)))
test$VehAgeGLM <- as.factor( VehAgeGLM[test$VehAge+1,2])
test[ ,"VehAgeGLM"] <- relevel(test[ ,"VehAgeGLM"], ref="2")
DrivAgeGLM <- cbind( c(18:100), c(rep(1,21-18), rep(2,26-21), rep(3,31-26), rep(4,41-31), rep(5,51-41), rep(6,71-51), rep(7,101-71)))
test$DrivAgeGLM <- as.factor(DrivAgeGLM[ test$DrivAge -17 ,2])
test[ ,"DrivAgeGLM"] <- relevel(test[ ,"DrivAgeGLM"], ref="5")
test$BonusMalusGLM <- as.integer( pmin(test$BonusMalus, 150))
test$DensityGLM <- as.numeric(log(test$Density))
test[,"Region"] <- relevel(test[,"Region"],ref="Centre")

test$fit <- predict(glm, newdata=test, type="response")

######################
# In-Sample errors
######################

in_sample <- 2*( sum( learn$fit ) - sum( learn$ClaimNb ) + sum( log(( learn$ClaimNb / learn$fit )^(learn$ClaimNb))))
average_in_sample <- in_sample / nrow(learn)

######################
# Out-of-Sample errors
######################

out_of_sample <- 2*( sum( test$fit ) - sum( test$ClaimNb ) + sum( log(( test$ClaimNb / test$fit )^(test$ClaimNb))))
average_out_of_sample <- out_of_sample / nrow(test)


################################################################################
# 4. Tree
################################################################################

Exposure <- learn$Exposure
ClaimNb <- learn$ClaimNb
Area <- learn$Area
VehPower <- learn$VehPower
VehAge <- learn$VehAge
DrivAge <- learn$DrivAge
BonusMalus <- learn$BonusMalus
VehBrand <- learn$VehBrand
VehGas <- learn$VehGas
Density <- learn$Density
Region <- learn$Region
tree1 <- rpart(cbind(Exposure, ClaimNb) ~ Area + VehPower + VehAge + DrivAge + BonusMalus + VehBrand + VehGas + Density + Region, data = learn , method="poisson", control=rpart.control(xval=1, minbucket=10000, cp=0.0005))
# rpart.plot( tree1 )


######################
# In-Sample errors
######################

learn$fit <- predict(tree1, learn, type = 'vector')
in_sample <- 2*( sum( learn$fit ) - sum( learn$ClaimNb ) + sum( log(( learn$ClaimNb / learn$fit )^(learn$ClaimNb))))
average_in_sample <- in_sample / nrow(learn)

average_in_sample

######################
# Out-of-Sample errors
######################

test$fit <- predict(tree1, test, type = 'vector')

out_of_sample <- 2*( sum( test$fit ) - sum( test$ClaimNb ) + sum( log(( test$ClaimNb / test$fit )^(test$ClaimNb))))
average_out_of_sample <- out_of_sample / nrow(test)
average_out_of_sample 
