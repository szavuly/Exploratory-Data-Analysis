# LIBRARIES
library(arm)
library(readr)
library(dplyr)
library(ggplot2)
library(pastecs)
library(DataCombine)
library(descr)
library(fBasics)
library(stargazer)
library(sandwich)
library(lmtest)
library(splines)
library(readr)
library(gmodels)
library(mfx)
library(descr)

# CLEAR MEMORY
rm(list = ls())

# SET WORKING DIRECTORY
setwd("C:\\Users\\kszavuly\\OneDrive\\CEU\\ECON 5008 - Data Analysis for Economic Policy 2 20162017 Fall\\Term project")
getwd()

# LOAD  DATA
dataset <- read.csv("924f1225-e93e-4778-b465-f2578c2eef17_Data.csv")

# DATA TRANSFORMATIONS
names(dataset)[names(dataset) == "ď..Country.Name"] <- "country"
names(dataset)[names(dataset) == "Country.Code"] <- "code"
names(dataset)[names(dataset) == "Series.Name"] <- "series"
names(dataset)[names(dataset) == "X2015..YR2015."] <- "2015"
dataset[dataset == ".."] = NA

# SUBSETTING AND COMBINING SERIES
internet <- subset(dataset, select = c("country", "2015"), series == "Internet users (per 100 people)")
names(internet)[names(internet) == "2015"] <- "internet"
internet$internet <- as.numeric(as.character(internet$internet))
landline <- subset(dataset, select = c("country", "2015"), series == "Fixed telephone subscriptions (per 100 people)")
names(landline)[names(landline) == "2015"] <- "landline"
landline$landline <- as.numeric(as.character(landline$landline))
mobile <- subset(dataset, select = c("country", "2015"), series == "Mobile cellular subscriptions (per 100 people)")
names(mobile)[names(mobile) == "2015"] <- "mobile"
mobile$mobile <- as.numeric(as.character(mobile$mobile))
gdppc <- subset(dataset, select = c("country", "2015"), series == "GDP per capita (constant 2010 US$)")
names(gdppc)[names(gdppc) == "2015"] <- "gdppc"
gdppc$gdppc <- as.numeric(as.character(gdppc$gdppc))
gdppc$lngdppc <- log(gdppc$gdppc)
telco <- merge(gdppc,internet,by="country")
telco <- merge(telco,landline,by="country")
telco <- merge(telco,mobile,by="country")
telco$gdppc <- NULL

# SUMMARY AND FINALIZATION OF THE DATASET
summary(telco)
telco <- telco[complete.cases(telco),]
summary(telco)

# HISTOGRAMS
ggplot(telco, aes(x = lngdppc)) + geom_histogram(stat = "bin", binwidth=0.5, fill="deepskyblue3", colour="deepskyblue3") +labs(x="Binwidth: 0.5", y="Density of lngdppc")
ggplot(telco, aes(x = internet)) + geom_histogram(stat = "bin", binwidth=10, fill="deepskyblue3", colour="deepskyblue3") +labs(x="Binwidth: 10", y="Density of internet users / 100 people")
ggplot(telco, aes(x = landline)) + geom_histogram(stat = "bin", binwidth=10, fill="deepskyblue3", colour="deepskyblue3") +labs(x="Binwidth: 10", y="Density of landline users / 100 people")
ggplot(telco, aes(x = mobile)) + geom_histogram(stat = "bin", binwidth=10, fill="deepskyblue3", colour="deepskyblue3") +labs(x="Binwidth: 10", y="Density of mobile users / 100 people")

# SCATTERPLOTS
ggplot(data = telco, aes(x=lngdppc, y=internet)) + geom_point(size=1.5, colour="lightsalmon2")
ggplot(data = telco, aes(x=lngdppc, y=landline)) + geom_point(size=1.5, colour="lightsalmon2")
ggplot(data = telco, aes(x=lngdppc, y=mobile)) + geom_point(size=1.5, colour="lightsalmon2")

# PLOTTING REGRESSIONS ON INTERNET
ggplot(data = telco, aes(x=lngdppc, y=internet)) +
  geom_point(size=1.5, colour="lightsalmon2") +
  geom_smooth(method="loess", colour="seagreen3", se=T) 
ggplot(data = telco, aes(x=lngdppc, y=internet)) +
  geom_point(size=1.5, colour="lightsalmon2") +
  geom_smooth(method="lm", colour="seagreen3", se=T) 
ggplot(data = telco, aes(x=lngdppc, y=internet)) +
  geom_point(size=1.5, colour="lightsalmon2") +
  geom_smooth(method="lm", formula=y~poly(x,2,raw = TRUE), colour="seagreen3", se=T) 
ggplot(telco) +
  geom_smooth(aes(lngdppc, internet), method = "loess", colour = "deepskyblue3", se=F) +
  annotate("text", x = 7, y = 90, label = "loess", colour = "deepskyblue3") +
  geom_smooth(aes(lngdppc, internet), method = "lm", colour = "lightsalmon2", se=F) +
  annotate("text", x = 7, y = 80, label = "lm", colour = "lightsalmon2") +
  geom_smooth(aes(lngdppc, internet), method = "lm", formula=y~poly(x,2,raw = TRUE), colour = "seagreen3", se=F) +
  annotate("text", x = 7, y = 70, label = "quadratic", colour = "seagreen3")

# REGRESSIONS ON INTERNET WITH CONTROLS
reg1 <- lm(internet ~ lngdppc, data = telco)
reg2 <- lm(internet ~ lngdppc + landline, data = telco)
reg3 <- lm(internet ~ lngdppc + mobile, data = telco)
reg4 <- lm(internet ~ lngdppc + landline + mobile, data = telco)
stargazer(list(reg1, reg2, reg3, reg4), digits = 2, out = "regressions_internet_1.html")

# RESIDUALS
telco$residreg1 <- resid(reg1)
ggplot(telco, aes(x = residreg1)) + geom_histogram(stat = "bin", binwidth=10, fill="deepskyblue3", colour="deepskyblue3") +labs(x="Binwidth: 10", y="Density of residuals")
summary(telco$residreg1)
basicStats(telco$residreg1)

# EXPLORING INTERNET, LANDLINE AND MOBILE
ggplot(telco) +
  geom_smooth(aes(lngdppc, internet), method = "loess", colour = "deepskyblue3", se=T) +
  annotate("text", x = 6, y = 150, label = "internet", colour = "deepskyblue3") +
  geom_smooth(aes(lngdppc, landline), method = "loess", colour = "lightsalmon2", se=T) +
  annotate("text", x = 6, y = 140, label = "landline", colour = "lightsalmon2") +
  geom_smooth(aes(lngdppc, mobile), method = "loess", colour = "seagreen3", se=T) +
  annotate("text", x = 6, y = 130, label = "mobile", colour = "seagreen3")
ggplot(telco) +
  geom_smooth(aes(lngdppc, internet), method = "lm", colour = "deepskyblue3", se=T) +
  annotate("text", x = 6, y = 150, label = "internet", colour = "deepskyblue3") +
  geom_smooth(aes(lngdppc, landline), method = "lm", colour = "lightsalmon2", se=T) +
  annotate("text", x = 6, y = 140, label = "landline", colour = "lightsalmon2") +
  geom_smooth(aes(lngdppc, mobile), method = "lm", colour = "seagreen3", se=T) +
  annotate("text", x = 6, y = 130, label = "mobile", colour = "seagreen3")

# EXPLORING LANDLINE CORRELATIONS
ggplot(data = telco, aes(x=lngdppc, y=landline)) +
  geom_point(size=1.5, colour="lightsalmon2") +
  geom_smooth(method="loess", colour="seagreen3", se=T) 
regland1 <- lm(landline ~ lngdppc, data = telco)
telco$residregland1 <- resid(regland1)
ggplot(telco, aes(x = residregland1)) + geom_histogram(stat = "bin", binwidth=10, fill="deepskyblue3", colour="deepskyblue3") +labs(x="Binwidth: 10", y="Density of residuals (landline)")
summary(telco$residregland1)
basicStats(telco$residregland1)
stargazer(list(regland1), digits = 2, out = "regressions_landline_1.html")

# EXPLORING MOBILE CORRELATIONS
ggplot(data = telco, aes(x=lngdppc, y=mobile)) +
  geom_point(size=1.5, colour="lightsalmon2") +
  geom_smooth(method="loess", colour="seagreen3", se=T) 
regmob1 <- lm(mobile ~ lngdppc, data = telco)
telco$residregmob1 <- resid(regmob1)
ggplot(telco, aes(x = residregmob1)) + geom_histogram(stat = "bin", binwidth=10, fill="deepskyblue3", colour="deepskyblue3") +labs(x="Binwidth: 10", y="Density of residuals (mobile)")
summary(telco$residregmob1)
basicStats(telco$residregmob1)
stargazer(list(reg1, reg3, regmob1), digits = 2, out = "regressions_mobile_1.html")

# INTERNET INVESTMENTS
names(dataset)[names(dataset) == "X1995..YR1995."] <- "1995"
names(dataset)[names(dataset) == "X2005..YR2005."] <- "2005"
internetinvest <- subset(dataset, select = c("country", "1995", "2005"), series == "Internet users (per 100 people)")
internetinvest$"1995" <- as.numeric(as.character(internetinvest$"1995"))
internetinvest$"2005" <- as.numeric(as.character(internetinvest$"2005"))
internetinvest$invest <- internetinvest$"2005" - internetinvest$"1995"
ggplot(internetinvest, aes(x = invest)) + geom_histogram(stat = "bin", binwidth=10, fill="deepskyblue3", colour="deepskyblue3") +labs(x="Binwidth: 10", y="Density of internet growth")
boxplot(internetinvest$"1995", internetinvest$"2005", internetinvest$invest, xlab = "1995, 2005, 2005-1995", ylab = "Internet users / 100 people")
basicStats(internetinvest$"1995")
basicStats(internetinvest$"2005")
basicStats(internetinvest$invest)

# GROUPS OF HIGH AND LOW INTERNET INVESTORS
internetinvest$high[internetinvest$invest > 23.97] <- 1
internetinvest$high[internetinvest$invest < 23.97] <- 0
internetinvest$low[internetinvest$invest < 23.97] <- 1
internetinvest$low[internetinvest$invest > 23.97] <- 0
freq(internetinvest$high, col="deepskyblue3", main="High") 
basicStats(internetinvest$high)
basicStats(internetinvest$low)

# GDPPC GROWTH
gdppcgrowth <- subset(dataset, select = c("country", "2005", "2015"), series == "GDP per capita (constant 2010 US$)")
gdppcgrowth$"2005" <- as.numeric(as.character(gdppcgrowth$"2005"))
gdppcgrowth$"2015" <- as.numeric(as.character(gdppcgrowth$"2015"))
gdppcgrowth$growth <- gdppcgrowth$"2015" - gdppcgrowth$"2005"
ggplot(gdppcgrowth, aes(x = growth)) + geom_histogram(stat = "bin", binwidth=100, fill="deepskyblue3", colour="deepskyblue3") +labs(x="Binwidth: 100", y="Density of GDPPC")
boxplot(gdppcgrowth$"2005", gdppcgrowth$"2015", gdppcgrowth$growth, xlab = "2005, 2015, 2015-2005", ylab = "GDP per capita (constant 2010 US$)")
basicStats(gdppcgrowth$"2005")
basicStats(gdppcgrowth$"2015")
basicStats(gdppcgrowth$growth)
gdppcgrowth$lngdppcgrowth <- log(gdppcgrowth$growth)
ggplot(gdppcgrowth, aes(x = lngdppcgrowth)) + geom_histogram(stat = "bin", binwidth=1, fill="deepskyblue3", colour="deepskyblue3") +labs(x="Binwidth: 100", y="Density of GDPPC")
basicStats(gdppcgrowth$lngdppcgrowth)

# GROUPS OF HIGH AND LOW GDPGROWTH
gdppcgrowth$high[gdppcgrowth$lngdppcgrowth > 6.74] <- 1
gdppcgrowth$high[gdppcgrowth$lngdppcgrowth < 6.74] <- 0
gdppcgrowth$low[gdppcgrowth$lngdppcgrowth < 6.74] <- 1
gdppcgrowth$low[gdppcgrowth$lngdppcgrowth > 6.74] <- 0
freq(gdppcgrowth$high, col="deepskyblue3", main="High") 
basicStats(gdppcgrowth$high)
basicStats(gdppcgrowth$low)

# CROSSTABLES
CrossTable(gdppcgrowth$high, internetinvest$high) 
CrossTable(gdppcgrowth$high, internetinvest$low) 
CrossTable(gdppcgrowth$low, internetinvest$high) 
CrossTable(gdppcgrowth$low, internetinvest$low) 

# HALT AND CATCH FIRE