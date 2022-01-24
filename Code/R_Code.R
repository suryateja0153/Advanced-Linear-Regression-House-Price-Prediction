#Author: Suryateja Chalapati

#Import Libraries
library(readxl)
library("car")
library(lmtest)
library("ggpubr")
library(stargazer)
library(data.table)

setwd("C:/Users/surya/Downloads")

hs <- read_excel("HuntersGreenHomeSales.xlsx", sheet = 'Data')

#NA values column wise
sapply(hs, function(x) sum(is.na(x)))
str(hs)

#Pre-processing
#Converting categoricals into binary factor levels
names(hs) <- tolower(colnames(hs))
setnames(hs, c('adom_agentdaysonmarket', 'cdom_cumuldaysmls'), c('adom', 'cdom'))
hs$spl_sale <- ifelse(hs$splsale == 'Short Sale' | hs$splsale == 'Bank Owned/REO' |
                        hs$splsale == 'Auction, Bank Owned/REO', 1, 0)
hs$baths_total <- hs$bathsfull + hs$bathshalf*0.5
#hs$shingle_roof <- ifelse(hs$roof == 'Shake, Shingle' | hs$roof == 'Shingle' | hs$roof == 'Shingle, Tile', 1, 0)
hs$tile_roof <- ifelse(hs$roof == 'Concrete, Tile' | hs$roof == 'Slate' | hs$roof == 'Slate, Tile' | hs$roof == 'Tile', 1, 0)
hs$private_pool <- ifelse(hs$pool == 'Private' | hs$pool == 'Private, Community', 1, 0)
hs$community_pool <- ifelse(hs$pool == 'Community' | hs$pool == 'Private, Community', 1, 0)

hs$pendingdate <- as.numeric(format(hs$pendingdate,'%Y'))
hs$datesold <- as.numeric(format(hs$datesold,'%Y'))
str(hs)

#Removing NA columns
hs <- hs[-c(1:3, 5:7, 10, 13:15, 24)]
hist(hs$pricesold)
hist(log(hs$adom))

#Checking correlations
library(PerformanceAnalytics)

hs_corr <- hs[, -c(3)]
chart.Correlation(hs_corr)

library(corrplot)

hs_corplot <- cor(hs_corr)
corrplot(hs_corplot, method = "number", number.cex= 12/ncol(hs))

#1st DV Regression models
names(hs)
p1 <- lm(pricesold ~ pendingdate + spl_sale, data = hs)
summary(p1)
p2 <- lm(pricesold ~ beds + sqft + garages + yrblt + spl_sale + baths_total 
         + tile_roof + private_pool + community_pool, data = hs)
summary(p2)
p3 <- lm(pricesold ~ beds + sqft + garages + yrblt + spl_sale + baths_total 
         + tile_roof + private_pool + community_pool + pendingdate, data = hs)
summary(p3)

#1st DV Stargazer
stargazer(p1, p2, p3, type='text', single.row = TRUE)
stargazer(p1, p2, p3, type='text', ci=TRUE, ci.level=0.95, single.row = TRUE)

#2nd DV Regression models
a1 <- lm(adom ~ pendingdate, data = hs)
summary(a1)
a2 <- lm(adom ~ yrblt + pendingdate + lppersqft + spl_sale, data = hs)
summary(a2)
a3 <- lm(adom ~ yrblt + pendingdate + lppersqft + spl_sale + baths_total + private_pool + community_pool, data = hs)
summary(a3)

#2nd DV Stargazer
stargazer(a1, a2, a3, type='text', single.row = TRUE)
stargazer(a1, a2, a3, type='text', ci=TRUE, ci.level=0.95, single.row = TRUE)

#Assumptions tests
#Plots
par(mfrow = c(2, 2))
plot(p3)
par(mfrow=c(1,1))

par(mfrow = c(2, 2))
plot(a3)
par(mfrow=c(1,1))

qqnorm(p3$res)
qqline(p3$res, col = 'red')

plot(p3$res ~ p3$fit)    
hist(p3$fit)

qqnorm(a3$res)
qqline(a3$res, col = 'red')

plot(a3$res ~ a3$fit)    
hist(a3$fit)

ggqqplot(p3$res, ylab = "Price Model 3")
ggqqplot(a3$res, ylab = "ADOM Model 3")

#Normality
#Shapiro-Wilk's Test
hist(p3$res)
shapiro.test(p3$res)

hist(a3$res)
shapiro.test(a3$res)

#Kolmogorov-Smirnov Test
n <- rnorm(500)
hist(n)
ks.test(n, p3$res)

hist(n)
ks.test(n, a3$res)

#Homoscedasticity
#Bartlett's Test
bartlett.test(list(p3$res, p3$fit))     
bartlett.test(list(a3$res, a3$fit))

#Levene's Test
leveneTest(p3$res, p3$fit, center=mean)
leveneTest(a3$res, a3$fit, center=mean)

#Breusch-Pagan Test
bptest(p3)
bptest(a3)

#Multicollinearity
vif(p3)
vif(a3)

#Autocorrelation (Independence)
#Durbin-Watson Test
dwtest(p3) 
dwtest(a3)
