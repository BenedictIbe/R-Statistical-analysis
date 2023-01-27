#Import the libraries
install.packages("datarium")
install.packages("qqplotr")
install.packages("ggplot2")
install.packages("car")
install.packages("corrplot")
install.packages("caret")
install.packages("TTR")
install.packages("forecast")
install.packages('forecast')
install.packages("tidyverse")
install.packages('plotForecastErrors')
install.packages('moments')
library(tidyverse)
library(TTR)
library(forecast)
library(forecast)
library(moments)
library(readxl)
library(corrplot)
library(ggplot2)
library(datarium)
library(qqplotr)
library(car)
library(caret)



# Import the datasets and create the dataframe
economic_factors <- read_excel("Statistic-dataset.xlsx", sheet = "Data", na = "..")
View(economic_factors)

economic_factors_1 <- read_excel("Statistic-dataset-2.xlsx", sheet = "Initial-6", na = "..")
View(economic_factors_1)

economic_factors_2 <- read_excel("Statistic-dataset-2.xlsx", sheet = "Later-6", na = "..")
View(economic_factors_2)

before <- c(economic_factors_1$`GDP (current US$) [NY.GDP.MKTP.CD]`)
after <- c(economic_factors_2$`GDP (current US$) [NY.GDP.MKTP.CD]`)

new <- data.frame(before, after, each = 40)

economic_factors_n <- data.frame(economic_factors$`GDP (current US$) [NY.GDP.MKTP.CD]`,
                                 economic_factors$`Population, total [SP.POP.TOTL]`,
                                 economic_factors$`Exports of goods and services (current US$) [NE.EXP.GNFS.CD]`,
                                 economic_factors$`Foreign direct investment, net (BoP, current US$) [BN.KLT.DINV.CD]`,
                                 economic_factors$`GNI (current US$) [NY.GNP.MKTP.CD]`,
                                 economic_factors$`Labor force, total [SL.TLF.TOTL.IN]`,
                                 economic_factors$`External debt stocks, total (DOD, current US$) [DT.DOD.DECT.CD]`)



GDP <-  economic_factors$`GDP (current US$) [NY.GDP.MKTP.CD]`
population <- economic_factors$`Population, total [SP.POP.TOTL]`
export <- economic_factors$`Exports of goods and services (current US$) [NE.EXP.GNFS.CD]`
FDI <- economic_factors$`Foreign direct investment, net (BoP, current US$) [BN.KLT.DINV.CD]`
GNI <- economic_factors$`GNI (current US$) [NY.GNP.MKTP.CD]`
labour_Force <- economic_factors$`Labor force, total [SL.TLF.TOTL.IN]`
Debt <- economic_factors$`External debt stocks, total (DOD, current US$) [DT.DOD.DECT.CD]`


economic_factors_n <- data.frame(GDP, population, export,FDI, GNI, labour_Force, Debt)


#Detecting missing values
sum(is.na(economic_factors_n))

#Treatment of missing values using the imputation method
economic_factors_n$GDP[is.na(economic_factors_n$GDP)] <- mean(economic_factors_n$GDP, na.rm = TRUE)
economic_factors_n$GDP

economic_factors_n$population[is.na(economic_factors_n$population)] <- mean(economic_factors_n$population, na.rm = TRUE)
economic_factors_n$population

economic_factors_n$export[is.na(economic_factors_n$export)] <- mean(economic_factors_n$export, na.rm = TRUE)
economic_factors_n$export

economic_factors_n$FDI[is.na(economic_factors_n$FDI)] <- mean(economic_factors_n$FDI, na.rm = TRUE)
economic_factors_n$FDI

economic_factors_n$GNI[is.na(economic_factors_n$GNI)] <- mean(economic_factors_n$GNI, na.rm = TRUE)
economic_factors_n$GNI

economic_factors_n$labour_Force[is.na(economic_factors_n$labour_Force)] <- mean(economic_factors_n$labour_Force, na.rm = TRUE)
economic_factors_n$labour_Force

economic_factors_n$Debt[is.na(economic_factors_n$Debt)] <- mean(economic_factors_n$Debt, na.rm = TRUE)
economic_factors_n$Debt

new$before[is.na(new$before)] <- mean(new$before, na.rm = TRUE)
new$before

new$after[is.na(new$after)] <- mean(new$after, na.rm = TRUE)
new$after


#Density plots of the variables
plot(density(economic_factors_n$GDP))
plot(density(economic_factors_n$population))
plot(density(economic_factors_n$export))
plot(density(economic_factors_n$FDI))
plot(density(economic_factors_n$GNI))
plot(density(economic_factors_n$labour_Force))
plot(density(economic_factors_n$Debt))

#Outlier detection with boxplot
boxplot(economic_factors_n, names = c('GDP','Population', 'Export', 'FDI', 'GNI', 'labour_Force', 'Debt'))
plot(density(economic_factors_n$GDP))

#Exploratory Data Anannlysis
head(economic_factors_n)
names(economic_factors_n)
tail(economic_factors_n)
str(economic_factors_n)
summary(economic_factors_n)
sd(economic_factors_n$GDP)
sd(economic_factors_n$population)
sd(economic_factors_n$export)
sd(economic_factors_n$FDI)
sd(economic_factors_n$GNI)
sd(economic_factors_n$labour_Force)
sd(economic_factors_n$Debt)
var(economic_factors_n$GDP)
var(economic_factors_n$population)
var(economic_factors_n$export)
var(economic_factors_n$FDI)
var(economic_factors_n$GNI)
var(economic_factors_n$labour_Force)
var(economic_factors_n$Debt)

#To measure skewness and kurtosis
#Measurinng Skewness
print(skewness(economic_factors_n))

print(skewness(new))
plot(hist(new$before))
plot(hist(new$after))



#Measurinng Kurtosis
print(kurtosis(economic_factors_n))
plot(hist(economic_factors_n$FDI))


#Test of hypothesis

# Checking to see if our   data is normally distributed
ggplot(mapping = aes(sample=economic_factors_n$GDP)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")     

ggplot(mapping = aes(sample=economic_factors_n$population)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample") 

ggplot(mapping = aes(sample=economic_factors_n$export)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")

ggplot(mapping = aes(sample=economic_factors_n$FDI)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")

ggplot(mapping = aes(sample=economic_factors_n$GNI)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")

ggplot(mapping = aes(sample=economic_factors_n$labour_Force)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample")

ggplot(mapping = aes(sample=economic_factors_n$Debt)) +
  stat_qq_point(size = 2,color = "blue") +
  stat_qq_line(color="orange") +
  xlab("Theoretical") + ylab("Sample") 

shapiro.test(economic_factors_n$GDP)
shapiro.test(economic_factors_n$population)
shapiro.test(economic_factors_n$export)
shapiro.test(economic_factors_n$FDI)
shapiro.test(economic_factors_n$GNI)
shapiro.test(economic_factors_n$labour_Force)
shapiro.test(economic_factors_n$Debt)

#Handling Non normal data
cube_GDP <- economic_factors_n$GDP^(1/3)
shapiro.test(cube_GDP)
hist(cube_GDP)

log_GDP <- log10(economic_factors_n$GDP)
shapiro.test(log_GDP)
hist(log_GDP)

cube_population <- economic_factors_n$population^(1/3)
shapiro.test(cube_population)
hist(cube_population)

log_Population <- log10(economic_factors_n$population)
shapiro.test(log_Population)
hist(log_Population)

cube_export <- economic_factors_n$export^(1/3)
shapiro.test(cube_export)
hist(cube_export)

cube_FDI <- economic_factors_n$FDI^(1/3)
shapiro.test(cube_FDI)
hist(cube_FDI)

cube_GNI <- economic_factors_n$GNI^(1/3)
shapiro.test(cube_GNI)
hist(cube_GNI)

log_GNI <- log10(economic_factors_n$GNI)
shapiro.test(log_GNI)
hist(log_GNI)

cube_labourForce <- economic_factors_n$labour_Force^(1/3)
shapiro.test(cube_labourForce)
hist(cube_labourForce)

log_labourForce <- log10(economic_factors_n$labour_Force)
shapiro.test(log_labourForce)
hist(log_labourForce)


cube_debt <- economic_factors_n$Debt^(1/3)
shapiro.test(cube_debt)
hist(cube_debt)

log_debt <- log10(economic_factors_n$Debt)
shapiro.test(log_debt)
hist(log_debt)

log_before <- log10(new$before)
shapiro.test(log_before)
hist(log_before)

log_after <- log10(new$after)
shapiro.test(log_after)
hist(log_after)

#Hypothesis Test (Paired sample T-Test)
t.test(log_before, log_after, paired = TRUE, alternative = 'less')

# Correlation
round(cor(economic_factors_n),  digits = 2)
pairs(economic_factors_n[, c("population", "export", "Debt")])

#Plotting a correlogram
corrplot2 <- function(data,
                      method = "pearson",
                      sig.level = 0.05,
                      order = "original",
                      diag = FALSE,
                      type = "upper",
                      tl.srt = 90,
                      number.font = 1,
                      number.cex = 1,
                      mar = c(0, 0, 0, 0)) {
  library(corrplot)
  data_incomplete <- data
  data <- data[complete.cases(data), ]
  mat <- cor(data, method = method)
  cor.mtest <- function(mat, method) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], method = method)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  p.mat <- cor.mtest(data, method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(mat,
           method = "color", col = col(200), number.font = number.font,
           mar = mar, number.cex = number.cex,
           type = type, order = order,
           addCoef.col = "black", # add correlation coefficient
           tl.col = "black", tl.srt = tl.srt, # rotation of text labels
           # combine with significance level
           p.mat = p.mat, sig.level = sig.level, insig = "blank",
           # hide correlation coefficients on the diagonal
           diag = diag
  )
}

# edit from here
corrplot2(
  data = economic_factors_n,
  method = "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75
)
#Regression Analysis


model_eco <- lm(GDP ~ export + Debt + population , economic_factors_n)
summary.lm(model_eco)

plot(GDP ~ export, economic_factors_n,
     col = 'blue', main = 'Regression plot',
     xlab = 'variables',
     ylab = 'frequency')
abline(model_eco, col = 'red')

#Linearity check
colnames(economic_factors_n)
pairs(economic_factors_n[,c(1,3,7,2)], lower.panel = NULL, pch = 19,cex = 0.2)
plot(model_eco, 1)

#Normality check
plot(model_eco, 2)

#Homoscedasticity check
plot(model_eco, 3)

#Multicolinearity
vif(model_eco)


#Time Series

#Creating the time series object
economic_ts <- economic_factors %>% 
  mutate(Month = 1:n()) %>%
  gather(Year, `GDP (current US$) [NY.GDP.MKTP.CD]`, -Month) %>%
  arrange(Year, Month) %>%
  {ts(economic_factors$`GDP (current US$) [NY.GDP.MKTP.CD]`, start = c(2002, 1), frequency = 12)}


economic_ts

#Plot our timeseries
plot.ts(economic_ts)
logeconomic_ts <- log(economic_ts)
plot.ts(logeconomic_ts)

logeconomic_ts2 <- log(logeconomic_ts)
plot.ts(logeconomic_ts2)

#Decompose df
economic_ts_decompose <- decompose(logeconomic_ts)
economic_ts_decompose2 <- decompose(logeconomic_ts2)
economic_ts_decompose
economic_ts_decompose2

plot(economic_ts_decompose)
plot(economic_ts_decompose2)
#Forecasting
logeconomic_ts[is.na(logeconomic_ts)] <- mean(logeconomic_ts, na.rm = TRUE)
logeconomic_ts

logeconomic_ts2[is.na(logeconomic_ts2)] <- mean(logeconomic_ts2, na.rm = TRUE)
logeconomic_ts2

economic_ts_forecast <- HoltWinters(logeconomic_ts)
economic_ts_forecast

economic_ts_forecast2 <- HoltWinters(logeconomic_ts2)
economic_ts_forecast2

plot(economic_ts_forecast)
plot(economic_ts_forecast2)

economic_ts_forecast3 <- forecast(economic_ts_forecast, h = 60)
plot(economic_ts_forecast3)

economic_ts_forecast4 <- forecast(economic_ts_forecast2, h = 60)
plot(economic_ts_forecast4)

#Correlogram of our forecast
acf(economic_ts_forecast3$residuals, lag.max=20 , na.action = na.pass)
Box.test(economic_ts_forecast3$residuals, lag=20, type="Ljung-Box")

acf(economic_ts_forecast4$residuals, lag.max=20 , na.action = na.pass)
Box.test(economic_ts_forecast4$residuals, lag=20, type="Ljung-Box")

plot.ts(economic_ts_forecast4$residuals) # make time series plot

economic_ts_forecast4$residuals <- economic_ts_forecast4$residuals[!is.na(economic_ts_forecast4$residuals)]

#plotForecastErrors <- function(forecasterrors)


plotForecastErrors(economic_ts_forecast4$residuals)
