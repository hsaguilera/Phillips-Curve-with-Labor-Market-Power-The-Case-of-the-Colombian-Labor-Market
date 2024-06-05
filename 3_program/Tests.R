library(plm)
library(knitr)
library(broom)
library(tidyverse)
library(stargazer)
library(gplots)
library(wooldridge)
library(tseries)
library(lmtest)
library(car)
library(nlme)
library(ggplot2)
library(nortest)


######################################################################################
# Variance Inflation Factor (VIF) (Multicollinearity)
Panel <- read.csv("C:/Investigation/wage phillips curve with labor market power/2_data/monthly_model_data.csv")
Panel$period <- as.Date(Panel$period)

Panel$unem = log(Panel$unem)

ols = lm(dln_meansalary ~ unem*high_mkp, data = Panel)
summary(ols)

vif(ols, type = "predictor")


######################################################################################
Panel <- read.csv("C:/Investigation/wage phillips curve with labor market power/2_data/monthly_model_data.csv")
Panel$period <- as.Date(Panel$period)
Panel$unem = log(Panel$unem)

ggplot(Panel, aes(x = unem)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black", alpha = 0.4) +
  labs(title = "Histograma de Crecimiento de Salarios", x = "Crecimiento de Salarios", y = "Frecuencia")

ggplot(Panel, aes(sample = dln_meansalary_dpto)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot de Crecimiento de Salarios", x = "Cuantiles Teóricos", y = "Cuantiles Muestrales")

# Testing for time-fixed effects
fixed = plm(dln_meansalary_dpto ~ unem * high_mkp, data = Panel, 
                 index = c("dpto", "period"), model = "within")

fixed.time = plm(dln_meansalary_dpto ~ unem * high_mkp + factor(period), data = Panel, 
                 index = c("dpto", "period"), model = "within")

summary(fixed.time)

# Testing for time-fixed effects
## p-value < 0.05 se usa efectos fijos
pFtest(fixed.time, fixed) # Da p-value = 1
plmtest(fixed, c("time"), type = ("bp")) # Da p-value = 2.432e-14


#Testing for cross-sectional dependence/contemporaneous correlation: 
#using Breusch-Pagan LM test of independence and Pasaran CD test
## p-value < 0.05 Dependencia cross-sectional, si es mayor no hay dependencia
pcdtest(fixed, test = c("lm")) # p-value < 2.2e-16
pcdtest(fixed, test = c("cd")) # p-value = 1.777e-13

# Testing for serial correlation
## p-value < 0.05 Serial correlation
pbgtest(fixed) # p-value < 2.2e-16

# Testing for unit roots/stationarity
## H0: Existe raiz unitaria (Acepta NULA > .05)
## H1: No raiz unitaria (< .05) 
Panel.set = plm.data(Panel, index = c("dpto", "period"))
adf.test(Panel.set$unem) # p-value = 0.01
adf.test(Panel.set$inglabo) # p-value = 0.01
adf.test(Panel.set$high_mkp) # p-value = 0.01

# Testing for heteroskedasticity
## p-value < 0.05 Presencia de heterocedasticidad
bptest(dln_meansalary_dpto ~ unem*high_mkp + factor(dpto), data = Panel, studentize = F) # p-value < 2.2e-16

## Controlling for heteroskedasticity
coeftest(fixed)

coeftest(fixed, vcovHC)

coeftest(fixed, vcovHC(fixed, method = "arellano"))

coeftest(fixed, vcovHC(fixed, type = "HC3"))

t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"), 
         function(x) sqrt(diag(vcovHC(fixed, type = x)))))


######################################################################################
# OLS model
ols = lm(dln_meansalary ~ unem*high_mkp, data = Panel)
summary(ols)

# OLS model using plm package
pooled = plm(dln_meansalary ~ unem*high_mkp, data = Panel,
             model = "pooling", index = c("dpto", "period"))
summary(pooled)

stargazer(pooled, type = "text")

# Checking the basic assumption of homoscedasticity for OLS Model
res = residuals(ols)
yhat = fitted(ols)
plot(yhat, res)

# The Fixed Effect Model
fe = plm(dln_meansalary ~ unem*high_mkp, data = Panel,
         model = "within", index = c("dpto", "period"))

pFtest(fe, ols)

re = plm(dln_meansalary ~ unem*high_mkp, data = Panel,
         model = "random", index = c("dpto", "period"))

phtest(fe, re)

# Check to see if Panel Effects exist in data
plmtest(pooled, type = c("bp"))

# Test for cross-sectional dependence
pcdtest(fe, test = c("lm"))

# Test for serial correlation
pbgtest(fe)

# Test for heteroscedasticity
bptest(dln_meansalary ~ unem*high_mkp + factor(dpto), data = Panel, studentize = F)

# Correction
