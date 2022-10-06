###Poisson and Logistic Regression###
library(readxl)
require(ggplot2)
require(sandwich)
require(msm)
library(aod)
library(dplyr)
library(logistf)
##Data
exe_1 = read_excel("exe1_poisson-logistic_regression.xlsx")
exe_2 = read_excel("exe1_poisson-logistic_regression.xlsx", 
                    sheet = "Sheet2")

#Change into factor/numeric
exe_1$jk = as.factor(exe_1$jk)
exe_1$ventilasi = as.factor(exe_1$ventilasi)
summary(exe_1)
colnames(exe_1)[2] = "count_tbc"

exe_2$Resiliensi = as.factor(exe_2$Resiliensi)
exe_2$`Suka Kuliah Online` = as.factor(exe_2$`Suka Kuliah Online`)
summary(exe_2)


##Poisson Regression, for data 1
with(exe_1, tapply(count, usia, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

ggplot(exe_1, aes(count_tbc, fill = ventilasi)) +
  geom_histogram(binwidth=.5, position="dodge")

poi_1 = glm(count_tbc ~ usia+jk+ventilasi , family = "poisson", data = exe_1)
summary(poi_1)
poi_2 = glm(count_tbc ~ usia+ventilasi , family = "poisson", data = exe_1)
summary(poi_2)

exe_1$"usia2" = ifelse(exe_1$usia=="3_>50", "3_>50", "1_<=50")
poi_3 = glm(count_tbc ~ usia2+ventilasi , family = "poisson", data = exe_1)
summary(poi_3)

exe_1$"ventilasi2" = ifelse(exe_1$ventilasi=="2_s", "2_s", "1_ts")
poi_4 = glm(count_tbc ~ usia2+ventilasi2 , family = "poisson", data = exe_1)
summary(poi_4)

poi_5 = glm(count_tbc ~ ventilasi , family = "poisson", data = exe_1)
summary(poi_5)
#Since we are limited to only discuss the first order of GLM, we choose best model based on AIC.
#We can also do deviance statistics analysis to compare between models
with(poi_1, cbind(res.deviance = deviance, df = df.residual,
               p = pchisq(deviance, df.residual, lower.tail=FALSE)))
#test model differences with chi square test
anova(poi_2, poi_1, test="Chisq")
anova(poi_5, poi_2, test="Chisq")
anova(poi_5, poi_1, test="Chisq")

#Thus a poisson regression with single predictor variable "ventilasi" is chosen (poi_5).
#Justification for using poisson regression is that the data has response variable
#of "count" type, that is for TBC disease patients.
#This particular data is not decently modeled by first order poisson regression.
#To intepret the coefficient, we use the "robust" version from Cameron and Trivedi (2009), which accounts for
#violation of assumption of mean = variance.
cov.poi5 = vcovHC(poi_5, type="HC0")
std.err = sqrt(diag(cov.poi5))
r.est = cbind(Estimate= coef(poi_5), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm(abs(coef(poi_5)/std.err), lower.tail=FALSE),
               LL = coef(poi_5) - 1.96 * std.err,
               UL = coef(poi_5) + 1.96 * std.err)
r.est
#Using robust-error-poisson regression, we can see that the coefficient estimates for "ventilasi_s" and 
#"ventilasi_c" are the same.
#The coefficients here imply logoarithm change in count of TBC patient.
#For "ventilasi_s", it is expected that the TBC patient log-count will differ about 0.4 with TBC patient log-count
#of the default "ventilasi_k"
#For "ventilasi_c" has the same interpretation as above.
#These 2 interpretation can be summarized as that an at least moderately-ventilated will be expected to have a 
#0.4 patient log count lower than the lack of ventilation.
#This result is parallel to the previous plot of count_tbc based on "ventilasi"
ggplot(exe_1, aes(count_tbc, fill = ventilasi)) +
  geom_histogram(binwidth=.5, position="dodge")
#We can see the "ventilasi_k" has mean of about 1.5 patient of TBC, while other categories of "ventilasi" 
#have mean of 1 patient of TBC


##Logistic Regression, for data 2
colnames(exe_2)[3] = "sko"
logi1 = glm(sko ~ IPK + Resiliensi, family = "binomial", data = exe_2)
summary(logi1)
#Warning message of numerical method error
#This is most likely due to outlier or maybe, it just has indistinguishable predicted and observed
#Lets inspect and compare predicted values vs observed values
exe_2$pred = predict(logi1, exe_2, type = "response")
View(exe_2)
#In this case, we have whats also known as "perfect fit". Thus, we can simply ignore it.
#OR, we can try identify a quite obvious "separation rule" that is happening in the data
exe_2 %>% group_by(sko) %>% summarise(
  min_ipk = min(IPK),
  max_ipk = max(IPK))
#Between "suka" and "tidak" the separation is perfectly dictated by variable "IPK"
#If, it is decided that "do nothing" is the choice, then simply interpret the coefficient with doing
#overall model check or significance test.
#For the other alternative, we can use penalized likelihood logisitc regression or also known as "firth logistic"
logi2 = logistf(sko ~ IPK + Resiliensi, data = exe_2)
summary(logi2)
#Interpreting using the firth logistic result above, it is found that "resiliensi" is not a useful predictor and
#we can simply build a simple logistic regression using "IPK" as predictor
logi3 = logistf(sko ~ IPK, data = exe_2)
summary(logi3)
#From result above, the log-odds of "Tidak" decreases by 11.34 for every 1-unit change in IPK