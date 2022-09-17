###Contingency Table Analysis###
#Probably the most popular way to represent categorical data. Contingency table simplifies categorical data
#type into table which cells are the count for each combination of category.
#We'll illustrate how to create, manipulate, and use the contingency table to gain insight from
#a categorical data type.
library(ISLR) #We use the wage data from this library
library(tidyverse)
library(Rfast)
library(MASS)
data("Wage")
View(Wage)

#Create a new category based on the wage
Wage$wage_cat = as.factor(ifelse(Wage$wage>median(Wage$wage),"Above","Below"))
View(Wage)
#Examine wage category against some other variables
table(Wage$jobclass,Wage$wage_cat)
table(Wage$education,Wage$wage_cat)      
table(Wage$race,Wage$wage_cat)
#We'll focus on the wage category vs  job class
con1 = table(Wage$jobclass,Wage$wage_cat)
#To further see the difference between the job class and wage category, we can do a mosaicplot that represent 
#the density for each cells relative to total number of observation
mosaicplot(con1)
#Next we can see the proportion of each cell relative to overall number of observation
prop.table(con1)
prop.table(con1, margin = 1)#adding proportion relative to row
prop.table(con1, margin = 2)#proportion relative to collumn
#Next we can do whats known as chi-square test of independence.

#This is a hypothesis test to inspect whether job class is independent with the wage category or not
chisq.test(con1)#Null hypothesis is that both variables are mutually independent
#From the chi square test above, it can be concluded that job class is not independent to the wage category.
#This is a strong indication of the relationship between the two categories.
#There is also an alternative the chi square test called the fisher exact test, this test is used instead of the
#chi square test when the number of observation is relatively low.
fisher.test(con1)

#Next we'll try log linear modelling for the data
#First we convert the data into a frequency table
freq1 = as.data.frame(as.table(con1))
colnames(freq1) = c("Job_class", "Wage_median", "Freq")#changing collumn names of the frequency table
freq1
#Then fitting log linear model using glm
mod0 = glm(Freq ~ Wage_median+Job_class+Wage_median:Job_class, data = freq1, family = poisson)
summary(mod0)
#Then weinspect the overall utility of the log linear model using deviance statistics
pchisq(deviance(mod0), df = df.residual(mod0), lower.tail = F)#compute p value for the deviance statistics
#The p value is very low even R function from above gives output of 0
#Null hypothesis that the expected frequency satisfy the given model is not accepted. In other words, the model
#fit seems to be poor. But at the same time, by using rule of thumb on the residual deviance against the degree
#of freedom, the model appears to be a good fit for the data.
#Therefore, we'll compapre side by side between the observed frequency and fitted frequency based on this model
cbind(mod0$data, fitted(mod0))
#This confirm the problem of "exact fit" which implies that the proposed model is exactly the same as the data.
#We particularly not against such model, but we also don't get any useful inference from this type of model.
#As such, we propose a new loglinear model as follow and repeat exactly the same procedure as above
mod1 = glm(Freq ~ Wage_median+Job_class, data = freq1, family = poisson)
summary(mod1)
pchisq(deviance(mod1), df = df.residual(mod1), lower.tail = F)#Poor fit
cbind(mod1$data, fitted(mod1))
#The second proposed model has poor fit to the data
#We can try for individual variables
mod2 = glm(Freq ~ Job_class, data = freq1, family = poisson)
summary(mod2)
mod3 = glm(Freq ~ Wage_median, data = freq1, family = poisson)
summary(mod3)
#We close this part with a note that the log linear model may not be a good model for this data, in particular
#job class against wage category data.

#Log likelihood test can be applied to test independence between 2 categorical variables
loglm(~1+2, con1)
#This further emphasize our previous finding of dependency between job class dan wage median category

#Next we discuss analysis for 3 way contingency table
#To create and save 3 way contingency table, xtabs function is used
con2 = xtabs(~jobclass+wage_cat+race, data=Wage)
ftable(con2)
#Conditional independence test for wage category against job class while controlling race using cochran-mantel-
#haenszel can be done with the following code
mantelhaen.test(con2)#The controlled variable is positioned in the last of the data(see line 77)
#With this 3 way contingency table, we can also build the log linear model
freq2 = as.data.frame(as.table(con2)) #transform into frequency table
mod4 = glm(Freq~ jobclass+wage_cat+race+jobclass:race+wage_cat:race+jobclass:wage_cat+jobclass:wage_cat:race, 
           data = freq2, family = poisson)
summary(mod4)
