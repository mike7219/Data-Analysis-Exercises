## Regression with binary outcomes
## ═════════════════════════════════

## Logistic regression
## ───────────────────────

##   This far we have used the `lm' function to fit our regression models.
##   `lm' is great, but limited–in particular it only fits models for
##   continuous dependent variables. For categorical dependent variables we
##   can use the `glm()' function.

##   For these models we will use a different dataset, drawn from the
##   National Health Interview Survey. From the [CDC website]:

##         The National Health Interview Survey (NHIS) has monitored
##         the health of the nation since 1957. NHIS data on a broad
##         range of health topics are collected through personal
##         household interviews. For over 50 years, the U.S. Census
##         Bureau has been the data collection agent for the National
##         Health Interview Survey. Survey results have been
##         instrumental in providing data to track health status,
##         health care access, and progress toward achieving national
##         health objectives.

##   Load the National Health Interview Survey data:
setwd("G:/Regression Assignment/")

NH11 <- readRDS("NatHealth2011.rds")
labs <- attributes(NH11)$labels
labs

##   [CDC website] http://www.cdc.gov/nchs/nhis.htm

## Logistic regression example
## ───────────────────────────────

##   Let's predict the probability of being diagnosed with hypertension
##   based on age, sex, sleep, and bmi

str(NH11$hypev) # check stucture of hypev
levels(NH11$hypev) # check levels of hypev

# collapse all missing values to NA
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))

# run our regression model
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,
              data=NH11, family="binomial")
coef(summary(hyp.out))
summary(hyp.out)
## Logistic regression coefficients
## ────────────────────────────────────

##   Generalized linear models use link functions, so raw coefficients are
##   difficult to interpret. For example, the age coefficient of .06 in the
##   previous model tells us that for every one unit increase in age, the
##   log odds of hypertension diagnosis increases by 0.06. Since most of us
##   are not used to thinking in log odds this is not too helpful!

##   One solution is to transform the coefficients to make them easier to
##   interpret
summary(hyp.out)


hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

## Generating predicted values
## ───────────────────────────────

##   In addition to transforming the log-odds produced by `glm' to odds, we
##   can use the `predict()' function to make direct statements about the
##   predictors in our model. For example, we can ask "How much more likely
##   is a 63 year old female to have hypertension compared to a 33 year old
##   female?".
?with
  
# Create a dataset with predictors set at desired levels
predDat <- with(NH11,
                expand.grid(age_p = c(33, 63),
                            sex = "2 Female",
                            bmi = mean(bmi, na.rm = TRUE),
                            sleep = mean(sleep, na.rm = TRUE)))
# predict hypertension at those levels
cbind(predDat, predict(hyp.out, type = "response",
                       se.fit = TRUE, interval="confidence",
                       newdata = predDat))

str(hyp.out)
##   This tells us that a 33 year old female has a 13% probability of
##   having been diagnosed with hypertension, while and 63 year old female
##   has a 48% probability of having been diagnosed.

## Packages for  computing and graphing predicted values
## ─────────────────────────────────────────────────────────

##   Instead of doing all this ourselves, we can use the effects package to
##   compute quantities of interest for us (cf. the Zelig package).

install.packages("effects")
install.packages("tidyr")
library(effects)
library(tidyr)
plot(allEffects(hyp.out))

## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.

NH11$r_maritl <- as.factor(NH11$r_maritl)
str(NH11$r_maritl)

NH11$r_maritl
##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).
ever.age.mar1 <- glm(everwrk ~ age_p + r_maritl,
                     data = NH11,
                     family = "binomial", na.action(NH11))
?glm
summary(ever.age.mar1)

ever.age.mar1

ever.age.mar12 <- coef(summary(ever.age.mar1))
ever.age.mar12[, "Estimate"] <- exp(coef(ever.age.mar1))
ever.age.mar12 




##   2. Predict the probability of working for each level of marital
##      status.
str(NH11$r_maritl)
NH11$r_maritl <- as.factor(NH11$r_maritl)

NH11$r_maritl

#Making New data Set w/ predictors
Predmarit1 <-  with(NH11,
                    expand.grid(r_maritl = c("5 Divorced", "7 Never married", "4 Widowed",
                                                                      "6 Separated", "1 Married - spouse in household",
                                                                      "8 Living with partner", "2 Married - spouse not in household"),
                                age_p = mean(age_p, na.rm = TRUE)),
                                stringsAsFactors = TRUE)


cbind(Predmarit1, predict(ever.age.mar1, type = "response",
        se.fit = TRUE, interval="confidence",
        newdata = Predmarit1))

#Results                      r_maritl    age_p        fit      se.fit residual.scale
#1                          5 Divorced 48.10983 0.06926155 0.006630955              1
#2                     7 Never married 48.10983 0.18082825 0.007649308              1
#3                           4 Widowed 48.10983 0.23521523 0.013295769              1
#4                         6 Separated 48.10983 0.12106080 0.015298167              1
#5     1 Married - spouse in household 48.10983 0.13400804 0.005009521              1
#6               8 Living with partner 48.10983 0.09011744 0.010666304              1
#7 2 Married - spouse not in household 48.10983 0.14374636 0.025782825              1


#Highest probability of working is Widowed

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.
