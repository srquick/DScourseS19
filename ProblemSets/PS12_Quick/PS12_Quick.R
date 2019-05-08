library(stargazer)
library(tidyverse)
library(sampleSelection)
library(mlogit)

df <- read.csv(file="/Users/samuelquick/Desktop/wages12.csv")
df$college <- as.factor(df$college)
df$married <- as.factor(df$married)
df$union <- as.factor(df$union)
stargazer(df)
    
# Complete Cases
imp.deletion <- lm(logwage ~ hgc + exper + I(exper^2) + college + union, data=df, na.action = na.omit)
print(summary(imp.deletion))               

# Mean imputation with missing logwages
df$mean_logwage <- df$logwage
df %<>% mutate(mean_logwage = logwage)
x <- mean(df$logwage,na.rm=TRUE)
df$mean_logwage[is.na(df$mean_logwage)] <- x

imp.mean <- lm(mean_logwage ~ hgc + exper + I(exper^2) + college + union, data=df, na.action = na.omit)
print(summary(imp.mean))

# Sample Selection

mean(df$logwage, na.rm=TRUE)
valid <- na.omit(df$logwage)
!is.na(valid)

selection(selection = valid ~ hgc + union + college + exper + married + kids,
                    outcome = logwage ~ hgc + union + college + exper + I(exper^2),
                    data = df, method="2step")


# Probit model
estim <- glm(union ~ hgc + college + exper + married + kids, 
              family = binomial(link='probit'), data = df)
print(summary(estim))
df$union <- predict(estim, newdata = df, type = "response")
print(summary(df$union))

# Q9
# counterfactual policy: Union, marriage, and children
df$married <- c(0)
df$kids <- c(0)
estim$coefficients["married", "kids"] <- estim$coefficients["income"], 
df$predProbitCfl <- predict(union, newdata = df, type = "response")
print(summary(df$predProbitCfl ))



# Table to include in text.file
stargazer(imp.mean, imp.deletion)

