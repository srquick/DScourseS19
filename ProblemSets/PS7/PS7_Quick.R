library(mice)
library(tidyverse)
library(stargazer)
library(broom)
library(mice)
library(tidyr)
library(magrittr)
library(HotDeckImputation)
library(stargazer)
library(tidyverse)

# Import, initial comparison, and steps as outlined in PS to follow:
wage <- read_csv("wages.csv")
wage1 <- wage %>% drop_na(hgc, tenure)
stargazer(wage1)



wage2 <- wage1 %>% drop_na(logwage)
est1 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=wage2)
stargazer(est1)



wage_mean <- wage1
wage_mean$logwage[is.na(wage_mean$logwage)] <- mean(wage_mean$logwage, na.rm = TRUE) 
est2 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=wage_mean)
stargazer(est2)



wage1$logwage_pred <- wage1$logwage
est3 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=wage1, na.action=na.exclude)
wage1$preds <- NA
wage1$preds [!is.na(wage1$hgc) & !is.na(wage1$tenure)] <- predict(est3, wage1, na.action=na.exclude)
wage1$logwage_pred[is.na(wage1$logwage)] <- wage1%preds[is.na(wage1$logwage)]



# Mice
head(wage1)
wage1.imp = mice(wage1, seed = 12345, m = 20)
summary(wage1.imp)
fit <- with(wage1.imp, lm(logwage ~ hgc + college + tenure + age + married))
round(summary(pool(fit)),2)



# Final Summary Table
stargazer(est1, est2, est3)