library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(mlr)
library(tidyverse)
library(magrittr)
set.seed(100)

income <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.


######################
# Clean up the data
######################

# Drop unnecessary columns
income$native.country <- NULL
income$fnlwgt         <- NULL
# Make sure continuous variables are coded as such
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)
# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

## Break up the data:
n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]

# BEGIN HERE #
# The Classification Task
theTask <- makeClassifTask(id = "taskname", data = income.train, target = 'high.earner')

resampleStrat <- makeResampleDesc(method = "CV", iters = 3)

tuneMethod <- makeTuneControlRandom(maxit = 10L)


alg.tree <- makeLearner("classif.rpart",predict.type = "response")
alg.lreg <- makeLearner("classif.glmnet",predict.type = "response")
alg.NN <- makeLearner("classif.nnet",predict.type = "response")
alg.bayes <- makeLearner("classif.naiveBayes",predict.type = "response")
alg.kNN <- makeLearner("classif.kknn",predict.type = "response")
alg.svm <- makeLearner("classif.svm",predict.type = "response")

Param.tree <- makeParamSet(makeIntegerParam("minsplit",lower=10,upper=50),
                             makeIntegerParam("minbucket",lower=5,upper=50),
                             makeNumericParam("cp",lower=0.001,upper=0.2))
Param.lreg <- makeParamSet(makeNumericParam("lambda",lower=0,upper=3),
                             makeNumericParam("alpha",lower=0,upper=1))
Param.NN <- makeParamSet(makeIntegerParam("size",lower=1,upper=10),
                             makeNumericParam("decay",lower=.1,upper=.5),
                             makeIntegerParam("maxit",lower=1000,upper=1000))
Param.kNN <- makeParamSet(makeIntegerParam("k",lower=1,upper=30))
Param.svm <- makeParamSet(makeDiscreteParam("kernel",values=c("radial")),
                             makeDiscreteParam("cost",values=c(2^-2,2^-1,2^0,2^1,2^2,2^10)),
                             makeDiscreteParam("gamma",values=c(2^-2,2^-1,2^0,2^1,2^2,2^10)))

tuned.tree <- tuneParams(learner = alg.tree,
                          task = theTask,
                          resampling = resampleStrat,
                          measures = list(f1,gmean),
                          par.set = Param.tree,
                          control = tuneMethod,
                          show.info = TRUE)

tuned.lreg <- tuneParams(learner = alg.lreg,
                          task = theTask,
                          resampling = resampleStrat,
                          measures = list(f1,gmean),
                          par.set = Param.lreg,
                          control = tuneMethod,
                          show.info = TRUE)

tuned.NN <- tuneParams(learner = alg.NN,
                          task = theTask,
                          resampling = resampleStrat,
                          measures = list(f1,gmean),
                          par.set = Param.NN,
                          control = tuneMethod,
                          show.info = TRUE)

tuned.kNN <- tuneParams(learner = alg.kNN,
                          task = theTask,
                          resampling = resampleStrat,
                          measures = list(f1,gmean),
                          par.set = Param.kNN,
                          control = tuneMethod,
                          show.info = TRUE)

tuned.svm <- tuneParams(learner = alg.svm,
                          task = theTask,
                          resampling = resampleStrat,
                          measures = list(f1,gmean),
                          par.set = Param.svm,
                          control = tuneMethod,
                          show.info = TRUE)

### The Six Learner Algorithms Analysis

alg.tree <- setHyperPars(learner=alg.tree, par.vals = tuned.tree$x)
alg.lreg <- setHyperPars(learner=alg.lreg, par.vals = tuned.lreg$x)
alg.NN <- setHyperPars(learner=alg.NN, par.vals = tuned.NN$x)
alg.kNN <- setHyperPars(learner=alg.kNN, par.vals = tuned.kNN$x)
alg.svm <- setHyperPars(learner=alg.svm, par.vals = tuned.svm$x)

resample(alg.tree,theTask,resampleStrat,measures=list(f1,gmean))
resample(alg.lreg,theTask,resampleStrat,measures=list(f1,gmean))
resample(alg.NN,theTask,resampleStrat,measures=list(f1,gmean))
resample(alg.kNN,theTask,resampleStrat,measures=list(f1,gmean))
resample(alg.svm,theTask,resampleStrat,measures=list(f1,gmean))

final.tree  <- train(learner=alg.tree, task=theTask)
final.lreg  <- train(learner=alg.lreg, task=theTask)
final.NN    <- train(learner=alg.NN, task=theTask)
final.bayes <- train(learner=alg.bayes, task=theTask)
final.kNN   <- train(learner=alg.kNN, task=theTask)
final.svm   <- train(learner=alg.svm, task=theTask)


# Prediction of the Test Results - (Each of six)
prediction.tree  <- predict(final.tree, newdata=income.test)
prediction.lreg  <- predict(final.lreg, newdata=income.test)
prediction.NN    <- predict(final.NN, newdata=income.test)
prediction.bayes <- predict(final.bayes, newdata=income.test)
prediction.kNN   <- predict(final.kNN, newdata=income.test)
prediction.svm   <- predict(final.svm, newdata=income.test)

print(performance(prediction.tree, measures=list(f1, gmean)))
print(performance(prediction.lreg, measures=list(f1, gmean)))
print(performance(prediction.NN, measures=list(f1, gmean)))
print(performance(prediction.bayes, measures=list(f1, gmean)))
print(performance(prediction.kNN, measures=list(f1, gmean)))
print(performance(prediction.svm, measures=list(f1, gmean)))


