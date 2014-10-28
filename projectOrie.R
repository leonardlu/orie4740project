
## STEP 1: SET UP R, READ IN DATA AND STORE VALUES FOR DIMENSIONS OF DATASET
library(mclust)
cs.data.full <- read.csv("cs-training.csv",sep=",", header=T)
nData = dim(cs.data.full)[1]
nCols = dim(cs.data.full)[2]


## STEP 2: DEAL WITH MISSING VALUES (2 ALTERNATIVES)

# METHOD A: Imputing missing values with median values
cs.data.full$MonthlyIncome[is.na(cs.data.full$MonthlyIncome)] <- median(cs.data.full$MonthlyIncome[!is.na(cs.data.full$MonthlyIncome)])
cs.data.full$NumberOfDependents[is.na(cs.data.full$NumberOfDependents)] <- median(cs.data.full$NumberOfDependents[!is.na(cs.data.full$NumberOfDependents)])

# METHOD B: Removing entire rows with missing data and updating dimensions of new datasetremoveBool = rep(T, nData)for (i in 1:nCols) { removeBool[is.na(cs.data.full[,i])] = F}cs.data.full <- cs.data.full[removeBool,]# Removes entries that have anomalous values for number of times of defaultcs.data.full <- cs.data.full[cs.data.full$NumberOfTime30.59DaysPastDueNotWorse < 95,]cs.data.full <- cs.data.full[cs.data.full$NumberOfTimes90DaysLate < 95,]cs.data.full <- cs.data.full[cs.data.full$NumberOfTime60.89DaysPastDueNotWorse < 95,]nData = dim(cs.data.full)[1]nCols = dim(cs.data.full)[2]nDatanCols




## STEP 3: CREATING NEW PREDICTOR VARIABLES

# NEW PREDICTOR 1: Creating new predictor of "MonthlyPayments" DebtRatio*MonthlyIncome
MonthlyPayments <- cs.data.full$MonthlyIncome*cs.data.full$DebtRatio
cs.data.full  <- cbind(cs.data.full,MonthlyPayments)

# NEW PREDICTOR 2: Creating new predictor MonthlyBalance difference between income and expense
MonthlyBalance <- cs.data.full$MonthlyIncome-cs.data.full$MonthlyPayments
cs.data.full  <- cbind(cs.data.full,MonthlyBalance)
cs.data.full$MonthlyBalance <- MonthlyBalance
cs.data.full$MonthlyPayments <- MonthlyPayments

# NEW PREDICTOR 3: Adding Total Late predictor = sum of number of times late
TotalLate <- NULL
TotalLate <- cs.data.full$NumberOfTime30.59DaysPastDueNotWorse+cs.data.full$NumberOfTime60.89DaysPastDueNotWorse+cs.data.full$NumberOfTimes90DaysLate
cs.data.full <- cbind(cs.data.full,TotalLate)


## STEP 4: SAMPLING 50,000 TO TRAIN DATA

# Sampling 50,000 cases for training at random. 
# I added more cases for training so that we are using exactly half the dataset as training data, and half as test data.
id <- sample(1:nData,50000)
cs.train <- cs.data.full[id,]
cs.test <- cs.data.full[-id,]
dim(cs.train)
dim(cs.test)





## STEP 5: PRINTING RESULTS FOR PRELIMINARY (PRE-CLUSTERING) RESULTS

# Modeling SeriousDlqin2yrs, the best model here is model.full.tranformed. Notice that some variables are transformed to improve normality.
cs.data <- na.omit(cs.data.full)
cs.data <- cs.data[,-1]
model.0 <- glm(SeriousDlqin2yrs ~ 1, data=cs.train, family=binomial)
model.full <- glm(SeriousDlqin2yrs ~ .,data=cs.train,family=binomial)
model.full.transformed <- glm(SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines + age + NumberOfTime30.59DaysPastDueNotWorse + DebtRatio + log(MonthlyIncome+1) + NumberOfOpenCreditLinesAndLoans + NumberOfTimes90DaysLate + NumberOfTime60.89DaysPastDueNotWorse+NumberOfTime30.59DaysPastDueNotWorse + NumberRealEstateLoansOrLines + NumberOfTime60.89DaysPastDueNotWorse + NumberOfDependents + log(TotalLate+1)+ MonthlyPayments+MonthlyBalance, family=binomial,data=cs.train)
model.interactions <- glm(SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines + age + NumberOfTime30.59DaysPastDueNotWorse + NumberOfOpenCreditLinesAndLoans + NumberOfTimes90DaysLate*NumberOfTime60.89DaysPastDueNotWorse*NumberOfTime30.59DaysPastDueNotWorse + NumberRealEstateLoansOrLines + NumberOfTime60.89DaysPastDueNotWorse + NumberOfDependents + log(TotalLate+1), family=binomial,data=cs.train)

# Creating ROC Plots for models. You have to run the code in ROC.R for this to work. 
ROC(model.0,cs.test,1)
ROC.add(model.full,cs.test,2)
#ROC.add(model.1,cs.test,3)
ROC.add(model.interactions,cs.test,4)
ROC.add(model.full.transformed,cs.test,"Black")





## STEP 6: CLUSTERING DATA

# ALTERNATIVE 1: Setting up k-means data and adding a cluster column
cs.data.kmeans <- cs.data.full[-1]
k.means <- kmeans(cs.data.kmeans, 5)
cs.data.full <- cbind(cs.data.full,k.means$cluster)
names(cs.data.full)[length(names(cs.data.full))] <- "cluster"
summary(cs.data.full)

# ALTERNATIVE 2: Using Mclust
cs.data.mclust <- cs.data.full[-1]
mclust <- Mclust(cs.data.mclust,G=5)
cs.data.full <- cbind(cs.data.full,mclust$cluster)
names(cs.data.full)[length(names(cs.data.full))] <- "cluster"
summary(cs.data.mclust)





## STEP 6: OBTAINING POST-CLUSTERING RESULTS
## Modeling SeriousDlqin2yrs, the best model here is model.full.tranformed. Notice that some variables are transformed to improve normality.
cs.data <- na.omit(cs.data.full)
cs.data <- cs.data[,-1]
model.0 <- glm(SeriousDlqin2yrs ~ 1, data=cs.train, family=binomial)
model.full <- glm(SeriousDlqin2yrs ~ .,data=cs.train,family=binomial)
model.full.transformed <- glm(SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines + age + NumberOfTime30.59DaysPastDueNotWorse + DebtRatio + log(MonthlyIncome+1) + NumberOfOpenCreditLinesAndLoans + NumberOfTimes90DaysLate + NumberOfTime60.89DaysPastDueNotWorse+NumberOfTime30.59DaysPastDueNotWorse + NumberRealEstateLoansOrLines + NumberOfTime60.89DaysPastDueNotWorse + NumberOfDependents + log(TotalLate+1)+ MonthlyPayments+MonthlyBalance, family=binomial,data=cs.train)
model.interactions <- glm(SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines + age + NumberOfTime30.59DaysPastDueNotWorse + NumberOfOpenCreditLinesAndLoans + NumberOfTimes90DaysLate*NumberOfTime60.89DaysPastDueNotWorse*NumberOfTime30.59DaysPastDueNotWorse + NumberRealEstateLoansOrLines + NumberOfTime60.89DaysPastDueNotWorse + NumberOfDependents + log(TotalLate+1), family=binomial,data=cs.train)

# Creating ROC Plots for models. You have to run the code I provided you with for this to work. 
ROC(model.0,cs.test,1)
ROC.add(model.full,cs.test,2)
#ROC.add(model.1,cs.test,3)
ROC.add(model.interactions,cs.test,4)
ROC.add(model.full.transformed,cs.test,"Black")






## Neural Network for classification. This one is interesting. It performed just as well.

library(nnet)
network1 <- nnet(SeriousDlqin2yrs ~ RevolvingUtilizationOfUnsecuredLines + age + NumberOfTime30.59DaysPastDueNotWorse + DebtRatio + log(MonthlyIncome+1) + NumberOfOpenCreditLinesAndLoans + NumberOfTimes90DaysLate + NumberOfTime60.89DaysPastDueNotWorse+NumberOfTime30.59DaysPastDueNotWorse + NumberRealEstateLoansOrLines + NumberOfTime60.89DaysPastDueNotWorse + NumberOfDependents + log(TotalLate+1)+ MonthlyPayments+MonthlyBalance,data=cs.train, size=1)
network2 <- nnet(SeriousDlqin2yrs ~ age + NumberOfTime30.59DaysPastDueNotWorse + DebtRatio + log(MonthlyIncome+1)+ NumberOfTimes90DaysLate + NumberOfTime60.89DaysPastDueNotWorse+NumberOfTime30.59DaysPastDueNotWorse + NumberRealEstateLoansOrLines + NumberOfTime60.89DaysPastDueNotWorse + NumberOfDependents + log(TotalLate+1)+ MonthlyPayments+MonthlyBalance,data=cs.train, 
size=2, decay=2, maxit=200)

ROC.add.nnet(network,cs.test,"blue")


library(tree)
## Classification Tree


fit = tree(formula = SeriousDlqin2yrs ~ age + DebtRatio + log(MonthlyIncome+1) + NumberRealEstateLoansOrLines + NumberOfDependents + log(NumberOfTime30.59DaysPastDueNotWorse+1) +  log(NumberOfTime60.89DaysPastDueNotWorse+1) + log(NumberOfTimes90DaysLate+1), data = cs.train, method="class")

plot(fit,uniform=TRUE, main="Classification Tree")
text(fit, use.n=TRUE, all=TRUE, cex=0.7)

res = predict.tree(object=fit, newdata=cs.test)






pfit<-prune(tree1,cp=fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])






##  Classification tree part 2
library(rpart)

fit = rpart(formula = SeriousDlqin2yrs ~ age + DebtRatio + log(MonthlyIncome+1) + NumberRealEstateLoansOrLines + NumberOfDependents + log(NumberOfTime30.59DaysPastDueNotWorse+1) +  log(NumberOfTime60.89DaysPastDueNotWorse+1) + log(NumberOfTimes90DaysLate+1), data = cs.train, method="class", control = rpart.control(minsplit=30,cp=0.0001))


#plotcp(fit)
table1 <-printcp(fit)


pfit = prune(fit, cp=0.00200343)
post(pfit,file="", cex=0.4)

plot(pfit,uniform=TRUE, main="Classification Tree")
text(pfit, use.n=TRUE, cex=0.4)

obj.pred = predict.tree(object=pfit, newdata=cs.test)
obj.pred








