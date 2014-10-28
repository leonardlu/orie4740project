
## Creates ROC plot for model M, predicting over data = data and using a color c

ROC <- function(m,data,c) {
	temp <- predict(m,data,type="response")
	tempresult <- as.numeric(temp > 0.5)
	temp1 <- table(data$SeriousDlqin2yrs, tempresult)
	
	test.full <- cbind(data,temp)
	names <- names(test.full)
	names[length(test.full)] <- "predicted"
	names(test.full) <- names
	threshold <- seq(0,1,0.01)
	fpr <- rep(0,length(threshold))
	tpr <- rep(0,length(threshold))
	fnr <- rep(0,length(threshold))
	tnr <- rep(0,length(threshold))
	for(i in 1:length(threshold)) {
		bad <- test.full[test.full$predicted>threshold[i],]
		good <- test.full[test.full$predicted<=threshold[i],]
		fp <- bad[bad$SeriousDlqin2yrs==0,]
		tp <- bad[bad$SeriousDlqin2yrs==1,]
		fn <- good[good$SeriousDlqin2yrs==1,]
		tn <- good[good$SeriousDlqin2yrs==0,]
		fpr[i] <- length(fp$age)/(length(fp$age)+length(tn$age))
		fnr[i] <- length(fn$age)/(length(fn$age)+length(tp$age))
		tnr[i] <- length(tn$age)/(length(fp$age)+length(tn$age))
		tpr[i] <- length(tp$age)/(length(fn$age)+length(tp$age))
	}
	plot(fpr,tpr,col=c,type="l",pch=2)
	return(cbind(fpr,tpr,threshold))
}


## Just as ROC except this overlays ROC over a previous ROC plot (had to have initiated one before using this)
ROC.add <- function(m,data,c) {
	temp <- predict(m,data,type="response")
	tempresult <- as.numeric(temp > 0.5)
	temp2 <- table(data$SeriousDlqin2yrs, tempresult)
	
	test.full <- cbind(data,temp)
	names <- names(test.full)
	names[length(test.full)] <- "predicted"
	names(test.full) <- names
	threshold <- seq(0,1,0.01)
	fpr <- rep(0,length(threshold))
	tpr <- rep(0,length(threshold))
	fnr <- rep(0,length(threshold))
	tnr <- rep(0,length(threshold))
	for(i in 1:length(threshold)) {
		bad <- test.full[test.full$predicted>threshold[i],]
		good <- test.full[test.full$predicted<=threshold[i],]
		fp <- bad[bad$SeriousDlqin2yrs==0,]
		tp <- bad[bad$SeriousDlqin2yrs==1,]
		fn <- good[good$SeriousDlqin2yrs==1,]
		tn <- good[good$SeriousDlqin2yrs==0,]
		fpr[i] <- length(fp$age)/(length(fp$age)+length(tn$age))
		fnr[i] <- length(fn$age)/(length(fn$age)+length(tp$age))
		tnr[i] <- length(tn$age)/(length(fp$age)+length(tn$age))
		tpr[i] <- length(tp$age)/(length(fn$age)+length(tp$age))
	}
	lines(fpr,tpr,col=c,type="l",pch=2)
	return(cbind(fpr,tpr,threshold))
}


## ROC.add for neural networks
ROC.add.nnet <- function(m,data,c) {
	test.full <- cbind(data,predict(m,data,type="raw"))
	names <- names(test.full)
	names[length(test.full)] <- "predicted"
	names(test.full) <- names
	threshold <- seq(0,1,0.01)
	fpr <- rep(0,length(threshold))
	tpr <- rep(0,length(threshold))
	fnr <- rep(0,length(threshold))
	tnr <- rep(0,length(threshold))
	for(i in 1:length(threshold)) {
		bad <- test.full[test.full$predicted>threshold[i],]
		good <- test.full[test.full$predicted<=threshold[i],]
		fp <- bad[bad$SeriousDlqin2yrs==0,]
		tp <- bad[bad$SeriousDlqin2yrs==1,]
		fn <- good[good$SeriousDlqin2yrs==1,]
		tn <- good[good$SeriousDlqin2yrs==0,]
		fpr[i] <- length(fp$age)/(length(fp$age)+length(tn$age))
		fnr[i] <- length(fn$age)/(length(fn$age)+length(tp$age))
		tnr[i] <- length(tn$age)/(length(fp$age)+length(tn$age))
		tpr[i] <- length(tp$age)/(length(fn$age)+length(tp$age))
	}
	lines(fpr,tpr,col=c,type="l",pch=2)
	return(cbind(fpr,tpr,threshold))
}





## ROC curve for clustered data. Call to the function has to be of the form ROC.cluster( data.frame , "color"). A data frame has to have a column named "cluster" that specifies cluster of each observation.
ROC.cluster <- function(train, test, c) {
test.full <- cbind(test,rep(0,dim(test)[1]))
names <- names(test.full)
names[length(test.full)] <- "predicted"
names(test.full) <- names
model.list <- list()
cluster.list <- list()
dataClustered <- NULL
for (i in 1:max(test.full$cluster)){
cluster <- test.full[test.full$cluster==i,]
model.list[[i]] <- glm(SeriousDlqin2yrs ~ age + DebtRatio + log(MonthlyIncome+1) + NumberRealEstateLoansOrLines +  NumberOfDependents + log(TotalLate+1), family=binomial,data=subset(train,cluster==i))
cluster$predicted <- predict(model.list[[i]],cluster,type="response")
cluster.list[[i]] <- cluster
}

for (i in 1:max(test.full$cluster)) {
dataClustered <- rbind(dataClustered,cluster.list[[i]])
}

threshold <- seq(0,1,0.01)
	fpr <- rep(0,length(threshold))
	tpr <- rep(0,length(threshold))
	fnr <- rep(0,length(threshold))
	tnr <- rep(0,length(threshold))
	for(i in 1:length(threshold)) {
		bad <- dataClustered[dataClustered$predicted>threshold[i],]
		good <- dataClustered[dataClustered$predicted<=threshold[i],]
		fp <- bad[bad$SeriousDlqin2yrs==0,]
		tp <- bad[bad$SeriousDlqin2yrs==1,]
		fn <- good[good$SeriousDlqin2yrs==1,]
		tn <- good[good$SeriousDlqin2yrs==0,]
		fpr[i] <- length(fp$age)/(length(fp$age)+length(tn$age))
		fnr[i] <- length(fn$age)/(length(fn$age)+length(tp$age))
		tnr[i] <- length(tn$age)/(length(fp$age)+length(tn$age))
		tpr[i] <- length(tp$age)/(length(fn$age)+length(tp$age))
	}
	lines(fpr,tpr,col=c,type="l",pch=2)
	return(cbind(fpr,tpr,threshold))

}

ROC.cluster(cs.test, cs.train, "black")
summary(cs.test)
summary(factor(cs.train$cluster))
summary(cs.data.full)








