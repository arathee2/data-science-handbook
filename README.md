### Linear Regression

	# check r-squared value. It has to be maximized for it be a good model. also check significance of variables. Remove
	# insignificant variables. Two tied insignificant vars can be removed by checking pr(>|t|) value. remove
	# insignificant vars with high pr(>|t|) value. check sign of estimate values of independent vars in summary(model)
	# and see if that makes sense. check for multicollinearity among independent vars(>0.7) and remove accordingly one of the two.

	# model

		linear.model <- lm(target ~ ., data = train)
		linear.predict <- predict(linear.model, cv)

	# calc RMSE and R-squared

		linear.sse <- sum((linear.predict - cv$target)^2)
		linear.sst <- sum((mean(train$target) - cv$target)^2)
		linear.r.squared <- 1-(linear.sse/linear.sst)
		linear.rmse <- sqrt(sse/nrow(cv))

	# ROCR and AUC(area under curve)
		
		library("ROCR")
		linear.predict <- predict(linear.model, cv)
		linear.ROCR <- prediction(linear.predict, cv$target)
		linear.auc <- as.numeric(performance(linear.ROCR,"auc")@y.values)

	# step model

		step.model <- step(linear.model)

==============================================================================================================================

### Logistic Regression
	
	# check for VIC. VIC to be minimized.

	# model

		glm.model <- glm(target ~ ., data = train, family = "binomial")
		glm.predict <- predict(glm.model, cv, type = "response")
	
	# confusion matrix for accuracy check
	
		table(cv$target, glm.predict>0.5)
	
	# ROCR and AUC(area under curve)
	
		library("ROCR")
		glm.predict <- predict(model, cv)
		glm.ROCR <- prediction(glm.predict, cv$target)
		glm.auc <- as.numeric(performance(glm.ROCR,"auc")@y.values)

==============================================================================================================================

### Neural Networks
	
	# min-max normalization

		maxs <- apply(data, 2, max) 
		mins <- apply(data, 2, min)
		scaled <- as.data.frame(scale(data_frame, center = mins, scale = maxs - mins))
		train.normalized
		test.normalized

	# model

		library(neuralnet)

		nn.model <- neuralnet(target ~ ., data = train.normalized, hidden=c(5,3), linear.output=T)
		# hidden specifies the neurons in hidden layers.
		# linear.output = T is for linear regression. It is set to F for classification.
		
		plot(nn.model)
		nn.predict <- compute(nn.model, cv.normalized[,1:13])

		# de-normalizing the predictions and test set to calculate the accuracy and not to forget to submit denormalized predictions.
		
		nn.predict.denormalized <- nn.predict$net.result*(max(data$target)-min(data$target))+min(data$target)
		test.denormalized <- (test.normalized$target)*(max(data$target)-min(data$target))+min(data$target)
		nn.rmse

==============================================================================================================================

### SVM

		library(e1071)

		svm.model <- svm(target ~ ., data = train, kernel = "radial", cost = 1, gamma = 0.1)
		svm.predict <- predict(svm.model, cv)
		table(cv$target, svm.predict)

==============================================================================================================================

### Decision Trees
	
	# for classification use method = "class" in rpart and type = "class" in predict
		library("rpart")
		library("rpart.plot")
		
	# model
		
		tree.model <- rpart(target ~ ., data = train, method = "class", minbucket/cp = ) # large min bucket=> overfitting, smaller minbucket => biasing.
		prp(tree.model) 
		tree.predict <- predict(tree.model, cv, type = "class")
		table(cv$target, tree.predict)

	# ROCR and AUC(area under curve)
		
		tree.predict <- predict(tree.model, cv) # type = "class" not to be included here
		tree.ROCR <- prediction(tree.predict[,2], cv$target)
		tree.auc <- as.numeric(performance(tree.ROCR,"auc")@y.values)

==============================================================================================================================

### Random Forest
		
		library("randomForest")
		set.seed(10)
		
	# model

		rf.model <- randomForest(target ~ ., data = train, ntree = 200, nodesize = 20) # nodesize similar to minbucket here.
		rf.predict <- predict(rf.model, cv)
		table(cv$target, rf.predict)

	# party-cforest

		library(party)

		cforest.model = cforest(target ~ . , data = train, controls=cforest_unbiased(ntree=1000))

		cforest.prediction = predict(cforest.model, cv, OOB = TRUE, type = "response")


==============================================================================================================================

### Clustering

	data_frame -> as.matrix -> as.vector -> clustering -> dim(vector) -> image.output
	data_frame -> as.matrix -> as.vector -> test

	# after clustering we can subset the data according to the clusters(for hierarchical especially) to study number of rows in each cluster.
	# cluster1 <- subset(train , cluster == nth.cluster.number)

	# Hierarchical

		distances <- dist(movie[2:20] OR vector, method = "euclidean")
		hcluster <- hclust(distances, method = "ward.D")
		plot(hcluster)
		hclusterGroups <- cutree(cluster, k = no.of.clusters)
		hclusterGroups[index.of.var.to.see.which.cluster.it.belongs.to]

	# K-Means

		library("flesclust")
		set.seed(1)
		kcluster <- kmeans(vector OR data_frame, centers = no.of.centroids, iter.max = no.of.max.iterations)
		str(kcluster)
		
		# rest of this for cluster-then-predict
		# in cluster-then-predict problems, (remove target var->normalize(optional)->cluster->kcca)=>build "k" train and test sets using subset from original train according to clusters.
		# example -> newTrain1 = subset(originalTrain, kclusterPredictTrain == 1), newTrain2 = subset(originalTrain, kclusterPredictTest == 2)
		
		kclusterKcca <- as.kcca(kcluster, originalDataFrame OR originalVector)
		kcluster.predict.train <- predict(kclusterKcca)
		kcluster.predict.test <- predict(kclusterKcca, newdata = test.data.as.vector)

		#easy way to split according to clusters in k-means
		
		KmeansCluster = split(data_frame, kcluster$cluster) # KmeansCluster[[1]] and so on to access 1st and other successive clusters

==============================================================================================================================

### Ensembling

	# 1. Split data into train, cv and test. Create multiple models using different ml algorithms on train set.
	# 2. Make a data frame ensemble_train which includes predictions of these multiple models (on cv) in each column along with the cv target variable.
	# ex: suppose model1 and model2 predict pred1_cv and pred2_cv on cv set then this ensemble_train contains pred1_cv,pred2_cv,cv_targetVariable.

	# 3. Now make predictions on test set using each of these models, say pred_test1, pred_test2 and make a data frame called ensemble_test where these are the columns.
	# 4. Now make a model ensemble_model which is built using ensemble_train and predicts on ensemble_test. This is the final prediction.

==============================================================================================================================

### K-fold cross validation
		
		# to calculate optimum value of cp for using it in CART.

		library(caret)
		set.seed(1)
		fitControl = trainControl( method = "cv", number = 10 )
		cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
		train( targetVariable ~ . , data = train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

==============================================================================================================================

### Splitting data set randomly

	# sample.split balacnes partitions keeping in mind the outcome variable

		library("caTools")
		set.seed(10)
		split <- sample.split(data_frame$target, SplitRatio = 0.8)
		train <- subset(data_frame,split == TRUE)
		cv <- subset(data_frame, split == FALSE)

==============================================================================================================================

### Multiple imputation- Filling NA's randomly

	# To be run only on variables having missing value. For convinience run on every variable except for dependent variable.
		
		library("mice")
		set.seed(10)
		imputed <- complete(mice(data_frame))
		data_frame$var <- imputed$var

==============================================================================================================================

### Removing Variables from data frame
		
		nonvars <- c("x1","x2","x3")
		train <- train[, !(names(data_frame) %in% nonvars)]
		test <- test[, !(names(data_frame) %in% nonvars)]
					
					OR

		data_frame$var <- NULL

					OR
		new_data_frame <- setdiff(names(data_frame),c("x1","x2","x3"))

==============================================================================================================================

### Plotting
	
		plot(x, y, xlab = , ylab = , main = , col = )
		hist(x, xlab = , xlim = c(low.limit, up.limit), breaks = 100)
		boxplot(y ~ x, xlab = , ylab = , main = )

==============================================================================================================================

### Misc
	
		which.max(var)
		which.min(var)
		outliers <- subset(data_frame, data_frame$var >=< condition)
		tapply(function.applied.to.this.var, result.displayed.according.to.this.var, function)
		match("value",var)
		which(var == "value")

	# choosing x random rows from a data set. given that x < nrow(train).

		trainSmall <- train[sample(nrow(train), x), ]

==============================================================================================================================

### Normalization 
	
	# this prevents to dominate vars with low values by the vars with high values. It sets the mean to 0 and SD to 1.

		library(caret)

		preproc <- preProcess(train)
		normTrain <- predict(preproc, train)
		normTest <- predict(preproc, test)

==============================================================================================================================

### One-Hot Encoding

		library(caret)

		dummies <- dummyVars(target ~ ., data = data_frame)
		temp <- as.data.frame(predict(dummies, data_frame))

==============================================================================================================================

### Correlation matrix
	
	library(corrplot)

		nums <- sapply(data_frame, is.numeric)
		cordata<-data_frame[,nums]
		cordata <-na.omit(cordata)
		cor_matrix <- cor(cordata) # to see correlation table
		cor_matrix
		corrplot(cor_matrix, method = "square", type = "lower") # to visualize correlation matrix

==============================================================================================================================

### Boruta Analysis to find importance of variable in a data set
	
	library(Boruta)	

		set.seed(13)
		bor.model <- Boruta(targetVariable ~ ., data = train, maxRuns=101, doTrace=0)
		summary(bor.model)
		boruta.cor.matrix <- attStats(bor.model)
		important.features <- features[boruta.model$finalDecision!="Rejected"]
		important.features

==============================================================================================================================

### Binning
	
		library("Hmisc")

		bins <- cut2(data_frame$variable, g = number_of_bins) # cuts the variable on basis of quantile.

==============================================================================================================================

### dplyr
		
		library("dplyr")

		select(data_frame, column_names)
		filter(data_frame, condition)
		arrange(data_frame, desc(factor_variable)) # ascending by default
		mutate(data_frame, new_variable_name = equation)
		group_by(data_frame, factor_variable)
		summarize(data_frame, newVarName =  function()) # group_by(factorVariable) %>% summarize(count = n()) used commonly.
		count(data_frame, variable) # works similar table
		top_n(20, data_frame) # for top 20 rows

		piping : data_frame %>% operation1 %>% operation2 and so on...

==============================================================================================================================

### Date and Time
	
		data_frame$date <- strptime(variable, format = "") # format can be - "%d/%m/%Y %H:%M:%S"
		data_frame$date <- as.POSIXct(data_frame$date) # dplyr does not handles date in POSIXlt format.
		data_frame$day <- as.integer(format(data_frame$date, "%d")) # day
		data_frame$month <- as.integer(format(data_frame$date, "%m")) # month
		data_frame$year <- as.integer(format(data_frame$date, "%Y")) # year
		data_frame$hour <- as.integer(format(data_frame$date, "%H")) # hour
		data_frame$weekday <- as.integer(format(data_frame$date, "%w")) # weekday 0(sunday) - 6(saturday)

==============================================================================================================================

### To check for constant factors

		f <- sapply(data_frame, function(x) is.factor(x))
		m <- data_frame[,names(which(f == "TRUE"))]
		ifelse(n <- sapply(m, function(x) length(levels(x))) == 1,"constant factor","0")
		table(n)

==============================================================================================================================

### caret

		library("caret")
		
		algo.control = trainControl( method = "repeatedcv", number = 10, repeats = 3 )
		algo.grid = expand.grid( model specific parameters )
		algo.model <-train(target ~ ., data = train, method = " ", preProcess = "scale", trControl = algo.control, tuneGrid = algo.grid)
		algo.predict <- predict.train(algo.model, cv)
		confusionMatrix(algo.predict, cv$target)
		imp <- varImp(algo.model)
		plot(imp, top = 20))

==============================================================================================================================

### GBM

		gbm.control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

		gbm.grid <-  expand.grid(interaction.depth = c(1, 5, 9), n.trees = (1:30)*50, shrinkage = 0.1, n.minobsinnode = 10)

		gbmFit <- train(target ~ ., data = train,
                method = "gbm",
                trControl = gbm.control,
                verbose = FALSE,
                tuneGrid = gbm.grid)

		gbm.predict <- predict(gbmFit, cv)

==============================================================================================================================

### xgboost
		
		library(xgboost)

		xgb.grid <- expand.grid(
	    eta = c(0.01, 0.001, 0.0001), 
		max_depth = c(5, 10, 15), 
	    gamma = c(1, 2, 3), 
	    colsample_bytree = c(0.4, 0.7, 1.0), 
	    min_child_weight = c(0.5, 1, 1.5),
	    nrounds = 2)

		xgb.control <- trainControl(
	    method="repeatedcv",
	    number = 10,
	    repeats = 3,
	    verboseIter = TRUE,
	    returnData=FALSE,
	    returnResamp = "all",
	    allowParallel = TRUE)

		xgb.model <- train(x = as.matrix(data_frame %>% select(-c(id,target))),
	    y = data_frame$target,
	    trControl = xgb.control,
	    tuneGrid = xgb.grid,
	    method="xgbTree" # "xgbLinear"
		)

		xgb.predict <- predict(xgb.model, data.matrix(cv))

		imp <- varImp(xgb.model)

==============================================================================================================================










==============================================================================================================================

Notes :

## If you set a random seed, split, set the seed again to the same value, and then split again, you will get the same
## split. However, if you set the seed and then split twice, you will get different splits. If you set the seed to
## different values, you will get different splits.

==============================================================================================================================





