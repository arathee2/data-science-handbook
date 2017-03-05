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
		
		library(ROCR)
		
		linear.predict <- predict(linear.model, cv)
		linear.ROCR <- prediction(linear.predict, cv$target)
		linear.auc <- as.numeric(performance(linear.ROCR,"auc")@y.values)

	# step model

		step.model <- step(linear.model)

==============================================================================================================================

### Logistic Regression
	
	## Binomial

		glm.model <- glm(target ~ ., data = train, family = "binomial", control = list(maxit = 50))
		glm.predict <- predict(glm.model, cv, type = "response")	
		table(cv$target, glm.predict>0.5)
	
	## Multinomial

		library ("nnet")

		multinom.model <- multinom(target ~ ., data = train)
		summary(multinom.model)
		multinom.predict <- predict(multinom.model, cv, "probs")

	## ROCR and AUC(area under curve)
	
		library(ROCR)
		library(pROC)

		glm.predict <- predict(model, cv)
		glm.ROCR <- prediction(glm.predict, cv$target)
		glm.AUC <- as.numeric(performance(glm.ROCR,"auc")@y.values)

		glm.prediction <- prediction(abs(glm.predict), cv$target)
		glm.performance <- performance(glm.prediction,"tpr","fpr")
		plot(glm.performance)

==============================================================================================================================

### K-Nearest Neighbor

		library("class")

		predictors <- names(train)[names(train) != "target"]
		knn.trainX <- train[, predictors]
		knn.cvX <- cv[, predictors]
		knn.trainY <- train$target
		knn.cvY <- cv$target
		
		# make sure all variables are numeric
		set.seed(1)
		knn.model <- knn(train = knn.trainX, test = knn.cvX, cl = knn.trainY, k = under.root.observations)
		confusionMatrix(knn.model, knn.cvY)

==============================================================================================================================

### Regularized Regression

		library("glmnet")
		# input numerical data only
		predictors <- names(train)[names(train) != "target"]
		trainX <- as.matrix(train[ ,predictors])
		trainY <- train$target
		cvX <- as.matrix(cv[ ,predictors])
		cvY <- cv$target

		# for classification add family = "binomial" to the following function.
		cv.glmnet.model <- cv.glmnet(trainX, trainY, type.measure = "deviance/mse/mae/class/auc", nfolds = 10, nlambda = 100)
		cv.glmnet.model
		plot(cv.glmnet.model)
		
		ridge.model <- glmnet(trainX, trainY, family = "gaussian","binomial","multinomial",
								alpha = 0, lambda = cv.glmnet.model$lambda.1se or cv.glmnet.model$lambda.min)

		lasso.model <- glmnet(trainX, trainY, family = "gaussian","binomial","multinomial",
								alpha = 1, lambda = cv.glmnet.model$lambda.1se or cv.glmnet.model$lambda.min)

		elnet.model <- glmnet(trainX, trainY, family = "gaussian","binomial","multinomial",
								alpha = 0.5, lambda = cv.glmnet.model$lambda.1se or cv.glmnet.model$lambda.min)

		ridge.prediction <- predict(ridge.model, cvX, s = lambda.used)
		lasso.prediction <- predict(lasso.model, cvX, s = lambda.used)
		elnet.prediction <- predict(elnet.model, cvX, s = lambda.used)
		rmse/table

==============================================================================================================================

### SVM

		library("e1071")

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
		
	# model

		set.seed(10)
		rf.model <- randomForest(target ~ ., data = train, importance = TRUE, ntree = 200, nodesize = 20) # nodesize similar to minbucket here.
		rf.predict <- predict(rf.model, cv)
		table(cv$target, rf.predict)
		varImpPlot(rf.model)

	# party-cforest

		library("party")

		cforest.model = cforest(target ~ . , data = train, controls=cforest_unbiased(ntree=1000, mtry = root.of.variables))
		cforest.prediction = predict(cforest.model, cv, OOB = TRUE, type = "response")
		table(cv$target, cforest.prediction)

==============================================================================================================================

### GBM

		library("gbm")

		gbm.model <- gbm(target ~ ., # if target doesn't work set to as.integer(target) after converting to numeric manually.
						distribution = c("bernoulli","multinomial","gaussian"), # multinomial more robust even in binomial case
						data = train,
						n.trees = 2000,
						interaction.depth = 1,
						n.minobsinnode = 10,
						shrinkage = 0.001,
						train.fraction = 1.0,
						keep.data = TRUE,
						verbose = TRUE)

		gbm.perf(gbm.model)

		gbm.predict <- predict(gbm.model, cv, n.trees = gbm.perf(gbm.model, plot.it = F)), type = "response")
		gbm.predict <- apply(gbm.predict, 1, which.max) # choose class with maximum probability

		table(gbm.predict, cv$target)

==============================================================================================================================

### H2O deep-learning

		library(h2o)

		localH2O <- h2o.init(ip = 'localhost', port = 54321, max_mem_size = '2g')
		
		names(train) <- NULL # optional. so that the algorithm does not take column name as a separate level in the target variable
		h2o.train <- as.h2o(train)
		h2o.cv <- as.h2o(cv)

		h2o.model <- h2o.deeplearning(x = setdiff(names(train), c("target","id")), # predictors names or indices
							y = "target", # label. check names(h2o.train) to see target variable name
							training_frame = h2o.train, # data to train
							activation = "TanhWithDropout", # or 'Tanh'
							input_dropout_ratio = 0.2, # % of inputs dropout
							hidden_dropout_ratios = c(0.5,0.5,0.5), # % for nodes dropout
							balance_classes = TRUE,
							hidden = c(50,50,50), # two hidden layers with 100 nodes each
							epochs = 100)

		h2o.predictions <- as.data.frame(h2o.predict(h2o.model, h2o.cv))
		confusionMatrix(h2o.predictions$predict, cv$target) # remove first row of h2o.predictions if length does not match

==============================================================================================================================

### xgboost
		
		library(xgboost)

		xgb.grid <- expand.grid(
	    eta = c(0.01, 0.001, 0.0001), 
		max_depth = c(5, 10, 15), 
	    gamma = c(1, 2, 3), 
	    colsample_bytree = c(0.4, 0.7, 1.0), 
	    min_child_weight = c(0.5, 1, 1.5),
	    nrounds = 2,
	    subsample = 1)

		xgb.control <- trainControl(
	    method="repeatedcv",
	    number = 3,
	    repeats = 3,
	    verboseIter = TRUE,
	    returnData=FALSE,
	    returnResamp = "all",
	    allowParallel = TRUE)

		xgb.predictors <- as.matrix(train[, !(names(train) %in% c("target","id"))])
		xgb.label <- train$target

		xgb.model <- train(x = xgb.predictors,
	    y = xgb.label,
	    trControl = xgb.control,
	    tuneGrid = xgb.grid,
	    method="xgbTree" # "xgbLinear"
		)

		xgb.predict <- predict(xgb.model, data.matrix(cv))
		confusionMatrix(xgb.predict, cv$target)
		important.features <- varImp(xgb.model)
		plot(important.features, 20)

==============================================================================================================================

### caret

		library("caret")
		
		f <- as.formula(paste("targetVariable ~", paste(names(train)[!names(train) %in% c("targetVariable")], collapse = " + ")))

		algo.control = trainControl(method = "repeatedcv", number = 10, repeats = 3)
		algo.grid = expand.grid(model specific parameters)
		algo.model <- train(target ~ ., data = train, method = "", preProcess = c("center","scale"),
							trControl = algo.control, tuneGrid = algo.grid)

		algo.predict <- predict.train(algo.model, cv)
		confusionMatrix(algo.predict, cv$target)
		imp <- varImp(algo.model)
		plot(imp, top = 20))

==============================================================================================================================

### K-fold cross validation

		k <- 10

		# Randomly shuffle the data
		data.frame <- data.frame[sample(nrow(data.frame)), ]

		# Create K equally size folds
		folds <- cut(seq(1, nrow(data.frame)), breaks = k, labels = FALSE)

		accuracy <- rep(0,k)
		x <- 0

		# Perform K-fold cross validation
		for(i in 1:k){
		    
		    x <- x + 1
		    #Segment your data by fold using the which() function 
		    cv.indices <- which(folds == i, arr.ind=TRUE)
			train <- data.frame[-cv.indices, ]
			cv <- data.frame[cv.indices, ]

			# model

			# calculate accuracy for current fold
			accurate.predictions <- 0
			confusion.matrix <- as.matrix(table(cv$target, predictions))
			
			for(i in 1:nrow(confusion.matrix)){
				accurate.predictions <- accurate.predictions + confusion.matrix[i,i]
			}

			accuracy[x] <- accurate.predictions/nrow(cv)
			print(paste("Fold",x,"accuracy",round(accuracy[x],4)))
			if(x == k) print(mean(accuracy))
		}

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

	predictions <- data.frame(algo1.prediction = , algo2.prediction = , algo3.prediction = ,
								final.prediction = rep(0, nrow(cv)), actual.label = cv$target)

    getmode <- function(x) {
    unique.x <- unique(x)
    unique.x[which.max(tabulate(match(x, unique.x)))]
	}

	predictions$final.prediction <- apply(predictions, 1, getmode)

==============================================================================================================================

### Splitting data set randomly

	# sample.split balances partitions keeping in mind the outcome variable

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

		library(DMwR)
		inputData <- knnImputation(inputData)

==============================================================================================================================

### Removing Variables from data frame
		
		data_frame <- data_frame[ ,!(names(data_frame) %in% c("x1","x2","x3"))]
		data_frame$var <- NULL
		new_data_frame <- setdiff(names(data_frame),c("x1","x2","x3"))

==============================================================================================================================

### Misc
	
		which.max(var)
		which.min(var)
		outliers <- subset(data_frame, data_frame$var >=< condition)
		match("value",var)
		which(var == "value")

		# choosing x random rows from a data set. given that x < nrow(train).
		trainSmall <- train[sample(nrow(train), x), ]

==============================================================================================================================

### One-Hot Encoding

		library(caret)

		dummies <- dummyVars(target ~ ., data = data_frame)
		temp <- as.data.frame(predict(dummies, data_frame))

==============================================================================================================================

### Correlation matrix
	
	library(corrplot)

		numeric <- sapply(data_frame, is.numeric)
		cordata <- data_frame[ ,numeric]
		cordata <- na.omit(cordata)
		cor_matrix <- cor(cordata) # to see correlation table
		cor_matrix
		corrplot(cor_matrix, method = "square", type = "lower") # to visualize correlation matrix

==============================================================================================================================

### Boruta Analysis to find importance of variable in a data set
	
	library(Boruta)	

		set.seed(13)
		boruta.model <- Boruta(targetVariable ~ ., data = train, maxRuns=101, doTrace=0)
		summary(boruta.model)
		boruta.cor.matrix <- attStats(boruta.model)
		important.features <- names(data_frame)[boruta.model$finalDecision!="Rejected"]
		important.features

==============================================================================================================================

### Binning
	
		library("Hmisc")

		bins <- cut2(data_frame$variable, g = number_of_bins) # cuts the variable on basis of quantile.

==============================================================================================================================

### To check for constant factors

		f <- sapply(data_frame, function(x) is.factor(x))
		m <- data_frame[ ,names(which(f == "TRUE"))]
		ifelse(n <- sapply(m, function(x) length(levels(x))) == 1,"constant factor","0")
		table(n)

==============================================================================================================================

### apply functions

		apply(array/matrix/data_frame, 1/2, function, ...) # need to specify row/column.
		lapply(list/vector/data_frame, function, ...) # work on columns. return list.
		sapply(list/vector/data_frame, function, ...) # work on columns. return vector if possible else matrix else list.
		tapply(function.applied.to.this.var, result.displayed.according.to.this.var, function)

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

### tidyr

		library(tidyr)

		separate(data_frame, variable, c("new","names"), sep = ", ") # separate rohtak, haryana
		
		unite(data_frame, new.variable, c(var1, var2), sep = " ") # combine first name and last name into name

		gather(data_frame, new.factor.var, new.numerical.var, column.start.name:column.end.name) %>%
				filter(new.numerical.var == 1) %>% 
				select(-new.numerical.var) # oppsoite of one-hot encoding

		spread(data_frame, categorical.var, corresponding.numerical.var) # opposite of gather

==============================================================================================================================

### Date and Time
	
		data_frame$date <- as.POSIXct(strptime(variable, format = "")) # format can be - "%d/%m/%Y %H:%M:%S"
		data_frame$date <- as.POSIXct(data_frame$date) # dplyr does not handles date in POSIXlt format.
		data_frame$day <- as.integer(format(data_frame$date, "%d")) # day
		data_frame$month <- as.factor(format(data_frame$date, "%B")) # month
		data_frame$year <- as.integer(format(data_frame$date, "%Y")) # year
		data_frame$weekday <- as.factor(format(data_frame$date, "%A")) # weekday
		data_frame$hour <- as.integer(format(data_frame$date, "%H")) # hour
		data_frame$minute <- as.integer(format(data_frame$date, "%M")) # minute

==============================================================================================================================



