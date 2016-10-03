### Linear Regression

	# check r-squared value. It has to be maximized for it be a good model. also check significance of variables. Remove
	# insignificant variables. Two tied insignificant vars can be removed by checking pr(>|t|) value. remove
	# insignificant vars with high pr(>|t|) value. check sign of estimate values of independent vars in summary(model)
	# and see if that makes sense. check for multicollinearity among independent vars(>0.7) and remove accordingly one of the two.

	# model

		model <- lm(y ~ x, data = train)
		pred <- predict(model, test)

	# calc RMSE and R-squared

		sse <- sum((pred - test$target)^2)
		sst <- sum((mean(train$target) - test$target) ^ 2)
		r-squared <- 1-(sse/sst)
		rmse <- sqrt(sse/nrow(data_frame))

	# ROCR and AUC(area under curve)
		
		library("ROCR")
		pred <- predict(model, test)
		ROCR.predict <- prediction(pred, test$target)
		auc <- as.numeric(performance(ROCR.predict,"auc")@y.values)

	# step model

		step.model <- step(model)

### Logistic Regression
	
	# check for VIC. VIC to be minimized.

	# model

		model <- glm(y ~ x, data = train, family = "binomial")
		pred <- predict(model, test, type = "response")
	
	# confusion matrix for accuracy check
	
		table(test$target, pred>threshold)
	
	# ROCR and AUC(area under curve)
	
		library("ROCR")
		pred <- predict(model, test)
		ROCR.predict <- prediction(pred, test$target)
		auc <- as.numeric(performance(ROCR.predict,"auc")@y.values)

### Trees
	
	# for classification use method = "class" in rpart and type = "class" in predict
		library("rpart")
		library("rpart.plot")
		
	# model
		
		model <- rpart(y ~ x, data = train, method = "class", minbucket/cp = ) # large min bucket=> overfitting, smaller minbucket => biasing.
		prp(model) 
		pred <- predict(model, test, type = "class")
		table(test$target, pred)

	# ROCR and AUC(area under curve)
		
		pred <- predict(model, test) # type = "class" not to be included here
		ROCR.predict <- prediction(pred[,2], test$target)
		auc <- as.numeric(performance(ROCR.predict,"auc")@y.values)

### Random Forest
		
		library("randomForest")
		set.seed(10)
		train$target <- as.factor(train$target)
		test$target <- as.factor(test$target)
	
	# model

		model <- randomForest(as.factor(y) ~ x, data = train, ntree = 200, nodesize = 5/25/15/20) #nodesize similar to minbucket here.
		pred <- predict(model, test)
		table(test$target, pred)


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
		kclusterPredictTrain <- predict(kclusterKcca)
		kclusterPredictTest <- predict(kclusterKcca, newdata = test.data.as.vector)

		#easy way to split according to clusters in k-means
		
		KmeansCluster = split(data_frame, kcluster$cluster) # KmeansCluster[[1]] and so on to access 1st and other successive clusters


### K-fold cross validation
		
		# to calculate optimum value of cp for using it in CART.

		library(caret)
		set.seed(1)
		fitControl = trainControl( method = "cv", number = 10 )
		cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
		train( targetVariable ~ . , data = train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )


### Splitting data set randomly

		# sample.split balacnes partitions keeping in mind the outcome variable

		library("caTools")
		set.seed(10)
		split <- sample.split(data_frame$target, splitRatio = 0.75)
		train <- subset(data_frame,split == TRUE)
		test <- subset(data_frame, split == FALSE)

### Multiple imputation- Filling NA's randomly

		# To be run only on variables having missing value. For convinience run on every variable except for dependent variable.
		
		library("mice")
		set.seed(10)
		imputed <- complete(mice(data_frame))

### Removing Variables from data frame
		
		nonvars <- c("x1","x2","x3")
		train <- train[, !(names(data_frame) %in% nonvars)]
		test <- test[, !(names(data_frame) %in% nonvars)]
					
					OR

		data_frame$var <- NULL

					OR
		new_data_frame <- setdiff(names(data_frame),c("x1","x2","x3"))

### Plotting
	
	plot(x, y, xlab = , ylab = , main = , col = )
	hist(x, xlab = , xlim = c(low.limit, up.limit), breaks = 100)
	boxplot(y ~ x, xlab = , ylab = , main = )

### Misc
	
	which.max(var)
	which.min(var)
	outliers <- subset(data_frame, data_frame$var >=< condition)
	tapply(function.applied.to.this.var, result.displayed.according.to.this.var, function)
	match("value",var)
	which(var == "value")

	# choosing x random rows from a data set. given that nrow(train > x)
	trainSmall <- train[sample(nrow(train), x), ]

### Normalization 
	
	# this prevents to dominate vars with low values by the vars with high values. It sets the mean to 0 and SD to 1.

	library(caret)

	preproc <- preProcess(Train)

	normTrain <- predict(preproc, Train)

	normTest <- predict(preproc, Test)

### One-Hot Encoding

	library(dummies)

	#create a dummy data frame
		new_my_data <- dummy.data.frame(my_data, names = c("Item_Fat_Content","Item_Type",
                        "Outlet_Establishment_Year","Outlet_Size",
                        "Outlet_Location_Type","Outlet_Type"))

### Correlation matrix
	
	library(corrplot)

		nums <- sapply(data_frame, is.numeric)
		cordata<-data_frame[,nums]
		cordata <-na.omit(cordata)
		cor_matrix <- cor(cordata) # to see correlation table
		corrplot(cor_matrix, method = "circle", type = "lower") # to visualize correlation matrix


### Boruta Analysis to find importance of variable in a data set
	
	library(Boruta)
		
		set.seed(13)
		bor_results <- Boruta(targetVariable ~ ., data = train, maxRuns=101, doTrace=0)
		summary(bor_results)





















Notes :

## If you set a random seed, split, set the seed again to the same value, and then split again, you will get the same
## split. However, if you set the seed and then split twice, you will get different splits. If you set the seed to
## different values, you will get different splits.
