library(e1071)

PrepareDataset <- function(path) {
	data <- read.table(path)
	data$V1 <- as.factor(data$V1)	# change to categorical type
	return(data)
}

EvaluateBayes <- function(formula, train.set, test.set) {
	classifier <- naiveBayes(formula, train.set)
	predicted <- predict(classifier, test.set[, -1])
	confusion.matrix <- table(predicted, test.set[, 1])
	accuracy <- sum(diag(confusion.matrix)) / sum(confusion.matrix)
	return(accuracy)
}

CrossValidateBayes <- function(formula, dataset, n.folds) {
	id <- sample(1:n.folds, nrow(dataset), replace=TRUE)

	results.matrix <- matrix(nrow=n.folds, ncol=1)
	for (i in 1:n.folds) {
		test.set <- dataset[id == i, ]
		train.set <- dataset[id != i, ]
		results.matrix[i, 1] <- EvaluateBayes(formula, train.set, test.set)
	}
	scores <- results.matrix
	means <- apply(scores, 2, mean)
	stds <- apply(scores, 2, sd)
	return(list(accuracy=results.matrix, mean=means, std=stds))
}
