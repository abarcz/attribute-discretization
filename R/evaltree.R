library(rpart)

EvalTree <- function(train.set, test.set, treshold) {
	# build tree model, predicting the V1 attribute, basing on the rest
	model <- rpart(V1 ~ ., method="class", data=train.set)
	probabilities <- predict(model, test.set[, -1])

	# select classes with max probabilities
	max.prob <- apply(probabilities, 1, max)
	idx <- which(probabilities == max.prob, arr.ind=TRUE)
	no.decision.val <- 0

	# if two decision has the same probability, mark as no.decision
	n.non.unique.class <- 0
	classes <- matrix(nrow=nrow(probabilities), ncol=1)
	for (i in 1:nrow(probabilities)) {
		ith.class.entry <- idx[which(idx[, 1] == i), ]
		if (length(ith.class.entry) > 2) {
			n.non.unique.class <- n.non.unique.class + 1
			ith.class <- no.decision.val
		} else {
			ith.class <- ith.class.entry[2]
		}
		classes[i] <- ith.class
	}
	print("Number of non-unique maxima:")
	print(n.non.unique.class)

	# apply no-decision treshold
	no.decision.selector <- max.prob < treshold
	classes[no.decision.selector] <- no.decision.val

	correct.selector <- classes == test.set$V1
	correct.num <- sum(correct.selector == TRUE)
	no.decision.num <- sum(no.decision.selector == TRUE)
	incorrect.num <- sum(correct.selector == FALSE) - no.decision.num
	return(c(correct.num, no.decision.num, incorrect.num))
}

CrossValidateTree <- function(dataset, n.folds, treshold) {
	id <- sample(1:n.folds, nrow(dataset), replace=TRUE)
	#ListX <- split(X,id) # gives you a list with the 5 matrices

	results.matrix <- matrix(nrow=n.folds, ncol=3)
	for (i in 1:n.folds) {
		test.set <- dataset[id == i, ]
		train.set <- dataset[id != i, ]
		results <- EvalTree(train.set, test.set, treshold)
		results.matrix[i, ] <- results
	}
	scores <- results.matrix / apply(results.matrix, 1, sum)
	means <- apply(scores, 2, mean)
	stds <- apply(scores, 2, sd)
	return(rbind(means, stds))
}
