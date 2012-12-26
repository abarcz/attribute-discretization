library(rpart)

EvalTree <- function(train_set, test_set, treshold) {
	# build tree model, predicting the V1 attribute, basing on the rest
	model <- rpart(V1 ~ ., method="class", data=train_set)
	probabilities <- predict(model, test_set[,-1])

	# select classes with max probabilities
	max_prob <- apply(probabilities, 1, max)
	idx <- which(probabilities == max_prob, arr.ind=TRUE)
	no_decision_val <- 0

	# if two decision has the same probability, mark as no_decision
	n_non_unique_class <- 0
	classes <- matrix(nrow=nrow(probabilities), ncol=1)
	for (i in 1:nrow(probabilities)) {
		ith_class_entry <- idx[which(idx[, 1] == i), ]
		if (length(ith_class_entry) > 2) {
			n_non_unique_class <- n_non_unique_class + 1
			ith_class <- no_decision_val
		} else {
			ith_class <- ith_class_entry[2]
		}
		classes[i] <- ith_class
	}
	print("Number of non-unique maxima:")
	print(n_non_unique_class)

	# apply no-decision treshold
	no_decision_selector <- max_prob < treshold
	classes[no_decision_selector] <- no_decision_val

	correct_selector <- classes == test_set$V1
	correct_num <- sum(correct_selector == TRUE)
	no_decision_num <- sum(no_decision_selector == TRUE)
	incorrect_num <- sum(correct_selector == FALSE) - no_decision_num
	return(c(correct_num, no_decision_num, incorrect_num))
}

CrossValidateTree <- function(dataset, n_folds, treshold) {
	id <- sample(1:n_folds, nrow(dataset), replace=TRUE)
	#ListX <- split(X,id) # gives you a list with the 5 matrices

	results_matrix <- matrix(nrow=n_folds, ncol=3)
	for (i in 1:n_folds) {
		test_set <- dataset[id == i,]
		train_set <- dataset[id != i,]
		results <- EvalTree(train_set, test_set, treshold)
		results_matrix[i,] <- results
	}
	scores <- results_matrix / apply(results_matrix, 1, sum)
	means <- apply(scores, 2, mean)
	stds <- apply(scores, 2, sd)
	return(rbind(means,stds))
}
