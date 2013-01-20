source("R/discretization.R")
source("R/stop_criterion.R")
source("R/intervals_num_criterion.R")
source("R/bu_min_chi_criterion.R")

BottomUp <- function(formula, data, stop.criterion=RequestedIntervalsNumCriterion(5)) {
	# Computes the top-down discretization of given data.
	#
	# Args:
	#	formula: a formula, selecting args for discretization
	#	data: training data, used to build discretization model
	#	stop.criterion.name: name of stop criterion to use
	#	stop.criterion.params: correctly named list of params for s-c
	#
	# Returns:
	#	model that can be used for discretization

	model <- CreateBaseDiscretization(formula, data, stop.criterion)

	for (stop.criterion in model$stop.criterions) {
		if (!("BottomUpStopCriterion" %in% class(stop.criterion))) {
			stop("Argument 'stop.criterion' is not a TopDownStopCriterion")
		}
	}

	model$call <- sys.call()

	class(model) <- c("BottomUp", "Discretization")

	model <- DiscretizeSelected(model)	# in R objects are always passed by value..

	return(model)
}

DiscretizeAttribute.BottomUp <- function(object, attribute.name) {
	# Returns set of split points, determining discretization
	# of attribute 'attr', according to 'labels'
	print(paste("discretize attribute", attribute.name))

	data <- CreateSlice(object, attribute.name)

	split.points <- CreateInintialSplitPoints(data)
	print("initial split points")
	print(split.points)
	intervals <- CreateIntervals(data, split.points)
	print("initial intervals")
	print(intervals)
	chi.vector <- CalculateChiForAllIntervals(intervals)
	minChi = min(chi.vector)
	while (!StopCriterionSatisfied(object, intervals,minChi) && length(intervals)>1) {
		chi.vector <- CalculateChiForAllIntervals(intervals)

		minChi = min(chi.vector)
		print(paste("minChi",minChi))
		index.split.poin.to.merge = order(minChi)[1]
		split.points <- MergeSplitPoints(split.points, index.split.poin.to.merge)
		print("create intervals")
		print("data")		
		print(data)
		print("split.points")
		print(split.points)
		intervals <- CreateIntervals(data, split.points)
		if(length(intervals)==1) {
			print("Warning: no more intervals to merge!")
		}
	}

	return(split.points)
}

CreateIntervals <- function(data,split.points) {
	intervals <- list()
	maxi <- 0
	
	if(is.null(split.points) || length(split.points)==0) {
		intervals[[1]] <- data
		return (intervals)
	}

	for(i in 1:length(split.points)) {
		split.point <- split.points[i]
		selector <- data[["attr"]] < split.point
		intervals[[i]] <- data[selector,]
		data <- data[!selector,]
		maxi <- i
	}
	if(length(data[["attr"]]) > 0) {
		intervals[[maxi + 1]] <- data
	}
	return(intervals)
}

CreateInintialSplitPoints <- function(data) {
	split.points <- c()

	if(length(data$attr)==0) {
		return (split.points)
	}

	for(i in 1:(length(data[["attr"]])-1)) {
		value1 <- data[i, "attr"]
		value2 <- data[i+1, "attr"]
		if(value1 != value2) {
			split.point <- (value1+value2)/2
			split.points <- append(split.points,split.point)
		}
	}
	return (split.points)
}

CalculateChiForAllIntervals <- function(intervals) {
	chi.vector <- c()
	for(i in 1:(length(intervals)-1)) {
		interval1 <- intervals[i]
		interval2 <- intervals[i+1]
		chi <- i
		chi.vector <- append(chi.vector, chi)
	}
	return(chi.vector)
}
	
MergeSplitPoints <- function(split.points, index.split.poin.to.merge) {
	new.split.points <- c()
	print(paste("length split points", length(split.points)))
	if(length(split.points)==1) {
		print("return c()")
		return (new.split.points)
	}

	if(index.split.poin.to.merge == 1) {
		split.points.tmp1 <- c()
	} else {
		split.points.tmp1 <- split.points[1:(index.split.poin.to.merge-1)]
	}

	if(index.split.poin.to.merge == length(split.points)) {
		split.points.tmp2 <- c()
	} else {
		split.points.tmp2 <- split.points[(index.split.poin.to.merge+1):length(split.points)]
	}

	new.split.points <- c(split.points.tmp1,split.points.tmp2)
	
	return(new.split.points)
}

CalculateChi <-function(interval1, interval2) {
	sum1 <- 0
	sum2 <- 0
	chi<-0
	labels <- c(unique(interval1$labels), unique(interval2$labels))
	labels <- unique(labels)
	print(labels)

	for(label in labels) {
		A1j <- 0
		A2j <- 0
		print(paste("j",label))
		selector <- interval1$labels == label
		print(paste("selector",selector))
		A1j <- length(interval1[selector,]$attr)
		print(paste("A1j",A1j))
		selector <- interval2$labels == label
		print(paste("selector",selector))
		A2j <- length(interval2[selector,]$attr)
		print(paste("A2j",A2j))
		R1 <- length(interval1$attr)
		print(paste("R1 ",R1))
		R2 <- length(interval2$attr)
		print(paste("R2 ",R2))
		Cj <- A1j + A2j
		print(paste("Cj ",Cj))
		N <- R1 + R2
		print(paste("N ",N))
		E1j <- (R1*Cj)/N
		print(paste("E1j ",E1j))
		E2j <- (R2*Cj)/N
		print(paste("E2j ",E2j))
		tmpSum1j <- (((A1j-E1j)^2)/E1j)
		sum1 <- sum1+tmpSum1j
		print(paste("tmpSum1j",tmpSum1j))
		print(paste("sum1 ",sum1))
		tmpSum2j <- (((A2j-E2j)^2)/E2j)
		sum2 <- sum2+tmpSum2j
		print(paste("tmpSum2j",tmpSum2j))
		print(paste("sum2 ",sum2))
		chi <- tmpSum1j + tmpSum2j + chi
	}
	return(chi)
}
