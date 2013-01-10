drawROC <-function(T,D){
	cutpoints<-c(-Inf, sort(unique(T)), Inf)
	print(cutpoints)
	sens<-sapply(cutpoints,
		function(c) sum(D[T > c]) / sum(D))
	print(sens)
	spec<-sapply(cutpoints,
		function(c) sum((1 - D)[T <= c] / sum(1 - D)))
	print(1- spec)
	#plot(1 - spec, sens, type="l")
}

ROC<-function(T,D){
	TT<-rev(sort(unique(T)))
	print(TT)
	DD<-table(-T,D)
	print(DD)
	sens<-cumsum(DD[, 2]) / sum(DD[, 2])
	mspec<-cumsum(DD[, 1]) / sum(DD[, 1])
	rval<-list(sens=sens, mspec=mspec, test=TT,
		call=sys.call())
	class(rval)<-"ROC"
	rval
}

print.ROC<-function(x,...){
	cat("ROC curve: ")
	print(x$call)
}

summary.ROC <- function(object, ...) {
	print('summary of ROC')
}

plot.ROC<-function(x, type="b", null.line=TRUE,
		xlab="1-Specificity", ylab="Sensitivity",
		main=NULL, ...) {
	par(pty="s")
	plot(x$mspec, x$sens, type=type,
	xlab=xlab, ylab=ylab, ...)
	if(null.line)
		abline(0, 1, lty=3)
	if(is.null(main))
		main<-x$call
	title(main=main)
}

lines.ROC<-function(x,...){
	lines(x$mspec, x$sens, ...)
}

identify.ROC<-function(x, labels=NULL,
		...,digits=1) {
	if (is.null(labels))
		labels<-round(x$test,digits)
	identify(x$mspec, x$sens, labels=labels,...)
}
