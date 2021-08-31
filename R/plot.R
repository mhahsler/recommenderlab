setMethod("plot", signature(x = "evaluationResults"),
	function(x, y,
		avg = TRUE, add=FALSE, type= "b", annotate = FALSE, ...) {

		if(missing(y)) y <- NULL
		plot_type <- match.arg(y, c("ROC", "prec/rec"))
		## if not ROC then prec/recall
		if(plot_type == "ROC") take <- c("FPR", "TPR")
		else take <- c("recall", "precision")

	  a <- avg(x)
	  n <- ""
	  n <- try(a[,"n"], silent = TRUE)
	  if(!("TPR" %in% colnames(a))) return(barplot(a))

		if(avg) {
		  x <- avg(x)


			x <- x[,take]
			if(add) lines(x, type=type,...)
			else graphics::plot(x, type=type, ...)

			## add annodations (xpd: don't clip)
			if(annotate) text(x[,1], x[,2], pos=3,
                            n, xpd=TRUE)
		}else{
			cm <- getResults(x)

			## plot first
			x <- cm[[1]][,take]
			if(add) lines(x, type=type,...)
			else graphics::plot(x, type=type, ...)

			## add annodations
			if(annotate) text(x[,1], x[,2], pos=3,
                            rownames(x), xpd=TRUE)

			## plot rest
			x <- cm[-1, drop = FALSE]

			tmp <- lapply(x, function(y) {
					y<-y[,take]
					lines(y, type=type,...)
				})
		}
	})



setMethod("plot", signature(x = "evaluationResultList"),
        function(x, y,
                xlim=NULL, ylim=NULL, col = NULL, pch = NULL, lty = 1,
                avg = TRUE, type="b",
                annotate= 0, legend="bottomright", ...) {

    a <- avg(x)
    if(!("TPR" %in% colnames(a[[1]]))) return(barplot(do.call(rbind, a),
      beside=TRUE, legend.text=names(a), ylim=ylim, col=col))

    if(is.null(pch)) pch <- 1:length(x)
    if(type=="l") pch <- NULL
    if(missing(y)) y <- NULL
    plot_type <- match.arg(y, c("ROC", "prec/rec"))
    take <- if(plot_type == "ROC") c("FPR", "TPR") else c("recall", "precision")

    ## find best xlim, ylim
    max_lim <- apply(sapply(x, FUN =
            function(y) apply(avg(y)[,take], MARGIN=2, max)), MARGIN=1, max)

    if(is.null(xlim)) xlim <- c(0, max_lim[1])
    if(is.null(ylim)) ylim <- c(0, max_lim[2])

    ## fix pch, lty and col
    if(length(pch)==1) pch <- rep(pch, length(x))
    if(length(lty)==1) lty <- rep(lty, length(x))

    if(is.null(col)) col <- 1:length(x)
    if(length(col)==1) col <- rep(col, length(x))


    graphics::plot(NA, xlab=take[1], ylab=take[2], ylim=ylim, xlim=xlim)
    if (!is.null(legend))
      legend(x=legend, legend=names(x), col=col, pch = pch, lty=lty, bty="n")
    for(i in 1:length(x)) plot(x[[i]], y=plot_type,
        add=TRUE, col=col[i], type=type, annotate = i %in% annotate,
        pch = pch[i], lty=lty[i], avg = avg, ...)
})

