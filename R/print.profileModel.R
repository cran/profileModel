`print.profileModel` <-
function (x, print.fit = FALSE, ...) 
{
    cat("\nCall: ", deparse(x$call), "\n\n")
    if (class(x) != "profileModel") 
        stop("An object of class 'profileModel' has to be supplied.")
    fitted <- x$fit
    BetaNames <- names(x$profiles)[!x$isNA]
    quantile <- x$quantile
    call.grid.bounds <- x$call[["grid.bounds"]]
    grid.bounds <- x$grid.bounds
    is.scaled <- !is.null(fitted$X.max.scaleFit)
    if (print.fit) {
        if (is.scaled) 
            cat("Fitted object (the design matrix is scaled by dividing each of its columns by the corresponding maximum):\n")
        else cat("Fitted object:\n")
        print(fitted)
        cat("\n")
    }
    cat("Profiled parameters:\n")
    print.default(BetaNames, quote = FALSE, print.gap = 2)
    cat("\n")
    if (!is.null(call.grid.bounds)) {
        cat("Profiling was done over the specified ranges of values:\n")
        print(grid.bounds)
    }
    else {
        if (!is.null(quantile)) 
            cat("Quantile was set to:", format(quantile, digits = getOption("digits")), 
                "\n")
        else {
            cat("Profiling was done over the ranges:\n")
            print(grid.bounds)
        }
    }
    cat("Grid size:", format(x$gridsize, digits = getOption("digits")), 
        "\n")
    cat("\n")
    cat("Agreement of the objective with fitting method", fitted$call[[1]], 
        ":", x$agreement, "\n")
    cat("Values of the objective less than", x$zero.bound, "were considered", 
        0, "\n")
    if (attr(x, "includes.traces")) 
        cat("The profile traces are included in the object.\n")
}
