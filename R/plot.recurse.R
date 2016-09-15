#' @title Calculates recursion information from the trajectory
#' 
#' @description Plots a trajectory color coded by number of revisits to each point.
#' 
#' @param x \code{recurse} object returned from call to \link{getRecursions}
#' @param xyt data.frame of x, y, and t representing the xy-coordinates and the time (same as call to \link{getRecursions})
#' @param col optional vector of colors as long as the maximum number of revisits to color code trajectory points
#' @param ... additional arguments to \link{plot}
#' 
#' @return the plot
#' 
#' @export
#' 
plot.recurse = function(x, xyt, col, ...)
{
	if (!hasArg(col))
	{
		col = rainbow(max(x$revisits))[x$revisits]
	}
	plot(xyt[,1], xyt[,2], xlab = "x", ylab = "y", asp = 1, col = col, ...)
	
}