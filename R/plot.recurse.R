#' @title Calculates recursion information from the trajectory
#' 
#' @description Plots a trajectory color coded by number of revisits to each point.
#' 
#' @details This method allows the user to visually represent the number of revisitations by location. The size
#' of the circle of radius R can be added to the plot with \code{\link{drawCircle}}.
#' 
#' @param x \code{recurse} object returned from call to \code{\link{getRecursions}}
#' @param xyt data.frame of x, y, t, and id representing the xy-coordinates and the time (same as call to \code{\link{getRecursions}})
#' @param ... additional arguments to \code{\link{plot}}
#' @param col optional vector of colors as long as the maximum number of revisits to color code trajectory points
#' @param alpha optional alpha value for color transparency between 0 and 1
#' @param legendPos a vector of length 2 with the x and y coordinate of the center of the legend in user coordinates
#' @return the plot
#' 
#' @seealso \code{\link{getRecursions}}, \code{\link{getRecursionsAtLocations}}, \code{\link{drawCircle}}
#' 
#' @author Chloe Bracis <cbracis@uw.edu>
#' @export
#' 
#' @examples
#' data(martin)
#' revisits = getRecursions(martin, radius = 1)
#' plot(revisits, martin, legendPos = c(10, -15))
#' drawCircle(10, -10, 1)
#' 
plot.recurse = function(x, xyt, ..., col, alpha = 1, legendPos = NULL)
{
	getContinuousPalette = function(n)
	{
		if (!requireNamespace("scales", quietly = TRUE)) 
		{
			stop("scales package needed for this function to work. Please install it.",
				 call. = FALSE)
		}
		
		cols = scales::brewer_pal(palette = "RdYlBu", direction = -1)(8)[3:8] # drop darker blue colors
		return( scales::gradient_n_pal(cols)(seq(0, 1, length = n)) )
	}
	
	if (!methods::hasArg(col))
	{
		if (requireNamespace("scales", quietly = TRUE))
		{
			col = getContinuousPalette(max(x$revisits))
			col = scales::alpha(col, alpha)
		}
		else
		{
			col = rev(grDevices::heat.colors(max(x$revisits)))
		}
	}
	
	if (inherits(xyt, "Move") & requireNamespace("move", quietly = TRUE))
	{
		xyt = data.frame(xyt@coords, t = xyt@timestamps)
	}
	
	revOrder = order(x$revisits)
	graphics::plot(xyt[revOrder,1], xyt[revOrder,2], xlab = "x", ylab = "y", asp = 1, 
		 col = col[sort(x$revisits)], ...)
	
	if(!is.null(legendPos))
	{
		if (!requireNamespace("fields", quietly = TRUE)) 
		{
			stop("fields package needed for legend. Please install it.",
				 call. = FALSE)
		}	
		
		if (length(legendPos) != 2 | !is.numeric(legendPos))
		{
			stop("legendPos should be a vector of length 2 of the x,y coordinates for the legend.")
		}
		
		fields::colorbar.plot(legendPos[1], legendPos[2], col = col, strip=1:max(x$revisits))
		ucord = graphics::par()$usr
		pin = graphics::par()$pin
		xdelta = pin[2] / pin[1] * (ucord[2] - ucord[1]) * 0.4 * 0.5 # 0.4 is default width of colorbar
		graphics::text(x = c(legendPos[1] - xdelta, legendPos[1] + xdelta), y = rep(legendPos[2], 2),
			 labels = c(1, max(x$revisits)), pos = 1, offset = 1)
	}
	
}