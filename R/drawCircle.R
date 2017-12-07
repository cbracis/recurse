#' @title Draws a circle
#' 
#' @description Draws a circle in data coordinates, so it will be a circle if the aspect
#' ratio of the plot is 1, or else it will be appear as an ellipse.
#' 
#' @details This function is useful to display a representative circle with the specified radius
#' on a plot of revisits.
#' 
#' @param x x-coordinate of circle center
#' @param y y-coordinate of circle center
#' @param radius radius of circle
#' @param nv how many plotted segments
#' @param border polygon border
#' @param col line color
#' @param lty line type
#' @param lwd line width
#' @return invisibly, the x and y points of the drawn circle
#' 
#' @seealso \code{\link{plot.recurse}}
#' 
#' @examples
#' data(martin)
#' revisits = getRecursions(martin, radius = 1)
#' plot(revisits, martin, legendPos = c(10, -15))
#' drawCircle(10, -10, 1)
#' 
#' @author Chloe Bracis <cbracis@uw.edu>
#' @export
#' 
drawCircle= function (x, y, radius, nv = 100, border = NULL, col = NA, lty = 1, lwd = 1) 
{
	# based on draw.circle() from plotrix, but draw oval for non 1-1 aspect ratio 
	# (data coordinates instead of user coordinates)
	
	angle.inc <- 2 * pi/nv
	angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
	if (length(col) < length(radius)) 
		col <- rep(col, length.out = length(radius))
	for (circle in 1:length(radius)) {
		xv <- cos(angles) * radius[circle] + x
		yv <- sin(angles) * radius[circle] + y
		graphics::polygon(xv, yv, border = border, col = col[circle], lty = lty, lwd = lwd)
	}
	invisible(list(x = xv, y = yv))
}