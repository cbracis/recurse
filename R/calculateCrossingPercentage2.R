#'
#' @title interpolates crossing time
#' 
#' @description used to verify alternate algorithm
#' 
#' @param xCen x-coordinate of center of circle
#' @param yCen y-coordinate of center of circle
#' @param xIn x-coordinate of point inside circle
#' @param yIn y-coordinate of point inside circle
#' @param xOut x-coordinate of point outside circle
#' @param yOut y-coordinate of point outside circle
#' @param radius radius of circle
calculateCrossingPercentage2 = function(xi, yi, x1, y1, x2, y2, r)
{
		d = sqrt((x2-x1)^2 + (y2-y1)^2)
		a = atan2(y2-y1, x2-x1)
		u = ((xi-x1)*cos(a))+((yi-y1)*sin(a))
		v = ((yi-y1)*cos(a))-((xi-x1)*sin(a))
		g = sqrt(r^2 - v^2)
		p = (u + g)/d
		return(p) 
}