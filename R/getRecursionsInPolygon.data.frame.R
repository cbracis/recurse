#' @describeIn getRecursionsInPolygon Get recursions inside a polygon for a trajectory data.frame object consisting of columns x, y, datetime, and id
#' @method getRecursionsInPolygon data.frame
#' @export
getRecursionsInPolygon.data.frame = function(trajectory, polygon, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), verbose = TRUE)
{
	stop("This method is temporarily unavailable until it can be migrated due to removal of rgeos.")
	
}