#' @describeIn getRecursionsAtLocations Get recursions at specified locations for a move2 object (for details see \code{vignette("programming_move2_object", package = "move2")})
#' @method getRecursionsAtLocations move2
#' @export
getRecursionsAtLocations.move2 = function(x, locations, radius, threshold = 0, timeunits = c("hours", "secs", "mins", "days"), verbose = TRUE)
{
  #check for move2 package
  if (!requireNamespace("move2", quietly = TRUE)) 
  {
    stop("move2 package needed for this function to work. Please install it.",
         call. = FALSE)
  }	
  
  stopifnot(move2::mt_is_time_ordered(x))
  stopifnot(move2::mt_is_track_id_cleaved(x))
  
  xyt = data.frame(
    x = sf::st_coordinates(x)[,1],
    y = sf::st_coordinates(x)[,2],
    t = move2::mt_time(x),
    id = move2::mt_track_id(x)
  )
  
	return( getRecursionsAtLocations(xyt, locations, radius, threshold, timeunits, verbose) )
}

