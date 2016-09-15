#' @title Generates a correlated random walk from an empirical set of steps and angles
#' 
#' @description Generates a correlated random walk starting at (startX, startY) using pairs of steps and 
#' turning angles drawn from the provided empirical distributions.
#' 
#' @param n number of steps to generate
#' @param startX starting x-coordiante for the walk
#' @param startY starting y-coordinate for the walk
#' @param steps vector of steps to draw from
#' @param turnAngles vector of turning angles to draw from
#' 
#' @return a data frame of the generated random walk with columns x and y describing the location 
#' and t for the time of each step
#' 
#' @export
#' 
generateEmpiricalCRW = function(n, startX, startY, steps, turnAngles)
{
	stopifnot(length(steps) == length(turnAngles))
	nSteps = length(steps)
	
	# draw random index
	idx <- sample(1:nSteps, n, replace = TRUE)
	
	# draw step and turning angle
	steps <- steps[idx]
	theta <- turnAngles[idx]

	# draw steps
	steps <- sample(steps, n, replace = TRUE)
	
	# draw turning angles
	theta <- sample(turnAngles, n, replace = TRUE)
	
	# cumulative angle (absolute orientation)
	Phi <- cumsum(theta)
	
	# step length components
	dX <- steps*cos(Phi)
	dY <- steps*sin(Phi)
	
	# actual X-Y values
	X <- cumsum(dX) + startX
	Y <- cumsum(dY) + startY
	
	return( data.frame(x = X, y = Y, t = 1:n))
}