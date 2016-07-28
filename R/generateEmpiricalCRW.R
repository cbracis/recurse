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