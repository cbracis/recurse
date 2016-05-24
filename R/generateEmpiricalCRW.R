generateEmpiricalCRW = function(n, steps, turnAngles)
{
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
	X<-cumsum(dX)
	Y<-cumsum(dY)
	
	return( data.frame(x = X, y = Y, t = 1:n))
}