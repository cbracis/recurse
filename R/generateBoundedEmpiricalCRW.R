generateBoundedEmpiricalCRW = function(n, startX, startY, steps, turnAngles, boundry)
{
	
	require(prevR)
	
	X = vector("numeric", n)
	Y = vector("numeric", n)
	
	X[1] = startX
	Y[1] = startY
	Phi = 0
	
	for (i in 2:n)
	{
		tries = 0
		repeat
		{
			# draw steps
			step <- sample(steps, 1, replace = TRUE)
			
			# draw turning angles
			theta <- sample(turnAngles, 1, replace = TRUE)
			
			# cumulative angle (absolute orientation)
			Phi <- Phi + theta
			
			# step length components
			dX <- step*cos(Phi)
			dY <- step*sin(Phi)
			
			# actual X-Y values
			x <- X[i-1] + dX
			y <- Y[i-1] + dY
			
			if (point.in.SpatialPolygons(x, y, boundry))
			{
				# point is inside boundry 
				X[i] = x
				Y[i] = y
				break
			}
			if (tries > 10)
			{
				print(paste("next point outside boundary, i =", i,  "(", X[i-1], Y[i-1], ")"))
				break
			}
			tries = tries + 1
		} 
	}
	
	return( data.frame(x = X, y = Y, t = 1:n))
}