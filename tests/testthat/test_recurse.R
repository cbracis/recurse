library(recurse)

context("recurse")

# support functions
#------------------------------------------------------------------------------------------------

getRevisits = function(data, radius)
{
	return(getRecursions(data, radius)$revisits)
}

createTimeVector = function(n)
{
	return(as.POSIXct("2009-5-1 1:00:00") + (1:n) * 60 * 60) # every hour
}

# data
#------------------------------------------------------------------------------------------------

# expected revisits for data "track" included in pacakge and calculated using an alternate method
# as.double(.Call("nvisits", df = track, radius = 1, maxt = 0, PACKAGE = "adehabitatHR"))
expectedRevisistsRadius1 = c(2, 2, 3, 2, 2, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2, 3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 1, 1, 1, 2, 2, 2, 2, 1, 1, 2)

# points on a line so distances between are unit lengths
simplePts = data.frame(x = 1, y = 1:5, t = createTimeVector(5))
simplePtsDist = matrix(0, nrow = 5, ncol = 5)
simplePtsDist =  abs(row(simplePtsDist) - col(simplePtsDist))

# track with data points on radius boundries so time in/out is easy to caluculate
gridTrack = data.frame(
	x = c(0, 1, 2, 1, 0, 1, 2, 2, 3, 5, 6, 3, 1, 3, 4),
	y = rep(0, 15),
	t = createTimeVector(15)
)
#plot(gridTrack$x, gridTrack$t/10, type = "b")

# tests
#------------------------------------------------------------------------------------------------

test_that("correct number of revisits",
		  {
		  	expect_equal(length(getRevisits(track, 1)), 100)
		  	
		  	expect_equal(getRevisits(track, 1), expectedRevisistsRadius1)
		  })

test_that("distance matrix",
		  {
		  	expect_equal(getRecursions(simplePts, 1)$dists, simplePtsDist)
		  })

test_that("time in radius",
		  {
		  	# examine visits to point at x=2 (use first time at t=3)
		  	stats = getRecursions(gridTrack, 1)$revisitStats
		  	expect_equal(as.numeric(stats[stats$coordIdx==3, "timeInside"]),
		  				 c(2, 3, 2))
		  	expect_equal(as.numeric(stats[stats$coordIdx==3, "timeSinceLastVisit"]),
		  				 c(NA, 2, 3))
		  	})