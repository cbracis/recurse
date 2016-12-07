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

createMoveObj = function(df)
{
	require(move)
	return( move(x = df$x, y = df$y, time = df$t,
				 proj = CRS("+proj=aeqd"), animal = df$id) )
}

# data
#------------------------------------------------------------------------------------------------

# expected revisits for data "track" included in pacakge and calculated using an alternate method
# as.double(.Call("nvisits", df = track, radius = 1, maxt = 0, PACKAGE = "adehabitatHR"))
expectedRevisistsRadius1 = c(2, 2, 3, 2, 2, 2, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2, 3, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 1, 1, 1, 2, 2, 2, 2, 1, 1, 2)

# points on a line so distances between are unit lengths
simplePts = data.frame(x = 1, y = 1:5, t = createTimeVector(5), id = "A1")
simplePtsDist = matrix(0, nrow = 5, ncol = 5)
simplePtsDist =  abs(row(simplePtsDist) - col(simplePtsDist))

# two tracks, one vertical x = 1, one horizontal y = 3
horzTrack = data.frame(x = -1:3, y = 3, t = createTimeVector(5), id = "B2")
twoTracks = rbind(simplePts, horzTrack)

# track with data points on radius boundries so time in/out is easy to caluculate
gridTrack = data.frame(
	x = c(0, 1, 2, 1, 0, 1, 2, 2, 3, 5, 6, 3, 1, 3, 4),
	y = rep(0, 15),
	t = createTimeVector(15),
	id = rep(1, 15)
)
#plot(gridTrack$x, gridTrack$t/10, type = "b")

# tests
#------------------------------------------------------------------------------------------------

test_that("correct number of revisits",
		  {
		  	expect_equal(length(getRevisits(track, 1)), 100)
		  	
		  	expect_equal(getRevisits(track, 1), expectedRevisistsRadius1)
		  	
		  	expect_equal(getRevisits(twoTracks, 0.5), rep(c(1, 1, 2, 1, 1), 2)) # middle pt overlaps
		  })

test_that("distance matrix",
		  {
		  	expect_equal(getRecursions(simplePts, 1, verbose = TRUE)$dists, simplePtsDist)
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

test_that("verbose",
		  {
		  	verbose = getRecursions(simplePts, 1, verbose = TRUE)
		  	nonVerbose = getRecursions(simplePts, 1, verbose = FALSE)
		  
		  })

test_that("revisit stats",
		  {
		  	stats = getRecursions(simplePts, 0.5)$revisitStats
		  	n = nrow(simplePts)
		  	
		  	expect_equal(stats$id, simplePts$id)
		  	expect_equal(stats$x, simplePts$x)
		  	expect_equal(stats$y, simplePts$y)
		  	expect_equal(stats$coordIdx, 1:n)
		  	expect_equal(stats$visitIdx, rep(1, n))
		  	expect_equal(stats$entranceTime, c(simplePts$t[1], simplePts$t[2:n] - as.difftime(30, units = "mins")))
		  	expect_equal(stats$exitTime, c(simplePts$t[1:(n-1)] + as.difftime(30, units = "mins"), simplePts$t[n]))
		  	expect_equal(stats$timeInside, as.difftime(c(0.5, 1, 1, 1, 0.5), units = "hours"))
		  	expect_equal(stats$timeSinceLastVisit, as.difftime(c(1, rep(NA, n)), units = "hours")[-1])
		  	
		  	
		  	stats2 = getRecursions(twoTracks, 0.5)$revisitStats
		  	expectedCoordIdx = c(1, 2, 3, 3, 4, 5, 6, 7, 8, 8, 9, 10)
		  	
		  	expect_equal(stats2$id, twoTracks$id[expectedCoordIdx])
		  	expect_equal(stats2$x, twoTracks$x[expectedCoordIdx])
		  	expect_equal(stats2$y, twoTracks$y[expectedCoordIdx])
		  	expect_equal(stats2$coordIdx, expectedCoordIdx)
		  	expect_equal(stats2$visitIdx, rep(c(1, 1, 1, 2, 1, 1), 2))
		  	expect_equal(stats2$entranceTime, twoTracks$t[expectedCoordIdx] - as.difftime(rep(c(0, rep(30, 5)), 2), units = "mins"))
		  	expect_equal(stats2$exitTime, twoTracks$t[expectedCoordIdx] + as.difftime(rep(c(rep(30, 5), 0), 2), units = "mins"))
		  	expect_equal(stats2$timeInside, as.difftime(rep(c(0.5, 1, 1, 1, 1, 0.5), 2), units = "hours"))
		  	expect_equal(stats2$timeSinceLastVisit, as.difftime(c(1, rep(NA, length(expectedCoordIdx))), units = "hours")[-1]) 
		  	
		  })

test_that("move objects",
		  {
		  	movePts = createMoveObj(simplePts)
		  	expect_equal( getRecursions(movePts, 1), getRecursions(simplePts,1) )
		  	
		  	moveStackPts = createMoveObj(twoTracks)
		  	expect_equal( getRecursions(moveStackPts, 1), getRecursions(twoTracks,1) )
		  	
		  })

# need to add some tests here for move and movestack, first figure out how to create
# add and test other versions of recurse
