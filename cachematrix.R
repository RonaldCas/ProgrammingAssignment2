
#####################################################################
# makeCacheMatrix takes a ordinary matrix (x) and returns a
# special a new type of matrix, which can cache its inverse matrix 
# example:
#	x <- matrix(c(3,-2,2,1),nrow=2, ncol=2)
#	y <- makeCacheMatrix(x)
# 
#####################################################################
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	setData <- function(y)		# setting (new) matrix
	{
		x <<- y
		inverse <<- NULL			# y is a new matrix, so inverse is not yet known
	}
	getData <- function()			# retrieve matrix
	{
		x					# return matrix	
	}
	setInverse <- function(inv)	# caching inverse matrix
	{
		inverse <<- inv
	}
	getInverse <- function()		# retrieve inverse matrix (if present)
	{
		inverse
	}
	
	# return list of these 4 functions (note that names of list entries do not have to correspond literally
	# to the actual functions)
	list(
		get =	getData, 
		set =	setData, 
		getInv =	getInverse, 
		setInv = 	setInverse
		)

}



#####################################################################
# cacheSolve calculates the inverse of a matrix and stores the inverse
# 
# example:
#	x <- matrix(c(3,-2,2,1),nrow=2, ncol=2)
#	y <- makeCacheMatrix(x)
#	cacheSolve(y)
# 
#####################################################################
cacheSolve <- function(y, ...) 
{
	inv <- y$getInv()
	if (!is.null(inv))
	{
		# apparently the inverse already has been calculated for this matrix
		# return it immediately
		message("returning cached inverse..")
		return(inv)
	}
	# inverse was not cached and has to be calculated
	data <- y$get() 				# get matrix data
	inv <- solve(data,...)		# calculate inverse
	y$setInv(inv)				# cache inverse
	
	inv							# return inverse matrix	
}
