## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function
# Maintains the cache of matrix inverse values in a list. Gets/Sets the values appropriately.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	
	get <- function() x
	setinv <- function(solve) i <<- solve
	getinv <- function() i
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)
}


## Write a short comment describing this function
# Accepts a matrix, checks the input type being matrix and calculates inverse using Solve
# If the inverse has been already calculated, then it does not calculate again but provides the value from cache
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
	# Try getting the inverse
	i <- x$getinv()
	if(!is.null(i)) {
		message("Getting inverse from cached data")
		return(i)
	}
	
	# Get the actual matrix
	data <- x$get()
	
	# Calculate inverse
	i <- solve(data)
	
	# Set the inverse in cache
	x$setinv(i)
	
	# returning the calculated value
	i
}