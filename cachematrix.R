## makeCacheMatrix(): creates a matrix that can cache its inverse.
## cacheSolve(): solves the inverse of the matrix created by makeCacheMatrix(). If inverse has already been solved it is retrieved instead from the cache.

makeCacheMatrix <- function(x = matrix()) {
	inverse = NULL
	set = function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inverse <<- inverse
	getinverse <- function() inverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
	inverse = x$getinverse()
	if(!is.null(inverse)){
		message("Getting cached data")
		return(inverse)
	}
	calcinverse = x$get()
	inverse = solve(calcinverse,...)
	x$setinverse(inverse)
	return(inverse)
	}
}
