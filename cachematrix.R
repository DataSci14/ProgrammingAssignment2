## I've made two functions, one that has ability to cache inverse of a matrix object 
## the second function computes the inverse of the matrix object that has been returned from the first function.
## If the inverse has been computed and no changes exist the second function will display the inverse from cache.


## The following function will create a matrix which sets and gets values for 
## a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function() m <<- solve(x)
	getinverse <- function() m
	list(set = set, get = get, 
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function will compute the inverse of a matrix above and store the result.  If the inverse has previously
## been calculated it will skip the calculation and retrieve and display the previously stored result.

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
