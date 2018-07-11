
## This function caches the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solveMatrix) m <<- solveMatrix
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function solves the inverse of the matrix made by makeCacheMatrix

cacheSolve <- function(x, ...){
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
