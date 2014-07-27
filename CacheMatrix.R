## This assignment deals with the problem of speeding up
## computations by retrieving the results of certain computations
## from the cache.


## makeCacheMatrix functions enables setting and getting
## the values for a variable containing a matrix.
## Since the inverse function is computationally expensive,
## it helps to retrieve from the memory where this value has already been assigned
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<-solve
	getsolve <- function() m
	list(set = set, get = get, 
		setsolve = setsolve, 
		getsolve = getsolve)
	
}


## The cacheSolve function calculates the inverse of a matrix
## Alternatively, this  function obtains the variable from the cache

cacheSolve <- function(x, ...) {
	m <- x$getsolve()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
      }
	data <- x$get()
	m <- solve(data, ...)
	x$setsolve(m)
	m
}