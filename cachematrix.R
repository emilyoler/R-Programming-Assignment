
## This function sets the value of the matrix, gets the value of the matrix, 
## sets the value of the inverse, then gets the value of the inverse. 

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get = function()x
	setinv = function(inverse) i <<- inverse
	getinv <- function() i 
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## This function calculates the inverse of the makeCacheMatrix() matrix found 
## above, where x is the output of the makeCacheMatrix(). If the inverse has 
## already been calculated and matrix has not changed, then cacheSolve 
## retreives the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinv()
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinv(i)
	return(i)
}
