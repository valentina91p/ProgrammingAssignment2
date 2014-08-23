## Functions that cache and calcute the inverse of a given matrix

## This function allow us to cache the inverse value 
## for a given matrix and save the matrix as well.
## We have the option to pass it the matrix value when created or to assign 
## it later with setMatrix.
## We also can assign tha inverse matrix value, since it's not calculated here

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	setMatrix <- function(y){
		x <<- y
		inv <<- NULL
	}
	getMatrix <- function() x
	setInverse <- function(i){
		inv <<- i
	}
	getInverse <- function() inv

	list(getMatrix=getMatrix,
		setMatrix=setMatrix,
		getInverse=getInverse,
		setInverse=setInverse)
}


## This function calculates the inverse matrix of x, first it looks for the 
## inverse in makeCacheMatrix object, if found it returns that, if not it 
## calculates it and assigns it in makeCacheMatrix object.

cacheSolve <- function(x, ...) {
    if(!is.null(x$getInverse())){
    	return(x$getInverse())
    }
    i <- solve(x$getMatrix())
    x$setInverse(i)
    i
}
