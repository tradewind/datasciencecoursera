## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix as argument and has the following functions,
## get(): return the matrix
## set(): set the matrix
## getInverse(): return the inverse of the matrix
## setInverse(): set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv_x <- NULL
	set <- function(y) {
		x <<- y
		inv_x <- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv_x <- inverse
	getInverse <- function() inv_x
	list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve checks first if there exists a cache of the invesrse of a matrix;
## if not, it would solve for the inverse of the matrix, cache it and then return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv_x <- x$getInverse()
	if(!is.null(inv_x)) {
		message("getting chached inverse matrix")
		return(inv_x)
	}
	matrix <- x$get()
	inv_x <- solve(matrix)
	x$setInverse(inv_x)
	inv_x
}
