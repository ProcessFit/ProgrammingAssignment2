# R Programming Assignment 2
# Objective is to create a cache of an inverse of a matrix, so it 
# doesn't need to be recalculated/
# This is done by taking advantage of R's lexical scoping rules:
# We define the object that will "hold" the value of the inverse at the same
# time that the matrix is created. 

# First function 'makeCacheMatrix' creates a special "matrix" which is 
# a list containing functions to:
#  -  get and set the value of the matrix
#  -  get and set the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        x
        # Initialise the inverse as NULL
        inv <- NULL
        # set allows the matrix to be changed, will NULL out the inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # get returns the matrix.
        get <- function() x
        # get and set inverse functions to hold and return the inverse
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        # return the functions as a list, e.g. can be called using the 
        # makeCacheMatrix$getinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


# cacheSolve calculates the inverse of the special "matrix"
# it first checks to see if the inverse has already been calculated.
# if the inverse already exists, it skips the calculation

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        inv <- solve(x$get(),...)
        x$setinv(inv)
        inv
}

