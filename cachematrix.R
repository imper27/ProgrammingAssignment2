## This script constructs a pair of functions 
## which togehter implement a cache for the inverse of a matrix.
## The first function, makeCacheMatrix, takes the original matrix as imput.
## This function constructs the matrix cache and returns it, as a list of four functions.
## The second function, cacheSolve, takes the cache object as input.
## It returns the inverse of the matrix, either from the cache, or by computing it.

## This function, makeCacheMatrix, constructs a cache object for the inverse of a matrix.
## It takes the original matrix as input.
## It then declares a local variable, m, to hold the inverse of the matrix.
## It constructs four functions, getters and setters for the matrix and its inverse.
## The local variable m, which holds the inverse matrix, is thus in the environment 
## of the getter and setter functions, and so is accessinble to them.
## The function makeCacheMatrix returns the getter and setter functions as a list.
makeCacheMatrix <- function(x = matrix()) {
    # This variable will hold the inverse of x.
    m <- NULL
    # Set the matrix, x, and unset the inverse.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # get the matrix
    get <- function() x
    # set the inverse
    setinverse <- function(inverse) m <<- inverse
    # get the inverse
    getinverse <- function() m
    # return the four functions as a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function, cacheSolve, takes as input the output from makeCacheMatrix.
## It returns the inverse of the cached matrix.
## It uses the getter and setter functions to first check if the inverse is
## available in the cache.
## If it is, cacheSolve returns the cached inverse.
## If the cache is empty, cacheSolve obtains the inverse, using the function "solve".
## It caches the result, and then returns it.
cacheSolve <- function(x, ...) {
    # get the inverse from the cache
    m <- x$getinverse()
    # check if the inverse is null
    # if not, return it, after printing an informative message
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # if the cache does not contain the inverse
    # get the cached matrix
    data <- x$get()
    # obtain its inverse using dolve
    m <- solve(data, ...)
    # cache the inverse
    x$setinverse(m)
    # return the inverse
    m
}
