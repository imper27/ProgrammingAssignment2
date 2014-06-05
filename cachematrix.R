## This script constructs a pair of functions 
## which implement a cache for the inverse of a matrix.
## The first function, makeCacheMatrix, takes the original matrix as imput.
## It then declares a local variable, m, to hold the inverse of the matrix.
## It constructs four functions, getters and setters for the matrix and its inverse.
## The local variable m, which holds the inverse matrix, is thue in the environment 
## of the getter and setter functions, and so is accessinble to them.
## The function makeCacheMatrix returns the getter and setter functions as a list.

## The second function, cacheSolve, takes as input the output from makeCacheMatrix.
## It returns the inverse of the matrix.
## It uses the getter and setter functions to first check if the inverse is
## available in the cache.
## If it is, cacheSolve returns the cached inverse.
## If the cache is empty, cacheSolve obtains the inverse, using the function "solve".
## It caches the result, and then returns it. 

## This function constructs the matrix cache and returns it, as a list of four functions.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes the cache object as input
## and returns the inverse of the matrix, 
## either from the cache, or by computing it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
