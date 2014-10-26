## This file contains 2 functions.  makeCacheMatrix provides the ability to calculate the
## inverse of a mtrix & cache it in the current environment.  cacheSolve uses the
## makeCacheMatrix to return the cached inverse of a metrix if it exists in the current
## environment, otherwise it is calculated & cached.

## This function, makeCacheMatrix creates a special "matrix", which is really a list
## containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## initialize the inverse of the matrix
  myInverse <- NULL
  ## define the "set" function
  set <- function(y) {
    x <<- y
    myInverse <<- NULL
  }
  ## define the "get" function
  get <- function() x
  ## define the "setinverse" function by extending solve to set the cached inverse
  setinverse <- function(solve) myInverse <<- solve
  ## define the "getinverse" function as returning the cached inverse
  getinverse <- function() myInverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function, cacheSolve, computes the inverse of the special "matrix" returned
## by makeCacheMatrix.  If the inverse has already been calculated (and the matrix
## has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Get the matrix inverse from the environment
  myInverse <- x$getinverse()
  ## Return the cached inverse value if it exists
  if(!is.null(myInverse)) {
    message("getting cached data")
    return(myInverse)
  }
  ## If the cached inverse didn't exist, set the matrix data value
  data <- x$get()
  ## Set the inverse of the matrix
  myInverse <- solve(data, ...)
  ## Cache the inverse of the matrix
  x$setinverse(myInverse)
  myInverse
}
