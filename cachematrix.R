## This is the Assignment 2
## The makeCacheMatrix function stores a matrix and its inverse. The user must assess that the matrix is invertible.
## get() returns the matrix.
## set(x) stores the matrix.
## setinverse(inverse2) stores the inverse of the matrix.
## getinverse() returns the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse2) inverse <<- inverse2
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
  
  

}


## The cacheSolve function returns either the matrix inverse that is calculated
## or a cached value previously stored.


cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
