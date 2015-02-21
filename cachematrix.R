## Put comments here that give an overall description of what your
## functions do

# These functions will compute the inverse of non-singular matrix
# Usage: X is a non-singular square Matrix
# First make a special vector Xlist <- makeCacheMatrix(X)
# Now compute inverse by calling cacheSolve(Xlist), it will return the inverse matrix
# If you call the above function again, you will see that it returns the cached inverse
# instead of computing new one


## Write a short comment describing this function
# This function will make a special vector which will contain 4 functions
# to set or get the data and set or get the inverse
# Input = MATRIX (Non-Singular)
# Output = List of 4 functions
makeCacheMatrix <- function(x = matrix()) {
  INV <- NULL
  set <- function(y) {
    x <<- y
    INV <<- NULL
  }
  get <- function() x
  setINV <- function(solve) INV <<- solve
  getINV <- function() INV
  list(set = set, get = get,
       setINV = setINV,
       getINV = getINV)
}


## This function will return the inverse of non-singular inverse matrix
# Input is a special vector formed by calling s<-makeCacheMatrix(X)
# s is passed as argument
# Output is the inverse of X, it will be computed only on first call
# On all subsequent calls, cached value will be returned

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  INV <- x$getINV()
  if(!is.null(INV)) {
    message("getting cached data")
    return(INV)
  }
  data <- x$get()
  INV <- solve(data, ...)
  x$setINV(INV)
  INV
}
