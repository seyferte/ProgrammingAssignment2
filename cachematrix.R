## Project: rprog-002 - Programming Assignment #2
## Purpose: Create a list with 4 functions with the goal of
##          storing in a cache the inverse of a matrix

## TEST CASE: a <- matrix(c(4,3,3,2), nrow = 2, ncol = 2)

## makeCacheMatrix: Creates a list with four functions: set(), get(), setimx(), getimx() 
## which set and retrieve a matrix x as well as the inverse of x. The function can be passed
## a matrix.

makeCacheMatrix <- function(x = matrix()) {
  working_mx <- NULL
  set <- function(y) {
    x <<- y
    working_mx <<- NULL
  }
  get <- function() x
  setimx <- function(mx) working_mx <<- mx
  getimx <- function() working_mx
  list(set = set, get = get,
       setimx = setimx,
       getimx = getimx)
}


## cacheSolve: Takes a list created by makeCacheMatrix. It checks to see if the inverse matrix
## has been created. If so, it retrieves it from the passed list. If not, the inverse matrix is
## created within the list. WARNING: It is assumed only invertible matrices will be used.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  working_mx <- x$getimx()
  if(!is.null(working_mx)) {
    message("getting cached data")
    return(working_mx)
  }
  new_mx <- x$get()
  working_mx <- solve(new_mx, ...)
  x$setimx(working_mx)
  working_mx
}
