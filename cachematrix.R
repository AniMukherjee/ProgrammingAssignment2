# Matrix inversion is usually a costly computation and there may be some benefit
# by caching the inverse of a matrix rather than compute it repeatedly. These
# two functions are used to cache the inverse of a matrix instead of recomputation.

## Would take as input an invertible matrix and return
## a complex object which contains functions for storing
## its value and inverse 

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Would take as input a makeCacheMatrix object
## Will compute the matrix inverse
## if it is not there already in cache of passed object

cacheSolve <- function(x, ...) {
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinv(xinv)
  xinv
}

## Example Test case:
## ani <- makeCacheMatrix(new matrix(c(1, 1, 1, 3, 4, 3, 3, 3, 4),3))
## cacheSolve(ani)
##       [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1
##
## cacheSolve(ani)
## getting cached data
##       [,1] [,2] [,3]
## [1,]    7   -3   -3
## [2,]   -1    1    0
## [3,]   -1    0    1
##
