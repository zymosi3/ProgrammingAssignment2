## This file contains functions which allow to cache result of 
## computation of matrix inversion
##
## Functions:
## 1. makeCacheMatrix: This function creates a special "matrix" 
##                     object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special
##                "matrix" returned by makeCacheMatrix above. If the 
##                inverse has already been calculated (and the matrix
##                has not changed), then the cachesolve should retrieve
##                the inverse from the cache.
##
## Example:
##   > amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##   > cacheSolve(amatrix)
##   [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5

## This function creates a special "matrix" object that can cache its inverse.
##
## Args:
##   x: The matrix. Optional, empty matrix by default.
##
## Returns:
##   A special "matrix object". This object contains the next functions:
##   set - sets matrix
##   get - gets matrix
##   setInverse - sets inverse of matrix
##   getInverse - gets inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
##
## Args:
##   x: The matrix to solve
##   ...: other arguments for solve() 
##
## Returns:
##   The inverse of x
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
