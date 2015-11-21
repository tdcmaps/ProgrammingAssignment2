## makeCacheMatrix takes an input matrix (that must be invertible) and returns a matrix that will cache a copy of its inverse and return the cached copy
## on subsequent calls to the function cacheSolve. makeCacheMatrix returns a list, and this list in the input to cacheSolve. 
## the matrix inverse is computed by cacheSolve, if necessary. makeCacheMatrix provides the mechanism to avoid re-computing the same inverse result.
## I tested these functions with a variety of square matrices created by using runif() to generate random numbers. In all cases that I tests (up to 200 x 200 matrix), inverting the matrix took very little time. So I assume the matrix must be very large if computing the inverse will be time consuming.
## functions do


makeCacheMatrix <- function(x = matrix()) {
  # test for square matrix that is invertible

  if( nrow(x) != ncol(x)) { return (" matrix must be a square matrix to compute inverse") }

  mi <- NULL
  # "set" puts the inverse matrix (y) into xi
  set <- function(y) {
    xi <<- y 
    mi <<- NULL
  }
  # "get" returns the data matrix(square) for computation
  get <- function() x 
  # "setInverse" invokes the solve funcion and saves the results in m
  setInverse <- function(solve) mi  <<- solve
  # "getInverse" simply returns the inverted matrix (kept in the variable mi)
  getInverse <- function() mi
  # return the group of manipulating functions as a list
  list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve computes the inverse of matrix by calling the "solve" function. But it uses the list functions
## of makeCacheMatrix to store the computed inverse and cache it in case the inverse is needed again.

# cachesolve computes the matrix inverse of a matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  # if the inverse has been computed, return it
  mi <- x$getInverse()
  if(!is.null(mi)){
    message("returning cached inverse")
    return (mi)
  }
  # otherwise compute the inverse
  data <- x$get()
  mi <- solve(data, ...)
  x$setInverse(mi)
  mi
}

