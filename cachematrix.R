## Coursera's R Programming, Week 3, Programming Assignment 2
## Class of August, 2020

cat("\014")  
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

## This function creates a matrix object capable of storing the original version and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of thematrix created with makeCacheMatrix. If the inverve is cached, it is automatically recovered.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}       


## Testing my functions

minhamatriz <- makeCacheMatrix(matrix(1:4, 2, 2))
minhamatriz$get()
minhamatriz$getInverse()
cacheSolve(minhamatriz)
minhamatriz$getInverse()
cacheSolve(minhamatriz)

