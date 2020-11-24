## Matrix inversion is usually a costly computation 
##and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly
## (there are also alternatives to matrix inversion). 
##This assignment is to write a pair of functions that cache the inverse of a matrix.

##makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.
##The getInverse function does inverse of the matrix
## makeCacheMatrix returns a list with 4 functions.
## This function makeCacheMatrix should be called first.
## Note that the matrix whose inverse is desired may
## optionally be passed to makeCacheMatrix.
## The returned list can be used to get and set the matrix m
## or get or set the the inverse of m.
## Additionally, the returned list may be passed to
## cacheSolve which either computes the inverse
## or, if the inverse of m has already been computed,
## the cached inverse of m.

makeCacheMatrix <- function(x = matrix()){
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() {x}
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##cacheSolve: This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. 
##For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## It is important to note that cacheSolve
## expects as its sole argument a list which
## is returned from makeCacheMatrix.

cacheSolve <- function(x, ...){
      inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat, ...)
      x$setInverse(inv)
      inv
}