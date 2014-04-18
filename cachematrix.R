## makeCacheMatrix creates a matrix object that can cache the inverse
## of the given matrix.
## 
## cacheSolve caches and computes the inverse of the given matrix.
##
## For example:
## > m1 <- matrix(c(2,0,0,0,2,0,0,0,2), nrow=3, ncol=3)
## > cm1 <- makeCacheMatrix(m1)
## > cacheSolve(cm1)
##      [,1] [,2] [,3]
## [1,]  0.5  0.0  0.0
## [2,]  0.0  0.5  0.0
## [3,]  0.0  0.0  0.5
## > cacheSolve(cm1)
## Getting cached inverse.
##      [,1] [,2] [,3]
## [1,]  0.5  0.0  0.0
## [2,]  0.0  0.5  0.0
## [3,]  0.0  0.0  0.5

## makeCacheMatrix creates a matrix object that can cache the inverse
## of the given matrix.

makeCacheMatrix <- function(x = matrix()) {
   # Intialize the inverse.
   inverse <- NULL
   # Getter for the matrix.
   get <- function() x
   # Setter for the matrix.
   set <- function(mat) {
      x <<- mat
      inverse <<- NULL 
   } 
   # Getter for the inverse.
   getinverse <- function() inverse
   # Setter for the inverse.
   setinverse <- function(inv) inverse <<- inv
   list(get = get, set = set,
        getinverse = getinverse,
        setinverse = setinverse)
}


## cacheSolve computes the inverse of the given matrix returned by
## makeCacheMatrix.  If its inverse has already been calculated,
## the inverse is retrieved from the cache and returned.  O/W,
## cacheSolve will compute the inverse and cache it in the given
## matrix.

cacheSolve <- function(x, ...) {
    # Retrieve cached inverse.  If available, simply return it.
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
       message("Getting cached inverse.")
       return(inverse)
    }    
    # There is no inverse.  Compute, cache and return it.
    mat <- x$get()
    inverse <- solve(mat, ...)
    x$setinverse(inverse)
    inverse
}
