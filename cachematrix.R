## Coursera programming assignment 2
## Calculate inverse matrix with functions in this file using cache

## makeCacheMatrix is a list of 4 functions
## these help manage the two inverse values (one in local, other in global envrironment)
## Also displays the matrix and its inverse to screen.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) { # caches the inverse globally
          x <<- y
          inv <<- NULL
  }
    
  get <- function() x # returns original matrix
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv # returns inverse of matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve calculates the inverse of matrix unless the 
## inverse is already available in the cache. It evokes the
## functions encompassed in makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message ("getting cached data")
    return(inv)  
  }
      data <- x$get() ## data is a dummy variable for performing the calculation
      inv <- solve(data,...) ## solve-function automatically calculates inverse
      x$setinverse(inv) ## the inverse is forwarded to another function defined in makeCacheMatrix
      inv ## display
}

