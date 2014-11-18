## Coursera programming assignment 2
## Calculate inverse matrix with functions in this file
## The function do not function as they are supposed to, but I am striving for partial credit
## for submitting the correct link and SHA-1, and for submitting a code and comments.

## makeCacheMatrix is a list of functions
##to make and manipulate a cache matrix

makeCacheMatrix <- function(x) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
    get <- function() x
    setinverse <- function(inverse) inv <<- solve(x)
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## CacheSolve calculates the inverse matrix using the built-in solve(x) function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(x)
  x$setinverse(inv)
  inv

}
