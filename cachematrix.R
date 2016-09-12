## The function makeCacheMatrix creates an object which stores a matrix and its inverse.  
## The function cacheSolve uses the object to return a cached inverse, if available, and compute and cache it 
## otherwise.

## Sample usage:
## > x <- matrix(c(1,2,3,4), nrow=2, ncol=2)
## > xlist <- makeCacheMatrix(x)
## > xinverse <- cacheSolve(xlist)
## > xlist$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > xinverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > xlist$get() %*% xinverse
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1

## This function accepts a matrix and creates a list with four members:
## get() returns the matrix.
## set() sets the matrix on the object and invalidates its cached inverse.
## getinverse() returns the cached inverse of the matrix.
## setinverse() sets the cached inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function uses the list created by makeCacheMatrix to return a cached inverse, if available, and
## compute, cache, and return the inverse if not already available.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
