## These functions transform a a matrix into a list, 
## with the precalculated(cached) inverse value stored on it.
## You must always initialize your matrix with makeCacheMatrix
## before using on cacheSolve 
## Example usage:
## my_matrix=matrix(c(1,2,3,4), nrow=2)
## my_cached_matrix = makeCacheMatrix(my_matrix)
## cacheSolve(my_cached_matrix) # first call, will calculate
## cacheSolve(my_cached_matrix) # second call, will fetch from cache and print 'getting cache data'

## Creates a list containing 4 functions
##  get: returns the value passed as x 
##  set: sets the value stored inside, retrieved by x
##  setinverse: sets the stored inv value (you must pass it, this function does not calculate)
##  getinverse: returns the stored inv value.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'.
## If x already was calculated, fetch from cache
cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
