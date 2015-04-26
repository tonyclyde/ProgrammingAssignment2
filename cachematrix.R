# Simple functions for calculating the inverse of a square matrix
# with result caching optimization.

# makeCacheMatrix: 
#  This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(wrapped_matrix = matrix()) {

  cached_inverse <- NULL
  
  set <- function(new_matrix) {
    wrapped_matrix <<- new_matrix
    cached_inverse <<- NULL
  }
  
  get <- function() {
    wrapped_matrix
  }
  
  setInverse <- function(inverse) {
    cached_inverse <<- inverse
  }
  
  getInverse <- function() {
    cached_inverse
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

# cacheSolve: 
#  This function computes the inverse of the special "matrix" 
#  returned by makeCacheMatrix above. If the inverse has already been 
#  calculated (and the matrix has not changed), then the cachesolve should 
#  retrieve the inverse from the cache.
cacheSolve <- function(x) {
  
  inv <- x$getInverse()
  
  if(is.null(inv)) {
    message("calculating and caching solution")
    inv <- solve(x$get())
    x$setInverse(inv)
  }
  
  return(inv)
}
