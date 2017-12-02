## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The following function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the matrix
  get <- function() x
  
  # Set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # get the inverse of the matrix
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 
# been calculated (and the matrix has not changed), then the 
# cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv
}


