## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Allows for the creation & retrieval of special "matrix" objects and their cached inversed counterparts.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function() i <<- solve(x) # solve(matrix) returns the inverse of a matrix
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
# Returns the inverse of the matrix produced in the "makeCacheMatrix" function unless the inverse has already been computed, in
# which case the inverse of the matrix shall be retrieved from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  i <- solve(x$get(), ...)
  x$setinverse(i)
  i
}
