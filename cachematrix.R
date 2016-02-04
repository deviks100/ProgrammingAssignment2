## These functions calculate the inverse of a matrix and cache the results.
## If the contents of the matrix don't change then the cached results can be used 
## rather than recomputing, improving efficiency for time consuming computations

## This function creates a list of the available get and set functions for this matrix
## and its inverse
makeCacheMatrix <- function(x = matrix()) {
  ##    1. set the value of the matrix
  ##    2. get the value of the matrix
  ##    3. set the value of the inverse
  ##    4. get the value of the inverse  
  
  inv <- NULL  ## inv is initialized
  set <- function(y) {
    ## takes the matrix passed in and uses it set a x and inv in parent environments
    x <<- y   
    inv <<- NULL
  }
  ## function that returns matrix x
  get <- function() x 
  
  ## sets inv in the parent environment to the matrix inverse
  setinverse <- function(inverse) inv <<- inverse 
  ## function that returns the inv in the parent env
  getinverse <- function() inv 
  
  ## a list of the functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function calculates the inverse of a given matrix.
## First it checks the cache to get the inverse of the matrix 
## If it is does not exist in the cache then it calculates the inverse of the matrix
cacheSolve <- function(x, ...) {
## Returns a matrix that is the inverse of 'x'

  ## gets the value of the inv in parent env
  inv <- x$getinverse()
  
  ## if not null, write a message that cached data is being returned and return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## otherwise get the matrix into temp variable data
  data <- x$get()
  ## calculate the inverse
  inv <- solve(data, ...)
  ## set the inv in parent environments to the inverse just calculated
  x$setinverse(inv)
  ## return the inverse of the matrix
  inv
}
