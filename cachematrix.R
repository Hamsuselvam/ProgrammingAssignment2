## The following function will cache the inverse of matrix object.

makeCacheMatrix <- function(x = matrix()) {
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    ## Creates special matrix object        
    inverse_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse)
    inverse_x <<- inverse
  getinverse <- function() inverse_x
  ## Special Vector contains list that sets the value of matrix and inverse of it and gets the values
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function retrieves the inverse of the matrix object from cache.

cacheSolve <- function(x, ...) {
  ## Obtains the inverse of matrix object
  inverse_x <- x$getinverse()
  ## If it already has the inverse of matrix, it gets the cached data       
  if(!is.null(inverse_x)) {
    message("Obtaining cached inverse of matrix")
    return inverse_x
  } 
  ## Otherwise it computes the inverse of the matrix
  else {
    ## Solve function computes the inverse of the matrix        
    inverse_x <- solve(x$get())
    ## Sets the computed value from solve function to inverse_x        
    x$setinverse(inverse_x)
    return(inverse_x)
  }
}
