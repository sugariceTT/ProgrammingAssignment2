## Below is a pair of functions that cache the inverse of a matrix

## Create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  ## Retrieve cache 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## If not cached, calculate inversion matrix now
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
