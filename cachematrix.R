## These functions will enable us to cache the inverse of a matrix
## rather than compute it every time.

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## i is the cached inverse
    i <- NULL
    set <- function(y)
    {
      ## If set is called, make sure to clear the cached inverse
      x <<- y
      i <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    list(
      set = set,
      get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}

## Computes the inverse of the special matrix object returned
## by makeCacheMatrix. If the inverse has been already calculated
## and cached, return it.

cacheSolve <- function(x, ...) {
    ## Check to see if inverse has already been cached
    i <- x$getinverse()
    if (!is.null(i))
    {
      message("getting cached data")
      return(i)
    }
    
    ## if not, generate inverse of matrix and store in cache
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
