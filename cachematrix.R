## These functions will enable us to cache the inverse of a matrix
## rather than compute it every time.

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y)
    {
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
    i <- x$getinverse()
    if (!is.null(i))
    {
      message("getting cached data")
      return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
