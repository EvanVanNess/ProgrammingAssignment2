## We calculate the inverse of a matrix and save it to the cache in order to save possible future calculation


## The makeCacheMatrix sets a matrix, gets a matrix, sets its inverse, and gets its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }


## We check to see if the inverse has already been calculated.  If so, we return it.  If not, then we calculate the inverse.m

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
  }

