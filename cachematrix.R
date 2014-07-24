## This is a group of functions for computing once and caching the inverse of a matrix rather than compute it repeatedly

##  Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the inverse is retrieved from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()           ## try to get inverse from cache
  if(!is.null(inv)) {             ## if found inverse in cache,
    message("getting cached data")
    return(inv)                   ## return cached inverse
  }
  data <- x$get()
  inv <- solve(data, ...)           ## calculate inverse of matrix
  x$setInverse(inv)
  inv
}
