## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setsolve <- function(solve) m <<- solve
   getsolve <- function() m
   list(set = set, get = get, 
        setsolve = setsolve, 
        getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
   s <- x$getsolve()
   if (!is.null(s)) {
      message("getting cached data")
      return(s)
   }
   data <- x$get()
   x$setsolve(solve(data, ...))
}
