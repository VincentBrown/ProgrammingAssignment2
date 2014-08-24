## These functions create an object which stores a matrix with get and set functions
## to access the stored matrix and it's cached inverse, if it's been calculated.  The 
## cacheSolve function checks if the object already has a cached inverse and if it doesn't, 
## it calculates the inverse and caches it and returns it

## makeCacheMatrix stores a matrix and it's inverse and provides get and set methods

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## checks an object to see if it has a cached matrix inverse.  If it does, it returns it,
## if it doesn't, it calculates the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
