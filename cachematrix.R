## The following two functions implement matrix inverse calculation and the value caching
## the makeCacheMatrix is used to store the value while the cachesolve is used to call the 
## makeCacheMatrix function and to get the value from cache (if available)

## This first function creates a list of functions that can be called when needed
## to obtain the inverse value of specific matrix


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


## The next function is called with the argument x, so you first run the makeCacheMatrix and store the output to variable x

cachesolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
# example of using functions above, this is matrix that can be inversed:
# mdat <- matrix(c(3,1, 4,2), nrow = 2, ncol = 2, byrow = TRUE)
# x <- makeCacheMatrix(mdat)
# cachesolve(x)
# cachesolve(x)
# the second cachesolve function call will produce output with cached data
