## contains two funsctions, first one is used to create matrix that catch it;s reverse, and second is used to caluclate the mean

## This function makeCacheMatrix is used to create a special matrix which can catch it's reverse
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


##  this function cacheSolve calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
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
