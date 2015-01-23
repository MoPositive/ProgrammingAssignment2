## These functions combine to create a separate environment
## where we will cache the inverse of a matrix

## This function will store the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function(inv) m
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function will calculate/store and/or retrieve
## the inverse matrix

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data...")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
