## makeCacheMatrix is a function that creates a special matrix object
## that can cache its inverse.

## cacheSolve is a function that computes the inverse of the special matrix
## returned by makeCacheMatrix. If inverse of the matrix has been calculated,
## cacheSolve will retrieve the inverse from the cache.

## to create a matrix and to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## to calculate the inverse or to get the inverse from makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return (m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
