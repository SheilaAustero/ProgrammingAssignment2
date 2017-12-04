## Below are two functions that are used to create a special object
## that stores a matrix and cache's its inverse.

## The first function makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## Set the value of the matrix
## Get the value of the matrix
## Set the value of the inverse of the matrix
## Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function calculates the inverse matrix of the special "matrix"
## created with the above function.
## It first checks to see if the inverse of the matrix has already been calculated. 
## If so, it gets the inverse of the  matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse matrix of the data and 
## sets the value of the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
