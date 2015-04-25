
### Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the
## inverse of a matrix rather than compute it repeatedly.

## These two functions uses R's scoping rules to define functions
## and solves the inverse of the cached matrix using 
## build in solve() function in R.

## This function creates a special "matrix" object that 
## can cache its inverse, and which is really a list
## containing a function to
  # set the value of the matrix
  # get the value of the matrix
  # set the value of the inverse
  # get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  # However, it first checks to see if the inverse 
  # has already been computed.
  if(!is.null(m)) {
    message("Getting cached inverse matrix")
    # If so, it gets the inverse from the cache and skips
    # the computation
    return(m)
    
  }
  # Otherwise, it solves the inverse of the cached matrix using 
  # build in solve() function in R 
  data <- x$get()
  m <- solve(data, ...)
  
  # and sets the value of the inverse in the cache
  # via the setinverse function.
  x$setinverse(m)
  ## Return a matrix that is the inverse of 'x'
  m
}