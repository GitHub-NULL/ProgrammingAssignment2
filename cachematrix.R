## The following two functions cache the inverse of a matrix x

## The following function makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {  ## set the values of x and m
    x <<- y
    m <<- NULL
  }
  get <- function() x ## get x value
  setinv <- function(inv) m <<- inv  ## set inverse x value
  getinv <- function() m ## get inverse x value
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)                        
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## try to obtain inverse matrix
  m <- x$getinv()
  if(!is.null(m)) {
    ## return cached data
    message("getting cached data")
    return(m)
  }
  ## obtain a matrix
  data <- x$get()
  ## inverse a matrix 
  m <- solve(data, ...)
  x$setinv(m)
  ## return inverse matrix
  m
}
