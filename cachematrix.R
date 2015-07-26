##  The two functions below are written to cache the inverse of an invertible square matrix.

## The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
    m <- NULL
    set <- function(y) { ##It sets the value of the matrix x
      x <<- y
      m <<- NULL
    }
    get <- function() x  ##It gets the value of the matrix x
    setinv <- function(inv) m <<- inv ##It sets the value of the inverse of matrix x
    getinv <- function() m  ##It gets the value of the inverse of matrix x
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }



## The second function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above and retrieves the inverse from the cache if it had been calculated previously

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data") ##Returns a the message "getting cached data"
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)## computes the inverse of the square invertible matrix
    x$setinv(m)
    m
  }
  ## Return a matrix that is the inverse of 'x'

