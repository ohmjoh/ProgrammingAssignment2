## makeCacheMatrix creates a matrix which is a list containing a function to 
## set/get the value of the matrix and set/get the value of the inversed matrix.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversion <- function(inversion) m <<- inversion
  getinversion <- function() m
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}


## cacheSolve calculates the inverse matrix of the matrix created
## with makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinversion()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversion(m)
  m
}
