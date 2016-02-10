## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setmatrix <- function(inpmatrix) invmatrix <<- inpmatrix
  getmatrix <- function() invmatrix
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix funtion. If the inverse has already been 
## calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  m <- x$getmatrix()
  if(!is.null(m)) {
        return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  m
}
