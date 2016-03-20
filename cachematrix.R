## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## Note: we assume that the matrix supplied is always invertible

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## returns original matrix
  get <- function() x
  
  ## set inverse matrix m
  setmatrix <- function(solve) m <<- solve
  
  ## if exists, returns inverse matrix m
  getmatrix <- function() m
  
  list(set = set, 
       get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatrix()
  
  ## in case m is not null, return cached matrix...
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## ...else, we calculate and store matrix
  data <- x$get()
  m <- solve(data)
  x$setmatrix(m)
  
  ## return inverse matrix m
  m
}
