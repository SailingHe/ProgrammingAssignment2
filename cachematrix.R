## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix makes a special matrix object which can store a matrix and its inverse. In fact the
## new object is a list with functions to 
## set the matrix values, get the matrix values, set the inverse and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    if ((nrow(y) == ncol(y)) && (det(y) != 0)) {   ##matrix must be square and determinant must be greater then 0
        +                x <<- y
        +                m <<- NULL        ##if matrix changes the cached inverse (if any) isn't correct anymore 
        +        }
    +        else {
      +            message("matrix must be square and its determinant != 0")
    }
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## Write a short comment describing this function
## cacheSolve retrieves the inverse of special matrix m from the cache if it is already computed and the matrix
  +## hasn't changedsince then. Otherwise it (re-)computes the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
