makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #sets the value of m to null
  set <- function(y) {
    x <<- y
    m <<- NULL
  } # set the value of the matrix
  get <- function() x # get the value of the maxtrix
  setinv <- function(solve) m <<- solve # set the value of the inverse
  getinv <- function() m # get the value of the inverse
  list(set = set, get = get,setinv = setinv,getinv = getinv)
}

cacheSolve <- function(x, ...) {

  m <- x$getinv()  # get inverse of matrix if it already exits
  if(!is.null(m)) {
    message("getting cached data")
    return(m) # check that matrix hasn't changed, and if it hasn't, sends a text message and returns the cached matrix
      }
  data <- x$get() # get the value of the input matrix
  m <- solve(data, ...) # compute the value of the inverse of the input matrix
  x$setinv(m)  # cache the inverse
  m
}
