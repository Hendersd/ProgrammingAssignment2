## The first function, makeVector creates a special "vector"
## which is really a list containing a function to 
## 1. set the value of the vector
## 2.get the value of the vector
## 3.set the inverse of the matrix
## 4.get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <-  function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## The following function calculates the inverse of the special "vector" created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computuation. Otherwise it calculates the inverse and sets the value of
## the inverse via the the setinverse function.

cacheSolve <- function(x, ...){
  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}