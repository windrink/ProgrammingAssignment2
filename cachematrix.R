## The first function makeCacheMatrix create an object containing a matrix and its inverse. The inverse could be null if not calculated.
## The second function check the object created by the first function, if there is a saved inverse, it return the saved value, if not, calculate the inverse of the matrix in the object, save it and return it.

## makeCacheMatrix function create an object containing a matrix in "x" and its inverse in "inv". There are four actions, the first is "set" that save the matrix and initialize its inverse with NULL, the "get" is to get the matrix, the "getinverse" is to get the value saved in "inv", the "setinverse" save the inverse of the matrix in "x" in "inv".

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## the cacheSolve function try to read the inverse saved in "inv" of the object created by the first function. If the value is NULL, it calculate the inverse of the matrix saved in "x" and save it in "inv" and return, if it is not NULL, return the matrix saved in "inv".

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
