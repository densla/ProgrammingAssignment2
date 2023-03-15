## Caches the Inverse of a matrix

## This function takes an optional argument x which can be used to initialize 
## the matrix. It creates a list of functions set, get, setInverse and 
## getInverse that can be used to manipulate and retrieve the matrix and its 
## inverse. The set function updates the matrix and resets the inverse to NULL. 
## The get function retrieves the matrix. The setInverse function sets the 
## inverse of the matrix. The getInverse function retrieves the inverse of the 
## matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function takes a matrix object created by makeCacheMatrix as its 
## argument. It first checks if the inverse of the matrix has already been 
## computed and cached using the getInverse function. If it has, it returns the 
## cached value and displays a message. If it hasn't, it computes the inverse 
## using the solve function and caches it using the setInverse function. 
## It then returns the inverse. The ... argument can be used to pass additional 
## arguments to the solve function.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

        ## Returns a matrix that is the inverse of 'x'
