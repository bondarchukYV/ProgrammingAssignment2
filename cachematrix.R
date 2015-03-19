## This function creates special matrix, that has 4 methods
## get() - returns matrix
## set() - set value of matrix equal to input value and set inverse matrix equal to null
## setinverse() - set inverse matrix
## getinverse() - get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function check if inverse matrix is cached and return cached value.
## in other case return inverse matrix to matrix returned by x$get()

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

