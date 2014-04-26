## makeCacheMatrix() creates a special matrix. It is a list containing 
## a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Return a matrix that is inverse of 'x'
## First time it is called, makes a computation.
## If cacheSolve() was called before, then returns cached data.

cacheSolve <- function(x, ...) {
  
    inv <- x$getinverse()
    
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## Example:
mat <- makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(mat) ## it should make the computation for solve(mat)
cacheSolve(mat) ## it should return cached data