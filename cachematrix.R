## This file contains 2 functions to cache the inverse of a matrix
## Assume that the matrix supplied is always invertible.

## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

testcase <- function() {
  mat <- matrix(c(1,2,3,4), 2, 2)
  x <- makeCacheMatrix(mat)
  print (cacheSolve(x))
  # ans should be
  # -2    1.5
  # 1     -0.5
  print (cacheSolve(x))
  # should get message "getting cached data"
}

#testcase()
