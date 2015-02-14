## Put comments here that give an overall description of what your
## functions do

# function makeCacheMatrix(matrix)
#  make cacheMatrix
#
# function cacheSolve(cacheMatrix)
#  returns the inverse matrix of the cacheMatrix
#  if already calculated, returns the cached matrix.

## Write a short comment describing this function

# function makeCacheMatrix(matrix)
#  make cacheMatrix
#
#  $set(matrix) : set matrix
#  $get()       : get matrix
#  $setinverse() : set inverse matrix
#  $getinverse() : get inverse matrix

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


## Write a short comment describing this function

# function cacheSolve(cacheMatrix)
#  returns the inverse matrix of the cacheMatrix
#  if already calculated, returns the cached matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
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
