## Create a special version of a matrix to be able to use a cache
# function in order to save computing time when having to calculate repeatedly
# the inverse of a matrix
#
# usage example:
# 1.- create the target matrix to using "makeCacheMatrix" function
#     > m <- makeCacheMatrix(matrix(c(1,2,3,4),2,2))
#
# 2.- use "cacheSolve" to calculate the inverse. If matrix inverse has been 
#     previously calculated, that value will be returned.
#     > cacheSolve(m)
#     [,1]  [,2]
#     [1,] -0.2  0.15
#     [2,]  0.1 -0.05
#     > cacheSolve(m)
#     getting cached data
#     [,1]  [,2]
#     [1,] -0.2  0.15
#     [2,]  0.1 -0.05



## Create a special matrix version to save it's inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
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
