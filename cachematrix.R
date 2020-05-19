## My test matrix is m1 and the inverse of Matrix m1, i called it n1

## makeCacheMatrix is a function that stores a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  m1 <- NULL
  set <- function(y) {
    x <<- y
    m1 <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m1 <<- solve
  getinverse <- function() m1
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function that requires an argument from makeCacheMatrix() to retrieve the inversed value
## that is store in makeCacheMatrix object's environment

cacheSolve <- function(x, ...) {
  m1 <- x$getinverse()
  if(!is.null(m1)) {
    message("getting cached data")
    return(m1)
  }
  data <- x$get()
  m1 <- solve(data, ...)
  x$setinverse(m1)
  m1
}

m1 <- matrix(c(2, 4, 1, 3), nrow = 2, ncol = 2)
n1 <- makeCacheMatrix(m1)
cacheSolve(n1)
cacheSolve(n1)
