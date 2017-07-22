## makeCacheMatrix function creates a matrix that is capable of caching its own inverse


makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve function creates a cached matrix
## and computes the inverse of the matrix if the inverse has not been previously cached
## if the inverse has not been previously computed the inverse is computed
## and the function passes it to the cached matrix for caching

cacheSolve <- function(x, ...) {
  a <- x$getinverse()
  if(!is.null(a)) {
    message("retrieving the cached data")
    return(a)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
