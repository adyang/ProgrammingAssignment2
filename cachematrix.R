makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(get = get, set = set,
       getInverse = getInverse,
       setInverse = setInverse)
}

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (is.null(i)) {
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
  } else {
    message("getting cached data") 
  }
  i
}
