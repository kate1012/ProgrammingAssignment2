## calculates the inverse of a matrix and stores value in cache 
## for faster computation

## creates getters/setters of a given matrix and its inverse value
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL 
  
  set <- function(m) {
    x <<- m
    inverse <<- NULL
  }
  
  get <- function() {
    x
  }
  
  getInverse <- function() {
    inverse
  }
  
  setInverse <- function(i) {
    inverse <<- i
  }

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## checks if the inverse is already in cache - returns cached value or 
## computed inverse 
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  m <- x$get()
  inverse <- solve(x$get())
  x$setInverse(inverse)
  
  inverse
}

