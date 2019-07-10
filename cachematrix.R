makeInverse <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) Inv <<- Inverse
  getInverse <- function() Inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

  
  cacheInverse <- function(x, ...) {
    Inv <- x$getInverse()
    if(!is.null(Inv)) {
      message("getting cached data")
      return(Inv)
    }
    message("saving cached data")
    data <- x$get()
    Inv <- solve(data, ...)
    x$setInverse(Inv)
    Inv
  }
  