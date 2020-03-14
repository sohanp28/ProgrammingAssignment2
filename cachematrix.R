makeCacheMatrix <- function( m = matrix() ) {
  j <- NULL
  set <- function( matrix ) {
    m <<- matrix
    j <<- NULL
  }
  get <- function() {
    
    m
  }
  setInverse <- function(inverse) {
    j <<- inverse
  }
  getInverse <- function() {
    
    j
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setInverse(m)
  m
}

