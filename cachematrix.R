## The below functions used in conjuction with each othe, calculate the inverse of a matrix and store it in the cache
## If the matrix has already been calculated then it is pulled from cache and returned in the function call

## Creates a special vector (of functions) to be used by the cacheSolve function to compute the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## If not alredy done, computes and returns the inverse of the matrix. If done already, returns the inverse from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
