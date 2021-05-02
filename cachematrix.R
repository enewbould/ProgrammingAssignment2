## makeCacheMatrix caches the inverse of a square, invertible matrix
## by creating a vector to
## 1. set the value of the vector 
## 2. get the value of the vector
## 3. set the inverse value of the matrix
## 4. get the inverse value of the matrix

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv<<-solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed) then
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  message("caching data")
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


