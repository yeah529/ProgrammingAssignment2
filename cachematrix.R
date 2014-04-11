## There are two functions following: makeCacheMatrix and cacheSolve, the pair functions that cache the inverse of a matrix
## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Return a list of four functions
  solved <- NULL
  
  set <- function(y) {
    x <<- y
    solved <<- NULL
  }
  
  get <- function() x
  
  set_inverse <- function(new) solved <<- new
  
  get_inverse <- function() solved
  
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##   If the inverse has already been calculated, then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  new <- x$get_inverse()
  
  ##Following part is to make sure whether the inverse of Matrix was calculated
  ##If it was calculated, get_inverse function would return non-null value
  if(!is.null(new)) {
    message("getting cached data")
    return(new)
  }
  
  ##If it has not been calculated, following code would be executed.
  data <- x$get()
  new <- solve(data, ...)
  x$set_inverse(new)
  new
}
