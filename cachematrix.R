#' This function creates a list of functions to 
#' cache matrix and inverse matrix
#' 
#' @return the list of cache
#' @examples 
#' x <- rbind(c(1,2), c(3,4))
#' y <- makeCacheMatrix(x)
#' 
makeCacheMatrix <- function(x = numeric()) {
  #Initializing the inverse matrix
  inv <- NULL
  
  #Set the value of matrix for caching
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #Return the value of cached matrix
  get <- function() {
    x
  }
  
  #Set the value of inverse matrix for caching
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  #Return the value of cached inverse matrix
  getinverse <- function() {
    inv
  }
  
  #Return the list of functions used as cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#' This function calculates matrix inverse.
#' It looks into cache and if available returns inverse matrix 
#' or computes inverse matrix if not available in cache
#' 
#' @return the inverse matrix
#' @examples 
#' x <- makeCacheMatrix(rbind(c(1,2), c(3,4)))
#' cacheSolve(x)
#' 
cacheSolve <- function(x, ...) {
  #Extract inverse value from the list
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Calculate inverse if no cache is available
  data <- x$get()
  inv <- solve(data, ...)
  
  # Set inverse matrix in the list
  x$setinverse(inv)
  
  # Return inverse matrix
  inv
}


