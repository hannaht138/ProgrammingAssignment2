## These functions definie a way to cache the calculation of a matrix inverse


## This function creates a matrix structured to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  iMatrix <- NULL
  
  set <- function(y){
      x <<- y
      iMatrix <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inv) iMatrix <<- inv
  getInverse <- function () iMatrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function computes the inverse of a matrix of the makeCacheMatrix format, using the cached value if possible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    iMatrix <- x$getInverse()
    if(!is.null(iMatrix)) {
          message("Getting cached data")
          return(iMatrix)
    }
    
    data <- x$get()
    iMatrix <- solve(data)
    x$setInverse(iMatrix)
    iMatrix
}
