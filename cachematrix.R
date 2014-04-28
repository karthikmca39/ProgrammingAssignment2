## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setMatrix <- function(y) {
        x <<- y
        inverse <<- NULL
    }
  
    getMatrix <- function() x
    setInverse <- function(solve) inverse <<- solve ## solve(X) will return inverse
    getInverse <- function() inverse
  
    ## List of matrix containing given matrix set & get and 
    ## inverse matrix get & set. 
    list(setMatrix = setMatrix, getMatrix = getMatrix,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve will retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) { ## Checking whether cache already there or not.
      return(inverse)
    }
    
    data <- x$getMatrix()
    inverse <- solve(data, ...) ## solve(X) will return inverse
    x$setInverse(inverse)
}
