## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## As Assignment says: 
## makeCacheMatrix: This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(matrix = matrix()) {
      invvMatrix = NULL;
      setMatrix = function(otherMatrix) {
            matrix <<- otherMatrix
            invvMatrix <<- NULL
      }
      
      getMatrix = function() matrix
      
      setinvMatrix = function(inverse) invvMatrix <<- inverse
      
      getinvMatrix = function() invvMatrix
      
      list (setMatrix = setMatrix,
            getMatrix = getMatrix,
            setinvMatrix = setinvMatrix,
            getinvMatrix = getinvMatrix
            )
}


## Write a short comment describing this function
##  As Assignment says:
## cacheSolve: This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
## Return a matrix that is the inverse of 'matrix'


cacheSolve <- function(matrix, ...) {
      invvMatrix = matrix$getinvMatrix()
      if(!is.null(invvMatrix)){
            return(invvMatrix)
      }
      
      dataMatrix = matrix$getMatrix()
      invvMatrix = solve(dataMatrix)
      
      matrix$setinvMatrix(invvMatrix)
            invvMatrix
}

