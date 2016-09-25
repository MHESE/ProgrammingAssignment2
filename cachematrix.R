## makeCacheMatrix fonction is used to create a special "matrix" that has the possibility to store too
## its inverse matrix.
## cachesolve fonction is used to return the inverse matrix.
## If it was calculate, the calculated inverse is returned without need to be recaclulate.
## Otherwise, it's calculate.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(dataMatrix = matrix()) {

    invDataMatrix <- NULL
    
    setMatrix <- function (newMatrix) { # For setting new data matrix 
        dataMatrix <<- newMatrix
        invDataMatrix <<- NULL
    }
    
    getMatrix <- function () dataMatrix  # For getting the data matrix
    
    setInvMatrix <- function (invDM) invDataMatrix <<- invDM  # For setting new inv matrix
    
    getInvMatrix <- function() invDataMatrix  # For getting the inverse data matrix

    # Return the special "cacheMatrix"     
    list (setMatrix = setMatrix,
          getMatrix = getMatrix,
          setInvMatrix = setInvMatrix,
          getInvMatrix = getInvMatrix
         )
}

## This function computes the inverse of the special "cacheMatrix" returned by makeCacheMatrix.

cacheSolve <- function(cacheMatrix, ...) {

    # If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
    # should retrieve the inverse from the cache.
    invDataMatrix <- cacheMatrix$getInvMatrix ()
    if(!is.null(invDataMatrix)) {
        message("getting cached data")
        return (invDataMatrix)
    }
    
    # So, the inverse has not already calculated. Let's calculate.
    dataMatrix <- cacheMatrix$getMatrix ()
    invDataMatrix <- solve (dataMatrix, ...)
    
    # Store and return the new inverse datav matrix.
    cacheMatrix$setInvMatrix (invDataMatrix)
    invDataMatrix
}
