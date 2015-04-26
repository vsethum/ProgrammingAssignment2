## This R program provides 2 functions - 
## A. a function called makeCacheMatrix that creates a matrix object that can cache its inverse
## B. another function called cacheSolve that computes the inverse of the matrix 

## The following function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    cachedInvMatrix <- NULL
    
    setMatrix <- function(newMatrix) {
        x <<- newMatrix
        cachedInvMatrix <<- NULL
    }
    
    getMatrix <- function() x
    setMatrixInverse <- function(matInv) cachedInvMatrix <<- matInv
    getMatrixInverse <- function() cachedInvMatrix
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)

}


## The following  function computes the inverse of the special matrix returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), then the 
##cachesolvecacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
 
    matInv <- x$getMatrixInverse()
    
    if(!is.null(matInv)){ 
        message("getting data from cache") 
        return(matInv) 
    }
    
    originalMatrix <- x$getMatrix() 
    matInv <- solve(originalMatrix, ...) ##solve calculates the inverse of the matrix
    x$setMatrixInverse(matInv) 
}