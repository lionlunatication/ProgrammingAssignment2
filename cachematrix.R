## cachematrix.R
## v1: November 23, 2014 - Ryan Vincent

## Calculates the inverse of an invertible square matrix, and stores 
## the inverse in cache. When the inverse is called on the same matrix, 
## the inverse from the cache is pulled to avoid redundant calculations. 

## This is especially useful for large invertible matrices, or in programs 
## where the inverse of a single matrix is required in a large number of 
## different environments.
## _____________________________________________________________________________


## This function stores the initialized matrix and its calculated inverse 
## in seperate slots in memory. The function creates a list of the get and 
## set functions for the matrix and its calculated inverse. The stored 
## inverse can be retrieved by calling getinverse() anytime. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(mtrx) {
        x <<- mtrx
        inv <<- NULL
    }
    
    get <- function() x
    
    setinv <- function(inverse) {
        inv <<- inverse
    }
    
    getinv <- function() inv
    
    list(set = set, get = get,
         setinverse = setinv,
         getinverse = getinv)
}


## This function computes the inverse of the stored matrix above, and sets 
## the value in cache. If the inverse was already computed, the inverse 
## stored in cache is retrieved and skips the computation.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("Retrieving from cache")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    
    ## Returns value only if matrix is invertible 
    ## (not required in question, but added for the sake of completeness)
    
    if(inv == NA) {
        message("Matrix is not square-invertible!")
    } else {
        x$setinverse(inv)
        return(inv)
    }
}
