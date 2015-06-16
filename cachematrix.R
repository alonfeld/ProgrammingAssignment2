## cacheSolve is wrapper for solve meant to save calculations by only solving once for a given matrix. 
## The rest of the calls will use a cache value from the first call.
## makeCacheMatrix sets the environment for the caching


makeCacheMatrix <- function(x = matrix()) {
    ## Set the environment for the caching. Create 4 functions that will be used by cacheSolve:
    ## set - Set a new matrix for the cach mechainsm.  
    ##       Upon set the cache is deleted, since it is no longer relvant for the stored matrix.
    ## get - Get the stored matrix
    ## setinverse, getinverse - set and get the cached inverse of the matrix
    
    cached_inverse <- NULL
    set <- function(new_matrix) {
        x <<- new_matrix
        cached_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(calculated_inverse) cached_inverse <<- calculated_inverse
    getinverse <- function() cached_inverse
    list(set = set, get = get, setinverse = setinverse, getinverse - getinverse)
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x', 'x' is a wrapper for a matrix created in makeCacheMatrix.
    ## Use the functions created in makeCacheMatrix for the given matrix to create and use a cache of the inverse 
    ## instead of calculating it repeatedly
    
    matrix_inverse <- x$getinverse()
    if(!is.null(matrix_inverse)) {
        message("getting cached data")
        return(matrix_inverse)
    }
    stored_matrix <- x$get()
    matrix_inverse <- solve(stored_matrix, ...)
    x$setinverse(matrix_inverse)
    matrix_inverse
}
