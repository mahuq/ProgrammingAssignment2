# makeCacheMatrix
# This function creates a special "matrix" 
# object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        xInverse <- NULL
        setF <- function(y) {
                x <<- Y
                xInverse <- NULL
        }

getF <- function() x
setInverse <- function(inverse) xInverse <<- inverse
getInverse <- function() xInverse

list(setF = setF, getF = getF, setInverse = setInverse,
                getInverse = getInverse)

}


# cacheSolve
# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
cacheSolve <- function(x, ...) {        
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {
                message("I am in cache, OK")
                return(invMatrix)
        }
        
# If the inverse has already been
# calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.
                
        data <- x$getF()
        invMatrix <- solve(data, ...)
        x$setInverse(invMatrix)
        invMatrix
        
}
# end of both functions