## The makeCacheMatrix function below creates a list of 
## functions to set & get values of a matrix  
## and to set and get the inverse of the matrix.
##
## set - sets the value of the matrix in environment y
## get - retrieves value of the matrix 
## setinverse - sets the value of the computed inverse in the "inverse" envirtonment
## getinverse - gets the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function retrieves the value of the inverse of the matrix
## from the cache if avialable in the cache. Otherwise, it calculates 
## the value using the solve function and stores it in the cache.
##
## The inverse of the matrix is retrieved from the cache using 
## the getinverse function and if found this functions returns it.
##
## If the value for the inverse of the matrix is not found in the cache,
## this functions calculates the inverse using the solve function 
## and sets it in the cache using the setinverse function and returns it.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("Found it in cache. Here it is.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data ,...)
        x$setinverse(inv)       #cache the inverse 
        inv
}
