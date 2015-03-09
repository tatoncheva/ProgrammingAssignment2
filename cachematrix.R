## The functions makeCacheMatrix and cacheSolve are creating a special 
## object which will be used in order to cache values for a given matrix
## and its inverse matrix.


## makeCacheMatrix creates a list object with functions
## which serve to operate with the matrix "cache" where
## the matrix and its inverse are stored

makeCacheMatrix <- function(x = matrix()) {
# init a local default value for inverse
    inverse <- NULL

    set <- function(y){
        x <<- y
        inverse <<- NULL
    }

    get <- function() x

    setInverse <- function(newInverse) {
        inverse <<- newInverse
    }

    getInverse <- function() inverse

# the result is a list with the above functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## cacheSolve calculates the inverse matrix, only if 
## there is no stored value for the inverse in the cache

cacheSolve <- function(x, ...) {
# try to get the "cached" inverse
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv) ##exit if the inverse is found
    }

## calculate the inverse
    data <- x$get()
    inverse <- solve(data, ...)

## set and return the inverse matrix
    x$setInverse(inverse)
    inverse
    
}
