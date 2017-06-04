## Put comments here that give an overall description of what your
## functions do

## creates and sets the values of the matrix and he inverse matrix to an object

makeCacheMatrix <- function(x = matrix()) {
        
      inv <- NULL
        set <- function(y)
        {
            x <<- y
            inv <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Checks the inversed cache, and then gets the result and skips computation or sets the value int the cache
## depending on the first check, uses the setInverse to assign the value of the matrix.

cacheSolve <- function(x, ...) 
{
## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
        if (!is.null(inv)) 
        {
             message("Getting cached data")
             return(inv)
        }
        
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

