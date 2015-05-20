## This function stores four functions:
## 1 - set - used to reset the matrix, if need be
## 2 - get - returns the input from the main function 'makeCacheMatrix'
## 3 - setinv - stores the input variable inv into the main function 'makeCacheMatrix'
## 4 - getinv - returns the variable i from the main function 'makeCacheMatrix'

makeCacheMatrix <- function(x = matrix) {
        ## creating/initialising variable i
        i <- NULL
        
        ## creating first function 'set', used to reset the matrix, if need be
        set <- function(y = matrix()) {
                x <<- y
                i <<- NULL
                return (x)
        }
        ## creating second function 'get', which just returns the variable x
        ## from the main function 'makeCacheMatrix'
        get <- function() x
        
        ## creating third function 'setinv', stores the input variable inv into
        ## the main function 'makeCacheMatrix'
        setinv <- function(inv) i <<- inv
        
        ## create fourth function 'getinv', which returns the variable i
        getinv <- function() i
        
        ## store the above four functions in the main function 'makeCacheMatrix'
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function returns the inverse of the matrix x.  If the inverse has already
## been calculated for a given matrix, function just retrieves from cache.  
## Otherwise, it calculates and returns the inverse.

cacheSolve <- function(x,...) {
        
        i <- x$getinv()
        
        ## check to see if i already exists.  If so, retrieve from cache.
        
        if(!is.null(i)) {       
                message("Retrieving cached data:")
                return(i)
        }       
        
        ## if the inveserse doesn't exist, then calculate it and return it
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}