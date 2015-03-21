## This program uses cache to skip repeated computations to get Inverse of Matrix




## makeCacheMatrix: provide interfaces (list of functions) to cache matrix data and its inverse
##  - input
##            (1) x : a matrix
##  - output
##            a list of functions:
##            (1) set : sets x into cache
##            (2) get : returns x from cache
##            (3) setinverse : sets inverse of x into cache
##            (4) getinverse : returns inverse of x from cache
makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## cacheSolve: gets a makeCacheMatrix (of a matrix) as input, and provides inverse of the matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

