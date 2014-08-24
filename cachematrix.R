## The functions makeCacheMatrix and cacheSolve are used to cache data 
## while traversing through loops.

## The function makeCacheMatrix takes the data of a matrix 
## and defines 4 functions to set data ,get data, set Inverse 
## and get Inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) m <<- inv
        getInverse <- function() m
        
        # list thats maintained for the caching        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        data <- x$get()
        if(!is.null(m) ) {
                
                # the check has been introduced to make sure the data 
                # and the inverse are valid by multiplying them to check for 
                # identity matrix of same dim
                # m %*% data should give an identity matrix 
                # identical function is used to check 2 matricies if they are identical
                # identity matrix.diag(nrow(m)) will give us an identity matrix
                if(nrow(m)>0 & identical(( m %*% data),diag(nrow(m)))){
                        message("getting cached data")
                        return(m)
                }
        }
        
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
