## makeCacheMatrix creates a special "Matrix", which is really a list of functions contain 
## 1. "set" the matrix, "get" the matrix, "setinverse" of the matrix and "getinverse" of the matrix


makeCacheMatrix <- function(x = matrix()) {
        IM <- NULL
        set <- function (y) {
                x <<- y
                IM <<- NULL
        }
        get <- function() x
        setinverse <- function(i) IM <<- i
        getinverse <- function() IM
        list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve function inverts the matrix that has passed in the makeCacheMatrix. Then stores the value
## as IM by calling setinverse function. cacheSolve function uses this value for the inversion of same matrix,
## instead computing again and produced IM along with a "getting cached IM".

cacheSolve <- function(x, ...) {
        
        IM <- x$getinverse ()
        if (!is.null(IM)) {
                print ("getting cached IM")
                return(IM)
        }
        else {
                data <- x$get ()
                IM <- solve(data, ...)
                x$setinverse(IM) 
        }
        IM
}


