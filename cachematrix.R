
## Overall Description
## The purpose of this task is to cache potentially time-consuming computations.
## Below are two functions that are used to create a special object that stores a numeric
##   matrix and caches its inverse.

## Since this assignment assume that the matrix supplied is always invertible, we need 
##   not consider the situation that the inverse of the matrix cannnot be obtained.


## Write a short comment describing this function
## The following function is to creates a special "matrix", which is really a list 
##   containing a function to 
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the matrix's inverse
##   4. get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    iv <- NULL
    set <- function(y) {
        x <<- y
        iv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) iv <<- solve
    getinverse <- function() iv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


## Write a short comment describing this function
## The following function calculates the inverse of the special "matrix" created with
##   the above function.

cacheSolve <- function(x, ...) {
    ## It first checks to see if the inverse has already been calculated. 
    ## If so, it gets the inverse from the cache and skips the computation.
    ## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
    ##   in the cache via the #setinverse# function.
    
    iv <- x$getinverse()
    if(!is.null(iv)) {
        message("getting cached data")
        return(iv)
    }
    data <- x$get()
    iv <- solve(data, ...)
    x$setinverse(iv)
    iv
    ## Return a matrix that is the inverse of 'x'
}
