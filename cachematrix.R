##  Coursera Programming assignment 2
##  Goal: Create a cache system that utilizes R's lexical scoping properties to reduce the time of inversing the values of a matrix. 


## This function takes a matrix as an input, creates a special type of matrix object, and caches the inverse. 

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set = function(y)    {
        x <<- y
        inv <<- NULL
    }
    
    get = function() x 
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## *cacheSolve* returns the inverse of the special matrix made in makeCacheMatrix. If an inverse matrix has been cached, 
## the function will return a message that it is returning the cached result. 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if (!is.null(inv)) {
        message("Returning the cached result")
            return(inv)
    }
    
    matrixvalues <- x$get()
        inv <- solve(matrixvalues, ...) 
        x$setinverse(inv)
        
        inv
        
}