## The inverse of an invertible square matrix can be found by
## combination of functions makeCacheMatrix and cacheSolve.

## Input of makeCacheMatrix is of the from:
## x <- matrix(c(n1,n2,...,n(kxk)), nrow = k, ncol = k)
## y <- makeCacheMatrix(x)
## Output is a list

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y){
                x <<- y
                invm <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inversematrix) invm <<- inversematrix # in cacheSolve this function is called (with input invm there) so in makeCacheMatrix invm is stored as invm (from cacheSolve) and by using <<- the function getmean() will return this invm.
        getinverse <- function() invm  
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Input of cacheSolve is the output of makeCacheMatrix
## If y <- makeCacheMatrix(x) then cacheSolve(y) can be found
## Output is the inverse matrix of x (calculated or loaded from cache)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        invm <- x$getinverse()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        base_matrix <- x$get()
        invm <- solve(base_matrix)
        x$setinverse(invm)
        invm
}
