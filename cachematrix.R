## Below are a pair of functions that cache and compute the 
## inverse of a matrix.

## This function creates a special "matrix" 
## object that can cache its inverse.
## assume that the matrix supplied is always invertible.

##    > matrix_one <- matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3)
##    > mcm <- makeCacheMatrix(matrix_one)
##    > cacheSolve(mcm) # outputs inverse 
##    > cacheSolve(mcm) # outputs inverse from cache

makeCacheMatrix <- function(mtx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinverse <- function(inv) {
        inverse <<- inv;
    }
    getinverse <- function() return(inverse);
    
    return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above.
## it first checks to see if the mean has already been calculated. 
## If so, it gets the mean from the cache and skips the computation.

##  USAGE: cacheSolve(mtx, ...) - where 'mtx' is a
##         'makeCacheMatrix' object

cacheSolve <- function(mtx, ...) {
    inverse <- mtx$getinverse()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- mtx$get()
    inverse <- solve(data, ...)
    mtx$setinverse(inverse)
    return(inverse)
}