## Matrix inversion is usually a costly computation and may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly. 
## We wrote a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x=matrix(), ...) {
    inv<-x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    inv<-x$get()
    inv<-solve(inv, ...)
    x$setinv(inv)
    inv
}
