#Matrix inversion is usually a costly computation and there may be some benefit 
#to caching the inverse of a matrix rather than compute it repeatedly
## The two functions makeCacheMatrix & cacheSolve written below are used to create a special object that 
## stores a matrix and caches its inverse respectively.

#The following function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinverse <- function(inv) inverse <<- inv;
    getinverse <- function() return(inverse);
    return(list(set = set, 
                get = get, 
                setinverse = setinverse, 
                getinverse = getinverse))
}

## Sample run:
## > mtx = rbind(c(1,2), c(3, 4))
## > m = makeCacheMatrix(mtx)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00



# The following function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
# This function assumes that the matrix is always invertible and computes the inverse of a square matrix with the help of solve() function in R

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Retrieving cached data for this run...")
        return(inv)
    }
    cached_data <- x$get()
    cache_inverse <- solve(cached_data, ...)
    x$setinverse(cache_inverse)
    return (cache_inverse)
}

## Sample run:
## No cached data in the first run
## > cacheSolve(m)
##     [,1] [,2]
##[1,] -2.0  1.0
##[2,]  1.5 -0.5

## Retrieving from the cache in the second run
## > cacheSolve(m)
## Retrieving cached data for this run...
##      [,1]  [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
## > 
