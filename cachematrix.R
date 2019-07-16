## There are 2 functions here.
## makeCacheMatrix -> his function creates a special "matrix" object that can cache its inverse.
##      This is nothing but list of 4 functions
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##      If the inverse has already been calculated (and the matrix has not changed)
##      , then the cachesolve should retrieve the inverse from the cache.


## This function takes a matrix as input. Returns a list of 4 functions.
## These 4 functions uses the input matrix to set,get or set to different environment

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function takes an input list. 
## It tries to find the inverse in the cache. If found returns from cache.
## If not found, then it caclcuates the inverse and stores the results in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        print(data)
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
