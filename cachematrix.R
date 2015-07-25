## makeCacheMatrix function creates a special matrix object that can cache its inverse
## 4 member functions, set, get, setinversion and getinversion

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        minversion <- NULL
        set <- function(y) {
                x<<- y
                minversion<<- NULL
        }
        get<- function() x
        setinversion<- function(inversion)  minversion <<- inversion
        getinversion<- function() minversion
        list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minversion <- x$getinversion()
        if(!is.null(minversion)) {
                message("getting cached data")
                return(minversion)
        }
        data<- x$get()
        minversion<- solve(data, ...)
        x$setinversion(minversion)
        minversion
}
