## The two functions below allow the catculation of the inverse of a matrix. The
## inverse is then stored in cache, to allow for its return without 
## recalculation. If the the original matrix is changed, the cache is cleared.

## The 'makeCacheMatrix' function is a list containing sub-functions, which can
## be used to set or return a marix, store the inverse of that matrix in cache,
## and/or return the cached inverse.
makeCacheMatrix <- function(x = matrix()) {
        
        ## If this function is called without one of the 'sub-functions' (set,
        ## get, setCache, or getCache) then 'x' is set to an empty matrix, and 
        ## the cache variable 'm' is set to NULL
        m <- NULL
        
        ## Sub-function'set' sets the matrix to x, and resets the cache
        set <- function(y) {
                x <<- y
                m <<- NULL
        } 
        
        ## Sub-function 'get' simply returns the matrix 'x'
        get <- function() x
        
        ## Sub-function 'setCache' stores an object passed to it into the
        ## cache 'm'
        setCache <- function(inverse) m <<- inverse
        
        ## Sub-function 'getCache' simply returns the cache 'm' 
        getCache <- function() m
        
        ## Defines the parent function 'makeCacheMatrix'
        list (set = set, get = get, setCache = setCache, getCache = getCache)
}


## The 'CacheSolve' function returns the inverse of the matrix set in the
## 'makeCacheMatrix' above. Where the inverse is already cached, the cache is
## returned. Otherwise the inverse is calculated, cached and returned

cacheSolve <- function(x, ...) {
        ## Checks to see if the inverse of the matrix is already cached. If so, 
        ## the cache is returned.
        m <- x$getCache()
        if(!is.null(m)) {
                message("Returning cached data.")
                return(m)
        }
        ## Calculates the inverse of the matrix from 'makeCacheMatrix' above.
        data <- x$get()
        m <- solve(data)
        x$setCache(m)
        m
}
