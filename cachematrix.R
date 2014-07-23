## This function looks for a matrix in the cache and
## if not found, it sets the matrix in the cache for future uses.
# If the matrix is found in the cache, it retrieves it.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
		
        get <- function() x
		
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
		 

}


## This function returns the inverse of a matrix.
## But first it searches in the cache in case it's already been calculated, 
## Otherwise the inverse is calculated and put in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		        m <- x$getinverse()
        if(!is.null(m)) {
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
