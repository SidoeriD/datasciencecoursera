## The following functions compute and cache the inverse of a invertible square matrix. 
## Caching the inverse prevents having to repeat the costly inversion computation more than once. 
## After the first computation the inverse is retrieved from the cache instead of having to compute it again

## The following function, makeCacheMatrix, creates a creates "matrix" object which is really a list containing a function to:
## 1. set the value of the matrix, 2. get the value of the matrix, 3. set the value of the inverse, 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function, cacheSolve, calculates the inverse of the "matrix" created with makeCacheMatrix. 
## If the inverse has already been calculated, it gets the inverse from the cache and skips the computation.
## Assumed is that the supplied matrix is invertible 

cacheSolve <- function(x, ...) {
       
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv  ## Return a matrix that is the inverse of 'x'
}
