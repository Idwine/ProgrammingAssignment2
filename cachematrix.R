## To be able to retrieve the inverse of a matrix, without
## the need to recalculte it every time, a cashable matrix
## is created in the first funtion.
## The second function checks if the inverse is allready
## calculated and if not, calculates it.

## Create a matrix with cashable inverse

makeCacheMatrix <- function(x = matrix()) {
## Create a variable to store the inverse:
    inv <- NULL
## Read in the matrix and store its values 'outside' the current environment:
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
## Give the matrix to 'anyone' who asks for it:
    get <- function() x
## Read in the inverse matrix, calculated by cacheSolve:
    setinverse <- function(inverse) inv <<- inverse
## Give the cached inverse matrix:
    getinverse <- function() inv
## Name the different attributes of the CacheMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)   
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
## Request the value of the cached inverse matrix:
    inv <- x$getinverse()
## Check if there exists a cached inverse matrix:
    if(!is.null(inv)) {
## If it exists, return the inversed matrix:
        message("getting cached data")
        return(inv)
    }
## If it doesn't exist, calculate the inverse matrix by first
## requesting the value of the matrix:
    data <- x$get()
## and then calculating it's inverse:
    inv <- solve(data, ...)
## Cache the inverse in the CacheMatrix:
    x$setinverse(inv)
## and return the value of the inversed matrix:
    inv
}
