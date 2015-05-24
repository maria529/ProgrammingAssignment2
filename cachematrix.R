## This code contains two functions to calculate the inverse of the matrix
## it tries to use cached inverse of the matrix if it has been calculated before
cCa

## The first function 'makeCacheMatrix; returns a special list containing 4 functions:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the inversed matrix
# 4. get the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(i) m <<- i
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function return a matrix that is the inverse of 'x'
## It first tries to get the cached inverse value, 
## if this is not successful, it calculates inverse of the matrix using solve function 
## and it records "cached" inverse matrix using setinverse()
cacheSolve <- function(x, ...) {
        z <- x$getinverse()
        if(!is.null(z)) {
            message("getting cached data")
            return(z)
        }
        data <- x$get()
        z <- solve(data, ...)
        x$setinverse(z)
        z
}

