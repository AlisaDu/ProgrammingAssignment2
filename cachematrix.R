## mFile contains two functions to define, calculate and retrieve 
## special matrix object and it's inverse


## function contains get-set methods for special matrix objects
## and its inversion

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x<<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## function checks the availability of cached  matrix inverse value.
## if value not available, calculates the inversion and stores it into object

cacheSolve <- function(x, ...) {
    inv <-x$getinverse()
    if(!is.null(inv)){
        message ("cached value available: ")
        return(inv)
    } 
    inv <- solve(x$get())
    x$setinverse(inv)
    inv
    
}
