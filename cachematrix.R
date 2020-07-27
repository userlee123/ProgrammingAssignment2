## Below are two functions that are used to create a special object 
## that stores a matrix and caches its inverse.

## The first function is used to set and get the values of a matrix
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverM <- NULL
    set <- function(y) {
      x <<- y
      inverM <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inverM <<- inv
    getinv <- function() inverM
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## The second function takes the output of the first
## function as input, and return the inverse matrix
## either from the cache or calculation

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
