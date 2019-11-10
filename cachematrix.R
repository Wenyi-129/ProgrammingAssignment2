## these two functions can cache the inverse of a matrix.

## creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(matrix) m <<-t(matrix)
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- t(data)
    x$setinv(m)
    m
}
