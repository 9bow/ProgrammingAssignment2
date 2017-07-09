
# this makeCacheMatrix() fuctnion generate & cache inverse matrix
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse
# 4. get the inverse
# this list is used as the input of the function cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}


# this function, cacheSolve returns inverse matrix of the given matrix 'x'.
# first, checks if the inverse is already calculated, if not, calculate and
# cache using the setInv() in the makeCacheMatrix()'s List.
cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv = solve(data, ...)
    
    x$setInv(inv)

    ## Return a matrix that is the inverse of 'x'    
    inv
}