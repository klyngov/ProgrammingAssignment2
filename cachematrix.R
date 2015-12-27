##
## makeCacheMatrix (x)
## cacheSolve (x, ...)
##
## These functions work in together to calculate the inverse of matrix (x). Call cacheSolve (x) first, and it 
## will return the inverse of matrix 'x'.  makeCacheMatrix(x) is used by cacheSolve to store the value of the  
## inverse matrix that has already been calculated. 
##


##
## makeCacheMatrix (x) -> Creates a list of functions to be used with the matrix 'x'
## 
## set (x) ==> set the value of the matrix to 'x'
## get ()  ==> return the current value of the matrix
## setInverse (x)  ==> set the inverse of the matrix to 'x'
## getInverse () ==> return the inverse of the matrix
##
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseMatrix) i <<- inverseMatrix
    getInverse <- function() i
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


##
## cacheSolve(x, ...) ==> calculates and returns the inverse of matrix 'x'
## If the value has already been calculated (cached), returns the cached value
##
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i     
}
