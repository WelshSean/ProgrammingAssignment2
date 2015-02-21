## We create two functions - the first one creates a list of functions (four in total) 
## that can get and store the Matrix and its inverse. We use the <= operator
## to store the values in a parent environment so that it persists across invocations of the 
## set and get functions. 


## This function takes a matrix as its argument and provides a means of "setting" and "getting"
## the matrix and its inverse persistently as described in the main comment. The methods for 
## getting and setting it are returned in a list.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
