## We create two functions - the first one creates a list of functions (four in total) 
## that can store and retrieve the Matrix and its inverse. We use the <= operator
## to store the values in a parent environment so that it persists across invocations of the 
## set and get functions. This performs a caching function.


## The "makeCacheMatrix"  function takes a matrix as its argument and provides a means of "setting" and "getting"
## the matrix and its inverse persistently as described in the main comment. The methods for 
## getting and setting it are returned in a list.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL               ## make i NULL in this environment (we want to get it from the parent)
    set <- function(y) {    ## Used to store the matrix - stores this in the parent env so that it is persisted
        x <<- y
        i <<- NULL          ## Set the value that the inverse is stored as to null tp indicat that we have
    }                       ## not yet evaluated the inverse.
    get <- function() x     ## will get x from the parent env as we havent declared x in get
    setinv <- function(inv) i <<- inv      ## Store the inverse of the matrix - again in the parent environment
    getinv <- function() i                 ## Get the inverse - will search to parent env as i not defined in this function
    list(set = set, get = get,          ## store functions in a list and return them.
         setinv = setinv,
         getinv = getinv)
}


## The "cacheSolve" function takes a list that is created by makeCacheMatrix and will 
## contain the functions that are used to manipulate the input matrix and 
## store/retrive the inverse when calculated.

cacheSolve <- function(x, ...) {
    i <- x$getinv()                     ## Check to see if we have stored an inverted matrix
    if(!is.null(i)) {                   ## if we havent "i" will be NULL and we'll go and do the inversion
        message("getting cached data")  ## otherwise we just return the cached matrix.
        return(i)
    }
    data <- x$get()                     ## Get the stored matrix
    i <- solve(data, ...)               ## Do the inversion
    x$setinv(i)                         ## Store the inverted matrix
    i                                   ## Display the result
}
