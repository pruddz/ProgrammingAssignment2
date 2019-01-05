## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    # Initialise inverse matrix
    inverse_matrix <- NULL 


    # Create functions to set and get the matrix
    set <- function(y) {
        x <<- y
        inverse_matrix <<- NULL
    }
    get <- function() x               

    # Create functions to set and get the inverse matrix
    setinverse <- function(inverse) inverse_matrix <<- inverse
    getinverse <- function() inverse_matrix               


    # Create a list of functions to return
    list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


## Function returns inverse of matrix either by calculation or cache retrieval

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    inverse <- x$getinverse()

    # no need to calculate inverse if its already cached
    if(!is.null(inverse)) {
        message("using cached inverse")
        return(inverse)
    }


    # Use the solve function to calculate the inverse of the matrix
    # and then cache it
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)

    return(inverse)
}
