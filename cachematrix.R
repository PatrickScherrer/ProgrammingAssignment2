## makeCacheMatrix creates a matrix object that can cache its inverse in a higher environment
## cacheSolve checks to see if the matrix exists, and returns the value of it from a higher environment
## if it does

## Creates matrix and can set matrix or return matric from higher environmnet

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## Retrieves value of matrix from higher enviroment, if its poluated (not null), returns that cached value
## otherwise, it inverts the matrix and assigns it to the cache in the higher environment

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)){
            message("its in the cache, retrieving")
            return(m)
        }
        matrix<-x$get()
        m<-solve(matrix, ...)
        x$setmatrix(m)
        m
}
