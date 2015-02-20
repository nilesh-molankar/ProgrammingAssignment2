## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        ## the matrix sent to this function is stored in x
        
        ## inv is the variable that will store the inverse of the matrix
        inv <- NULL
        
        ## if the user needs to change the value of the matrix
        ## set function can be used to change it.
        ## example 
        ## new_matrix <- matrix( c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow=3, ncol=3)
        ## x$set(my_matrix) ## here x was initially used to store result of makeCacheMatrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## this function returns the current value of x
        get <- function() x
        
        
        ## set the inverse of the matrix x.  Called by cacheSolve
        ## <<- is used to ensure that the inv defined in the parent 
        ## environment is modified and not the local one
        setInverse <- function(solv) inv <<- solv
        
        ## returns the inverse of matrix as stored in inv. 
        ## Will be null if setInverse has not been called or
        ## if set is called after the last call to setInverse
        getInverse <- function() inv
        
        ## return value of the makeCacheMatrix function is a list
        ## of functions and variables that we want to make available to all
        ## They are accessed with the $ operator. 
        ## return the list
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
} ## end of makeCacheMatrix function


## Write a short comment describing this function
## This function will check if inverse of the matrix exists if yes will return that else
## it will calculate the inverse using the solve function.. 
## start of cacheSolve function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## get the current inverse value if any using the $ operator
        inv <- x$getInverse()
        
        ## check if inv is null or not
        ## if not null return the value, this comes from cache 
        ## and not recalculation required
        if(!is.null(inv)) {
                message("getting cached Inverse")
                ## return or end of execution will be called 
                return(inv)
        }
        
        ## call get() to get the underlying matrix
        data <- x$get()
        
        ## calculate the inverse of the underlying matrix
        inv <- solve(data, ...)
        
        ## store the inverse in x so it can be reused if matrix is not changed.
        x$setInverse(inv)
        
        ## return the inverse value.
        inv
}   ## end of cacheSolve function 
