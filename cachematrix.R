## This R code-file creates two functions: makeCacheMatrix and cacheSolve.
## See comments in the body of each function for a detailed description of each.

makeCacheMatrix <- function(x = matrix()) {
        ## this is function to return a list containing functions to:
        ## 1. set the matrix
        ## 2. get the matrix
        ## 3. set the inverse
        ## 4. get the inverse
        ##  this list is used as the argument for cacheSolve()
        ## input 'x' to this function is a square invertible matrix (NB. there is
        ## no checking for either condition)
        
        inverse_matrix = NULL
        set = function(y) {
                
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                ## here <<- (super-assignment) is used to allow variables to store state 
                x <<- y
                inverse_matrix <<- NULL
        }
        get = function() x
        setinv = function(inverse) inverse_matrix <<- inverse 
        getinv = function() inverse_matrix
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        ## this is a function to return the inverse of the original matrix input to makeCacheMatrix()
        ## input 'x' to this function is the output of makeCacheMatrix()
        
        inverse_matrix = x$getinv()
        
        ## if the inverse has already been calculated
        if (!is.null(inverse_matrix)){
                ## get it from the cache and skips the computation. 
                message("getting cached data")
                return(inverse_matrix)
        }
        
        ## else calculate the inverse matrix
        matrix_data = x$get()
        inverse_matrix = solve(matrix_data, ...)
        
        # sets value of the inverse in the cache using the setinv function.
        x$setinv(inverse_matrix)
        return(inverse_matrix)
}