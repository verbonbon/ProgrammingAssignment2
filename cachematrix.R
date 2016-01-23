
## This function creates a special "matrix" object that can cache its inverse.

###first predefine x as a matrix

makeCacheMatrix <- function(x = matrix()) {
        inve <- NULL             
        ## set the stored inverse value as null
        
        set <- function(y) {      
                x <<- y        
                ## set the value of the matrix (substitute the matrix x with y inve the main function:makeCacheMatrix
                
                inve <<- NULL
                ## restores the value of the stored inverse value "inve" to null, because the old matrix is not needed anymore
        }
        get <- function() x 
        ## returns the value of matrix x stored in the main function. 
        
        setinverse <- function(inverse) inve <<- inverse 
        ## store/set the value of the matrix "inve" into the main function makeCacheMatrix 
        
        getinverse <- function() inve 
        ## return/get the value of the matrix "inve"
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 
        # create a list to store the 4 functions defined in the function makeCacheMatrix
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the 
##inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache

cacheSolve <- function(x, ...) {
        inve <- x$getinverse() 
        ## calling the function "getinverse" created above
        
        if(!is.null(inve)) {
                message("getting cached data")
                return(inve) 
                ## check whether the matrix "inve" exist (not null). 
                ##If it exists in memory, it returns a message "getting cached data" 
                ##and return the value "inve" 
        }
        data <- x$get()
        inve <- solve(data, ...)
        x$setinverse(inve)
        inve
        ## if the matrix "in" does not exist (is null), it get the original matrix (x), 
        ## compute the inverse of the matrix, 
        ## cache inverse of the matrix,
        ## and return the value of the invsered matrix 
}
