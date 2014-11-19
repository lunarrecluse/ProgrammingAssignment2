## Two functions here are to calculate the inverse Matrix, and cache
## it in order for later calling

## The first function, makeCacheMatrix creates a special "matrix", 
## which is a list containing function to: 1. set the value of the
## matrix; 2. get the value of the matrix; 3. set the value of the 
## inverse; 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # Set the matrix
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        # Get the matrix
        get <- function() x
        
        # Set the inverse matrix
        setinverse <- function(inverse) m <<- inverse
        
        # Get the inverse matrix
        getinverse <- function() m
        
        # make a list containing the functions defined above for easy call
        list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}

## This function, "cacheSolve", calculate the inverse of the special
## "matrix" created by above function.

cacheSolve <- function(x, ...) {
        # check if the inverse has already be calculated, if yes, return
        # previous stored calculation
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # get the matrix and calculate the inverse matrix
        data <- x$get()
        m <- solve(data)
        
        # send the inverse matrix into our object 
        x$setinverse(m)
        
        # return the inverse matrix
        m
}
