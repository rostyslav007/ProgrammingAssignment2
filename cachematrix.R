## Week 3 assignment

## Function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL                             ## initialize inv as NULL, will be used to store actual value later on 
     	set <- function(y) {                    ## define the set function to assign new 
        	x <<- y                             ## value of matrix in parent environment
         	inv <<- NULL                        ## reset inv to NULL if there is a new function
     	}
     	get <- function() x                     ## get function definition,  returns value of the matrix argument

     	setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv into parent environment
     	getinverse <- function() inv                     ## gets the value of inv where called
     	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
							 ## needed to refer to the functions with the $ operator
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inv <- x$getinverse()
     if(!is.null(inv)) {
         message("retrieval of cached data")
         return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
}
