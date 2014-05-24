## Description
##
## This program demonstrates how to assign data to the global environment 
## and how to pass functions into functions
##
##Usage
##
## Return the inverse of a square matrix. 
## How to run the demo is, first, create a square matrix 
## ( equal number of rows and cols). Next, execute makeCacheMatrix
## function and pass in the matrix variable. Be sure to assign the 
## function to a variable. Finally, call the cacheSolve
## function, passing the makeCacheMartix variable. The first execution of 
## cacheSolve 
## will set the cache and display the contents. Subsequent calls will return 
## the data already existing in the cache. The set function of the 
## makeCacheMatrix variable can be used to change the content of the cache.



## The makeCacheMartix() function set a global variable 'x' that is used 
## to hold the matrix information. It primes the cache with the matrix 
## passed in. If one is not provided, an empty matrix object is created.

makeCacheMatrix <- function(x = matrix()) {
        
	        inv <- NULL

		set <- function(y) {
			x <<- y
		        inv <<- NULL
		}
								        
		get <- function() x
											        
		setinverse <- function(solved) inv <<- solved
										        
		getinverse <- function() inv
														        
		## The list allows the "methods" set,get,setinverse and getinverse 
		## to be public and accessed 
		## from the command line
																        
		list(set = set, get = get,
		     setinverse = setinverse,
		     getinverse = getinverse)

}


## cacheSolve looks at the content of the queue of the function passed in. If the queue's 
## empty, cacheSolve performs the inverse and sets the queue with the result. 
## If the queue already has data, cacheSolve returns the existing data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()

        if(!is.null(inv)) {
		message("getting cached data")
	        return(inv)
	}

        data <- x$get()
	inv <- solve(data, ...)

	x$setinverse(inv)

	inv
}
