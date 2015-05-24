############################
## ProgrammingAssignment2 ##
############################


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## As asked in this assignment I have created two functions makeCacheMatrix 
## and cacheSolve function
##
## I considered that all matrix used are invertible as described in this 
## assignment instructions
##
## makeCacheMatrix is a function that stores information about a matrix and 
## is inverse matrix.
## cacheSolve is a function that calculates the inverse matrix of the stored 
## matrix, if it was not calculated previously.



##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


## makeCacheMatrix is a function that stores information about an matrix.
## This function:
## --Set the matrix
## --Get the matrix
## --Set the inverse of the matrix
## --Get the inverse of the matrix
## function arguments: x (input matrix)




makeCacheMatrix <- function(x = matrix()) {

      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL          
      }

      get <- function() x

      setinv_m <- function(inv_m) m <<- inv_m
	  
      getinv_m <- function()m

      list(set = set, get = get,setinv_m = setinv_m,getinv_m = getinv_m)
}



##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


## cacheSolve is a function that calculates the inverse matrix of the stored
## matrix, if it was not calculated previously.
## Function arguments: x (output of makeCacheMatrix function); … (read solve() function 
## manual for more information about this arguments) 




cacheSolve <- function(x,...) {
        
	m <- x$getinv_m()
	
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinv_m(m)
	x$get()
	
        return(m)

}

