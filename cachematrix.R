## The following code is used to compute matrix inversion and also caching the inverse of a matrix instead of computing multiple tiimes.
## It contains two functions: makeCacheMatrix and cacheSolve. Each of them are explained in more detail below. 


#############################################################################################################
###############################   makeCacheMatrix Functionn  ################################################
#############################################################################################################
#
# This function creates a list that contains four internal functions: set,get,setinv, and getinv.
# It also uses <<- assignment operator so these internal variables don't get into parent environment.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {

                # use `<<-` to assign a value to an object in a different environment from the current one.
                x <<- y
                inv <<- NULL  # It initialises inv to null.
		    }
                get <- function() x # This returns the input matrix.
      	    setinv <- function(inv) inv <<- inv # This sets the inversed matrix.
      	    getinv <- function() inv # This returns the inversed matrix.
	          list(set=set, get=get, setinv=setinv, getinv=getinv)
}


#############################################################################################################
####################################   cacheSolve Function   ################################################
#############################################################################################################
#
# cacheSolve function computes the inverse of the “matrix” from the function makeCacheMatrix(). 
# If the matrix hasn't change and the inverse has already been calculated, it will just retrieve the inverse directly from the cache.

  cacheSolve <- function(x, ...) {
      inv <- x$getinv() # Here you get the inversed matrix from object x
      # it will be null if uncalculated.
      if(!is.null(inv)) { # if there is an inversion result
	  message("Retrieving the cached data")
	  return(inv) # return the inversion that is calculated
      }
      data <- x$get() # Otherwise, do x$get to get the matrix object
      inv <- solve(data) # Solve it
      x$setinv(inv) # Then set it to the object
      inv # return the solved result
  }
