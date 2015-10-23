## DATA SCIENCE Coursera/Programming Assignment 2
## Caching the Inverse of a Matrix
#################################################################
#
# The function makeCacheMatrix creates a special "matrix"; its 
# INPUT : invertible matrix
# OUTPUT: a list containing functions to
#
#    set the value of the matrix 
#    get the value of the matrix
#    set the value of the inverse 
#    get the value of the inverse
#
makeCacheMatrix <- function(x = matrix()){
	inv <- NULL # initiate the inverse of the matrix
	# ----------------------------------------------------
	# "set" - function to set the value of matrix
        set <- function(newMatrix)
       	{
                x <<- newMatrix
                inv <<- NULL 
        }
	# ----------------------------------------------------
	# "get" - function to get the value of the matrix 
        get <- function()
	       	x
	# ----------------------------------------------------
	# "setInv" - function to set the inverse matrix
        setInv <- function(solve) 
		inv <<- solve
	# ----------------------------------------------------
	# "getInv" - function to get the inverse matrix
        getInv <- function()
		inv
	# ----------------------------------------------------
	# creation of the list of functions 
        list(
	     set = set, 
	     get = get,
             setInv = setInv,
             getInv = getInv
	     )
}


################################################################
#
# This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix. 
# INPUT: object "matrix" obtained with makeCacheMatrix
# OUTPUT: inverse of that matrix
# (If the inverse has already been calculated and the matrix has not changed, 
# then the cacheSolve retrieves the inverse from the cache.)
#
cacheSolve <- function(x, ...) {
        storedInv <- x$getInv()
        if(!is.null(storedInv)) 
	{ 
                message("getting cached data")
                return(storedInv)
        }
	# if the inverse is NULL proceeds as follows
        storedMat <- x$get() 
        matInv <- solve(storedMat, ...) # calling solve function to invert matrix 
        x$setInv(matInv) # caching computed inverse matrix 
        matInv           # return a matrix that is the inverse of 'x'
       	 
}
