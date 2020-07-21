## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a list of vectors to functions.  These functions
## access a cache location for a matrix and it's inverse.
## 
## We store the matrix and its inverse in the parent environment of these
## function definitions using the <<- operator. This 'Parent Environment' is
## normally the Global Environment.  
## 
## This ensures we don't lose the matrix (and the computer effort) when we leave 
## a function or climb to the parent environment.  As long as the user is in an
## environment where makeCacheMatrix is available, the matrices will be
## available.

makeCacheMatrix <- function(x = matrix()) {
        
        ## initialize a memory location for the inverse
        i <- NULL
        
        ## Define the 'SetMatrix' function.  
        ## 
        ## y - matrix passed to the function
        ## x - matrix location in the parent environment
        ## i - inverse matrix location in the parent environment
        ## 
        ## This function uses the <<- operator to put the passed matrix (y)
        ## into the parent-frame memory location (x).
        ##   
        ## Since we are 'Set'ting a new matrix, we have not yet calculated an 
        ## inverse so the inverse cache location (i) is set to NULL
        ## 
        setMatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## Define the 'getMatrix' funciton.  It simply returns the matrix (x).
        ## 
        getMatrix <- function() x
        
        ## Define the 'setInverse' function.  Inv is the already-solved inverse
        ## matrix that was passed to getMatrix.  We use the <<- operator to
        ## store the result in the parent environment
        ## 
        setInverse <- function(Inv) i <<- Inv
        
        ## Define 'getInverse' function.  It simply returns 'i' which exists
        ## in the parent environment and, therefore, acts as a cache
        ## 
        getInverse <- function() i
        
        ## Create the list of function vectors.  If 'vec' is the vector returned
        ## when users called makevector, users access these functions,
        ## and therefore the cache, by calling:
        ## 
        ## vec$setMatrix
        ## vec$getMatrix
        ## vec$setInverse
        ## vec$getInverse.
        ## 
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve retrieves the cached matrix inverse.  The function preserves
## computer time by checking to see if the Inverse has already been calculated.
## This is why we set the inverse to NULL when we create a new Matrix Cache or
## set the cache to a new matrix.
## 
cacheSolve <- function(x, ...) {
        
        i <- x$getInverse()
        
        ## If (i) is not NULL, the inverse is already calculated
        if(!is.null(i)) {
                
                ## Inverse already exists because it is not NULL Return what we 
                ## already assigned and quit the function
                ## 
                message("getting cached data")
                return(i)
        }
        
        ## Inverse does not exist so we have to use the computer power to
        ## calculate it and send the result to the cache.
        ## 
        data <- x$getMatrix()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}