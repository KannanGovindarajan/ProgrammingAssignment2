## Below functions returns inverse of a matrix
## function implements caching rather than computing it repeatedly, 
## there by saves compute cost for repeated calls with same input matrix

## Sample code to test:
## source("cachematrix.R")
## c=rbind(c(1, -1/4), c(-1/4, 1))
## mat <- makeCacheMatrix(c)
## cacheSolve(mat)
## c %*% cacheSolve(mat) ## See if you get Identity matrix

## This function creates a special "matrix" object that can cache   
## inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        
        ## intialize inputs
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## gets input matrix
        get <- function() x
        
        ## sets inverse matriSx to cache
        setinverse <- function(inverse) m <<- inverse
        
        ## gets cached inverse matrix
        getinverse <- function() m
        
        ## return the list of functions which can be performed
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of the special "matrix" 
## (returned by above makeCacheMatrix() function), computes only 
## if inverse of a matrix doesn't exist in cache

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
       
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        
        ## if cache doesn't exist, get input matrix
        data <- x$get()
        
        ## compute inverse of an matrix 
        m <- solve(data)
        
        ## set inverse of an matrix to cache
        x$setinverse(m)
        
        ##return inverse of a matrix
        m
}
