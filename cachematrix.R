################################################################################
## File 'cachematrix.R'. Assignment 2 of the 'R Programming Course'           ##
##                       (Week 3: 'Loop Functions and Debugging')             ##
################################################################################
## The pair of functions stored in this file allows cache the inverse of a 
## matrix. In particluar, these functions are developed to save time in
## computationally costly matrix inversions, where caching the inverse of a 
## could be faster than computing it repeatedly


## This function creates a special "matrix" object that can cache its inverse.
## this special matrix in fact is really a list containing 4 functions:
## 1) set    : to set the value of the matrix
## 2) get    : to get the value of the matrix
## 3) setinv : to set the value of the inverse of the matrix
## 4) getinv : to get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
} # 'makeCacheMatrix' END



## This function computes the inverse of the special "matrix" returned by the 
## 'makeCacheMatrix' function above. If the inverse has already been calculated 
## (and the matrix has not changed), then 'cacheSolve' should retrieve the 
## inverse from the cache

## NOTE: this function assumes that the supplied matrix is always invertible.
cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
} # 'cacheSolve' END


# Examples (proof-of-concept, with a 3x3 matrix):

# 0) Creating the special matrix
#    A <- makeCacheMatrix()

# 1) Setting the values of the matrix
#    A$set( matrix(rnorm(9), 3, 3) )

# 2) Getting the values of the matrix
#    A$get()

# 3) Setting the value of its inverse
#    A$setinv( solve(A$get()) )

# 4) Getting the values of its inverse
#    A$getinv()

# 5) Testing the 'cacheSolve' function with a matrix with an already 
#    computed inverse (notice the 'getting cached data' message):
#    cacheSolve(A)

# 6) Testing the 'cacheSolve' function with a matrix withOUT an already 
#    computed inverse:
#    A1 <- makeCacheMatrix()
#    A1$set( matrix(rnorm(9), 3, 3) )
#    A1 <- makeCacheMatrix() # WITHOUT 'getting cached data' message
#    A1 <- makeCacheMatrix() # WITH 'getting cached data' message
