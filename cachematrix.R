## Put comments here that give an overall description of what your
## functions do

# ## By: Jan Eijlers
# ## Date: 25-Jan-2015
# Matrix inversion is a costly computation. This function caches 
# the inverse of a matrix rather than compute it repeatedly. There 
# are two functions used to cache the inverse of a matrix. 
# makeCacheMatrix and CacheSolve
# Example function makeCacheVector was used as a basis for theze functions.

## Write a short comment describing this function

# makeCacheMatrix creates a list containing a function to 
# a. set the value of the matrix 
# b. get the value of the matrix 
# c. set the value of inverse of the matrix 
# d. get the value of inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL 
    set <- function(y) { 
        x <<- y 
        inver <<- NULL 
    } 
    get <- function() x 
    setinverse <- function(inverse) inver <<- inverse 
    getinverse <- function() inver
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}


## Write a short comment describing this function

# The cacheSolve function returns the inverse of the matrix. It first checks if 
# the inverse has already been computed. If so, it retrieves the result and skips the 
# computation. If not, it computes the inverse, stores the value in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
    inver <- x$getinverse() 
    if(!is.null(inver)) { 
         message("getting data from cache.") 
         return(inver) 
    } 
    data <- x$get() 
    inver <- solve(data) 
    x$setinverse(inver) 
    inver 
}

## ## Testing:

## Test1: Fill the Matrix, check if data is stored, and can be retrieved
## x <- matrix(NA,2,2)
## x[1,1] <- 1
## x[1,2] <- -2
## x[2,1] <- -2
## x[2,2] <- 1
## m <- makeCacheMatrix(x) 
## m$get() 

## Results 1
##      [,1] [,2]
## [1,]    1   -2
## [2,]   -2    1

 
## Test2: Run the Cachesolve
## CacheSolve(m) 

## Results 2
##            [,1]       [,2]
## [1,] -0.3333333 -0.6666667
## [2,] -0.6666667 -0.3333333

 
## Test3: Run test 2 again.
## Should be state the it's got from cache: "Getting ... "
## cacheSolve(m) 

## Results3:
## getting data from cache.
##            [,1]       [,2]
## [1,] -0.3333333 -0.6666667
## [2,] -0.6666667 -0.3333333