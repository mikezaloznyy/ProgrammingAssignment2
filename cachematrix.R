## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix() function allows to set and get a matrix, and 
## to set and get inverse of a matrix

## Sample usage:
## m <- matrix(c(-1, -2, 1, 1), 2,2)
## x <- makeCacheMatrix(m)
## x$get()
##      [,1] [,2]
## [1,]   -1    1
## [2,]   -2    1
## 
## inv <- cacheSolve(x)
## inv
##      [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1
## 
## > inv <- cacheSolve(x)
## getting cached data
## > inv
##      [,1] [,2]
## [1,]    1   -1
## [2,]    2   -1
makeCacheMatrix <- function(x = matrix()) {
    ## initialize the inverse to NULL
    m <- NULL
    
    ## setter function for our matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## getter function does a simple return of the matrix
    get <- function() x

    ## 
    setinverse <- function(inv) m <<- inv
    
    ## getter for matrix inverse
    getinverse <- function() m
    
    ## we need to bind our  new functions
    list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}


## Write a short comment describing this function
## cacheSolve() returns previously cached inverse of a matrix
## if no cached value exists, cacheSolve() will compute such an inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
	
    # if the inverse of a matrix has previously been computed, return cached value
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # if we do not have a cached value, compute inverse of a matrix
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
