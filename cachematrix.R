## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function, makeCacheMatrix(), creates a special "matrix" object, which can cache its inverse
## 1) set the matrix
## 2) get the matrix
## 3) set the vector of inverse 
## 4) get the vector of inverse 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get<-function() x
    setinverse <-function(inverse) m <<-inverse
    getinverse <-function() m
    list(set = set, get = get, setinverse = setinverse, getinverse= getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above,
## with the help of using solve() function as suggested in the instruction of this assignment
##?solve
##This generic function solves the equation a %*% x = b for x, where b can be either a vector or a matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)  ##Computing the inverse of a square matrix can be done 
                           ##with the solve function in R. For example, if X is a square invertible matrix, 
                           ##then solve(X) returns its inverse.
    x$setinverse(m)
    m
}

