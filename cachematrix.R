## https://class.coursera.org/rprog-033
##
## Peer Assessments /Programming Assignment 2: Lexical Scoping
##
## Assignment: Caching the Inverse of a Matrix
##
## Matrix inversion is usually a costly computation and there may be some benefit
##    to caching the inverse of a matrix rather than compute it repeatedly (there are
##    also alternatives to matrix inversion that we will not discuss here). Your
##    assignment is to write a pair of functions that cache the inverse of a matrix.
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache
##    its inverse.
##

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned
##    by makeCacheMatrix above. If the inverse has already been calculated (and
##    the matrix has not changed), then the cachesolve should retrieve the inverse
##    from the cache.
##

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}


##
## Test Data
##
## my_matrix$set(matrix(c(1,0,0,0,1,0,0,0,1), 3, 3))
## cacheSolve(my_matrix)
## cacheSolve(my_matrix)
##
## http://mathfaculty.fullerton.edu/mathews/n2003/Web/InverseMatrixMod/InverseMatrixMod.html
## my_matrix <- makeCacheMatrix(matrix(c(4,1,1,1,8,4,5,3,4,7,4,0,0,2,-3,-2), 4, 4))
## cacheSolve(my_matrix)
## cacheSolve(my_matrix)
##
## https://answers.yahoo.com/question/index?qid=20100128160145AA1fd6T
## my_matrix <- makeCacheMatrix(matrix(c(3, 0, 0, 0, 0, 2, -6, 0, 0, 0, 17, 14, 2, 0, 0,22, -2, 15, 8, 0,43, 12, 1, -1, 5), 5, 5))
## cacheSolve(my_matrix)
## cacheSolve(my_matrix)
##

