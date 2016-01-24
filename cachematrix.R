## Begining of the Rprogramming assigment2

##Author:Amitrishit Contact email:aaammiitt@gmail.com

## The following two functions compute inverse of a given square matrix 
## and catche it if computed before to avoid repeatation of expensive computation. 

## makeCacheMatrix fuction does following:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
## Note: "<<-" operator which can be used to assign a value to an object in an
## environment that is different from the current environment. 
## Below are two functions that are used to create a special object 
## that stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) Inv <<- inverse
    getinverse <- function() Inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The following cacheSolve function returns the inverse of the "matrix" created
## with the above fuction. It first checks if the inverse has already been computed.
## If so, it gets the result and skips the computation. If not, it computes 
## the inverse, sets the value in the cache via setinverse function.

## Computing the inverse of a square matrix can be done with the solve function in R.
## For example, if X is a square invertible matrix, then solve(X) returns its inverse. 
## Note:This function assumes that the matrix is always invertible 
## That means det(matrix)is not equal to 

cacheSolve <- function(x, ...) {
        Inv <- x$getinverse()
    if(!is.null(Inv)) {
        message("getting cached data.")
        return(Inv)
    }
    data <- x$get()
    Inv <- solve(data,...)
    x$setinverse(Inv)
    Inv
}
## Example of how above fuction works:
## First enter the square invertible matrix for which inverse need to be computed

## > X = matrix( 
## + c(1, 0, 1, 2, 4, 0, 3, 5, 6),
## + nrow=3,
## + ncol=3)
## > X
##     [,1] [,2] [,3]
##[1,]    1    2    3
##[2,]    0    4    5
##[3,]    1    0    6

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

## I = makeCacheMatrix(X)
## > I$get()
## [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    4    5
## [3,]    1    0    6

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

## > cacheSolve(I)
## [,1]        [,2]        [,3]
## [1,]  1.0909091 -0.54545455 -0.09090909
## [2,]  0.2272727  0.13636364 -0.22727273
## [3,] -0.1818182  0.09090909  0.18181818

## The inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.
## > cacheSolve(I)
## getting cached data.
## [,1]        [,2]        [,3]
## [1,]  1.0909091 -0.54545455 -0.09090909
##[2,]  0.2272727  0.13636364 -0.22727273
## [3,] -0.1818182  0.09090909  0.18181818

## End of the Rprogramming assigment2