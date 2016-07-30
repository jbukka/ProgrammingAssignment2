## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## The following two functions are used to calculate and cache the inverse of a matrix.
## These functions assume the supplied matrix is always invertible.


## This function creates a special "matrix" object that can cache its inverse
## input: x is a square invertible matrix
## returns: special "matrix" object which is really a list containing functions to
##          1. set the matrix  2. get the matrix 
##          3. set the inverse 4. get the inverse 
## Used  the `<<- ` operator to assign a value to an object in an environment 
## different from the current environment. 

makeCacheMatrix <- function(x = matrix()) {
  
  invMat <- NULL
  
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) invMat <<- inverse
  getInverse <- function() invMat
  
  list(set = set, 
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes and returns the inverse of the matrix. If the inverse has 
## already been calculated, it gets the result from the cache and skips the computation.
## If not, it computes the inverse, sets the value in the cache via the setInverse function.
## input: x is the special matrix created by makeCacheMatrix() function 
## returns: inverse of the original matrix input to makeCacheMatrix() function


cacheSolve <- function(x, ...) {
  
  invMat <- x$getInverse()
  
  if(!is.null(invMat)) {
    message("getting cached data.")
    return(invMat)
  }
  
  data <- x$get()
  invMat <- solve(data)
  x$setInverse(invMat)
  invMat
  
}



## **** Test Runs 
## **** Case 1

## > mcm <- makeCacheMatrix(matrix(1:4,2))
## > mcm$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## *** First Run ****
## > cacheSolve(mcm)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## *** Second Run ****
## > cacheSolve(mcm)
## getting cached data.
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## **** Case 2
## ** 'set' method of the already created object is called

## > mcm$set(matrix(c(1,0,5,2,1,6,3,5,0),3,3))
## > mcm$get()
##      [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    5
## [3,]    5    6    0
## > cacheSolve(mcm)
##      [,1] [,2] [,3]
## [1,]   -6  3.6  1.4
## [2,]    5 -3.0 -1.0
## [3,]   -1  0.8  0.2
##   > cacheSolve(mcm)
## getting cached data.
##      [,1] [,2] [,3]
## [1,]   -6  3.6  1.4
## [2,]    5 -3.0 -1.0
## [3,]   -1  0.8  0.2


## Uncomment this function to use it for testing 

## testcase = function(mat){
##   ## mat: an invertible matrix

##   temp = makeCacheMatrix(mat)

##   print("Given Matrix")
##   print(mat)
##   cat("\n")
##   print(" *** First Run - inverse of the Matrix****")
##   print(cacheSolve(temp))
##   cat("\n")
##   print(" *** Second Run - inverse of the Matrix****")
##   cacheSolve(temp)
## }


## Test Results using the above testcase Function
## > testcase(matrix(c(2,3,2,1,2,1,1,1,2),3,3))
## [1] "Given Matrix"
##      [,1] [,2] [,3]
## [1,]    2    1    1
## [2,]    3    2    1
## [3,]    2    1    2

## [1] " *** First Run - inverse of the Matrix****"
##      [,1] [,2] [,3]
## [1,]    3   -1   -1
## [2,]   -4    2    1
## [3,]   -1    0    1

## [1] " *** Second Run - inverse of the Matrix****"
## getting cached data.
##      [,1] [,2] [,3]
## [1,]    3   -1   -1
## [2,]   -4    2    1
## [3,]   -1    0    1



