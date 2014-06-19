## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function
  ## Set up a matrix m in a nullspace N(m); The matrix m is defined in NULL space if the following equation is satisfied 
  ## mx = 0
  ## Get a value of the matrix
  ## Set a inverse of the matrix m; if m is a square matrix than solve(m) will return the inverse(m)
  ## For more general cases ginv(m) is the right solution. Moore-Penrose Generalized Inverse of m
  ## ginv(m) requires loading the MASS package. 

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
 
  get <- function()x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  m <- x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get() 
  m <- solve(data, ...)
  x$setsolve(m) 
 m
  
}

## Following the instructions given for vectorMean function in one of the discussions forums 
## I have tested the cacheMatrix function.
## Make the matrix b using cacheMatrix function:
## > b <- makeCacheMatrix(matrix(c(3,5,8,1,0,7,65,98,2),3,3))
## deposit the inverse of the matrix b in cache by:
## > cacheSolve(b)
##          [,1]        [,2]        [,3]
## [1,] -0.69223007  0.45711403  0.098890010
## [2,]  0.78102926 -0.51866801  0.031281534
## [3,]  0.03531786 -0.01311806 -0.005045409
## call b
## > b
## $set
## function (y) 
## {
##  x <<- y
##  m <<- NULL
## }
## <environment: 0x000000000a7ee938>
  
## $get
## function () 
##  x
## <environment: 0x000000000a7ee938>
  
##  $setsolve
## function (solve) 
##  m <<- solve
## <environment: 0x000000000a7ee938>
  
##  $getsolve
## function () 
##  m
## <environment: 0x000000000a7ee938>
## call aagain cacheSolve function to see if I will get the inverse from the cache
## > cacheSolve(b)
## and received the message: getting cached data
##          [,1]        [,2]         [,3]
## [1,] -0.69223007  0.45711403  0.098890010
## [2,]  0.78102926 -0.51866801  0.031281534
## [3,]  0.03531786 -0.01311806 -0.005045409





