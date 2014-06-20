

## Write a short comment describing this function
  ## Initialize a matrix x in a nullspace N(x) with a command set; Some arbitrary matrix A is defined in NULL space N(A)
  ## if the following equation is satisfied Ax = 0, where x is set of vectors that satisfy this equation.
  ## Assign some value of the matrix with the command get
  ## Calculate the inverse of the matrix x; if x is a square matrix than solve() will give the inverse(x), denoted as m here
  ## For more general cases ginv(x) is the right solution. Moore-Penrose Generalized Inverse of x
  ## ginv() requires loading the MASS package. 
  ## The inverse value of the matrix is deposited in the cache. The operator <<- assign the value of the object in
  ## environment that is different than the current environment. 
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


## Return a matrix that is the inverse of 'x'. Once the inverse is calculated and stored in cache, cacheSolve function look
## at the cache and take the inverse value of the matrix from the cache. If the inverse is different than zero, the function 
## send a message "getting data from cache", and read the value from cache.
cacheSolve <- function(x, ...) {
  
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

## >$set
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





