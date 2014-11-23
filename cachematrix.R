# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. 
# The assignment is to write a pair of functions that cache the inverse of a matrix
# The following two functions are used to cache the inverse of a matrix.


# Assignment consists of writing  the following functions:
# 1.makeCacheMatrix: This function creates a special "matrix" object that can 
# cache its inverse.
# 2.cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already 


# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.


# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Sample run:
x = rbind(c(2, -2/8), c(-2/8, 2))
m = makeCacheMatrix(x)
m$get()


## No cache in the first run
cacheSolve(m)


## Retrieving from the cache in the second run
cacheSolve(m)
## getting cached data.



#This is how it works with real examples:
> # Matrix inversion is usually a costly computation and there may be some benefit
  > # to caching the inverse of a matrix rather than compute it repeatedly. 
  > # The assignment is to write a pair of functions that cache the inverse of a matrix
  > # The following two functions are used to cache the inverse of a matrix.
  > 
  > 
  > # Assignment consists of writing  the following functions:
  > # 1.makeCacheMatrix: This function creates a special "matrix" object that can 
  > # cache its inverse.
  > # 2.cacheSolve: This function computes the inverse of the special "matrix" 
  > # returned by makeCacheMatrix above. If the inverse has already 
  > 
  > 
  > # makeCacheMatrix creates a list containing a function to
  > # 1. set the value of the matrix
  > # 2. get the value of the matrix
  > # 3. set the value of inverse of the matrix
  > # 4. get the value of inverse of the matrix
  > makeCacheMatrix <- function(x = matrix()) {
    +     inv <- NULL
    +     set <- function(y) {
      +         x <<- y
      +         inv <<- NULL
      +     }
    +     get <- function() x
    +     setinverse <- function(inverse) inv <<- inverse
    +     getinverse <- function() inv
    +     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    + }
> 
  > 
  > 
  > 
  > # The following function returns the inverse of the matrix. It first checks if
  > # the inverse has already been computed. If so, it gets the result and skips the
  > # computation. If not, it computes the inverse, sets the value in the cache via
  > # setinverse function.
  > 
  > 
  > # This function assumes that the matrix is always invertible.
  > cacheSolve <- function(x, ...) {
    +     inv <- x$getinverse()
    +     if(!is.null(inv)) {
      +         message("getting cached data.")
      +         return(inv)
      +     }
    +     data <- x$get()
    +     inv <- solve(data)
    +     x$setinverse(inv)
    +     inv
    + }
> 
  > ## Sample run:
  > x = rbind(c(2, -2/8), c(-2/8, 2))
> m = makeCacheMatrix(x)
> m$get()
[,1]  [,2]
[1,]  2.00 -0.25
[2,] -0.25  2.00
> 
  > 
  > ## No cache in the first run
  > cacheSolve(m)
[,1]       [,2]
[1,] 0.50793651 0.06349206
[2,] 0.06349206 0.50793651
> 
  > 
  > ## Retrieving from the cache in the second run
  > cacheSolve(m)
getting cached data.
[,1]       [,2]
[1,] 0.50793651 0.06349206
[2,] 0.06349206 0.50793651
> ## getting cached data.
  > 