## Below are two functions that are used to create a special object 
## that stores a matrix and caches its inverse

## The first function (makeCacheMatrix) creates a special object,
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get <- function() x
  setinverse <- function(solve) i<<-solve
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The following function (cacheSolve) calculates the inverse of the special
## object created with the above function. However, it first checks to see
## if the inverse has already been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse 
## of the data and sets the value of the inverse in the cache via the setinverse
## function

cacheSolve <- function (x, ...){
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  print(i)
}

## Example:
## Create a matrix b:
## > b<-matrix(c(1:4),2,2)
##
## Run the first function to create the list of four functions:
## > a<-makeCacheMatrix(b)
##
## a$get() will return your matrix
## > a$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## Run the second function to calculate the inverse of the matrix:
## > cacheSolve(a)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## If you run the second function again, the inverse of the matrix has already
## been stored via the setinverse function, therefore the function will use
## the cached data and not calculate the inverse again. A message 
## (getting cached data) will appear, to inform you about this
## > cacheSolve(a)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
