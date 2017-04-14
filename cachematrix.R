## Put comments here that give an overall description of what your
## functions do

## function creates a matrix object (list) which can store its inverse

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y){
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinv <- function(y) x_inv <<- y
  getinv <- function() x_inv
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}


## This function solves for the inverse of a matrix (unless it's cached)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x_inv <- x$getinv() 
    if (!is.null(x_inv)){
      print("Returning cached data")
      return(x_inv)
    }
    x_inv<-solve(x$get())
    x$setinv(x_inv)
    x_inv
}
