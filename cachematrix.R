## Put comments here that give an overall description of what your
#install the package Matlib and the library (matlib), the function inv() inverse matrices
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #inizialise matrix
  InvA <- NULL
  set <- function(y) {
    x <<- y
    InvA <<- NULL
  }
  get <- function() x
  set_inv <- function(inv) InvA <<- inv
  get_inv <- function() InvA
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
    InvA <- x$get_inv()
    if(!is.null(InvA)) {
      message("getting cached data")
      return(InvA)
    }
    data <- x$get()
    InvA <- inv(data, ...)
    x$set_inv(InvA)
    InvA
}

#for executing the code test this exemple
#amatrix<-makeCacheMatrix(matrix(c(1,2,3, 5,12,13,1,0,0), ncol=3, nrow=3, byrow=FALSE))
# amatrix$get()
#answer
#[,1] [,2] [,3]
#[1,]    1    5    1
#[2,]    2   12    0
#[3,]    3   13    0
# cacheSolve(amatrix)
#[,1] [,2] [,3]
#[1,]    0 -1.3  1.2
#[2,]    0  0.3 -0.2
[3,]    1 -0.2 -0.2
