## These two functions save time for computing an inverse of a matrix
## If the inverse is already calculated the inverted matrix is cashed and there is no need to calculate the inverse again

## this function reserves a place in memory for a matrix inverse by creating a list of functions

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse<-NULL
  set<-function(y){
    x<<-y
    matrix_inverse<<-NULL
  }
  get<-function() x
  setinverse<- function(inverse) matrix_inverse<<-inverse
  getinverse<-function() matrix_inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## this function checks if the inverse of a matrix 'x' already exists, if yes, then it returns the cashed inverse, if not, then it calculates the inverse

cacheSolve <- function(x, ...) {
  matrix_inverse<-x$getinverse()
  if(!is.null(matrix_inverse)){
    message("getting cached data")
    return(matrix_inverse)
  }
  data<- x$get()
  matrix_inverse<-solve(data, ...)
  x$setinverse(matrix_inverse)
  matrix_inverse    ## Return a matrix that is the inverse of 'x'
}
