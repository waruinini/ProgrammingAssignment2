
library(MASS)  #it is used to calculate inverse for non squared as well as square matrices

makeCacheMatrix <- function(x=matrix()){       #set x as an argument of the function and 
                                                #matrix is initially set to be empty 
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x          #retrieve the value of the matrix               
  setinverse <- function(inverse) inv <<- inverse          # set the inverse of the matrix
  getinverse <- function()inv                              # retrieve the inverse of matrix
  list(set=set, get=get, setinverse=setinverse,
       getinverse=getinverse)  
}


cacheSolve <- function(x,...){     #the cachesolve function takes special matrix object created by makeCacheMatrix as input  
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("Inverse is retrieved from cache")
    return(inverse)         #check if inverse is already cached using the getinverse method
  }
  data <- x$get()         #if inverse is not cached, then compute inverse
  inverse <- solve(data, ...)
  x$setinverse(inverse)    #cache the inverse 
  return(inverse)
}
