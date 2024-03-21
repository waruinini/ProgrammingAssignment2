
library(MASS)  #it is used to calculate inverse for non squared as well as square matrices

makeCacheMatrix <- function(x=matrix()){       #set x as an argument of the function
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function()inv
  list(set=set, get=get, setinverse=setinverse,
       getinverse=getinverse)
  
}

cacheSolve <- function(x,...){
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("Inverse is retrieved from cache")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  return(inverse)
}
