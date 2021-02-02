## Functions to cache an inverse Matrix

## Makes the inverse cache

makeCacheMatrix <- function(x = matrix()) {
  #the inverse property
  i <- NULL
  #Set the matrix
  set <- function(matrix){
      m <<-matrix
      i <<- NULL
  }
  #Get & return the matrix
  get <- function(){
      m
  }
  #Set the inverse matrix
  sInverse <- function(inverse){
      i <<- inverse
  }
  #Get the inverse matrix
  gInverse <- function(){
      i
  }
  #Return methods list
  list(set=set, get=get, sInverse=sInverse,
       gInverse=gInverse)

}


## Return a matrix that is the inverse of 'x

cacheSolve <- function(x, ...) {
  m <- x$gInverse()
  
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  #get the matrix
  data <- x$get()
  #calculate inverse matrix
  m <- solve(data)%*%data
  #Set the inverse
  x$sInverse(m)
  #return the matrix
  m
}
