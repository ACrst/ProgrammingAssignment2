## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function is used to create a cache of the matrix which can be used for later operations

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(h){
    x<<-h
    i<<-NULL
  }
  get<-function() x
  setinverse<-function(d) i<<-d
  getinverse<-function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## Write a short comment describing this function
## This function is used to create inverse of the matrix using the cached values returned by makeCacheMatrix

## Write a short comment describing this function
##This function is used to obtain the inverse of matrix using the results from makeCacheMatrix function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  i<-x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i) #Return cached matrix
  }
  
  
  t<-x$get()
  i<-solve(t)  #Matrix multiplication to obtain inverse
  x$setinverse(i)
  i
}
