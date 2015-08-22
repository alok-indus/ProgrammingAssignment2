## Put comments here that give an overall description of what your
## functions do
## These are pair of functions makeCacheMatrix and cacheSolve.
## First function returns a list of functions that operate on the matrix that has been input
## Second function  cacheSolve returns the cached value of the input matrix . If it does not find the cached value it 
## computes the inverse using the setMatInverse function returned by the makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  ## This function returns a list of functions that operate on the input matrix x
  
  m <- NULL  ## m is a global variable. Initialized to NULL to indicate there is nothing in the cache
  setMAt <- function(y){  
    x <<- y             ## input matrix assigned to a global  variable x
    m <<- NULL          ## m set to NULL to indicate that there is new matrix so inverse is not in the cache
  }
  getMat <- function() x                        ## Returns the input matrix  x
  setMatInverse <- function(solve) m<<- solve   ## Used to put inverse calculated in cacheSolve in m
  getMatInverse <- function() m                 ## Returns the value of. It would eithr be null of inverse ofinput matrix
  list(setMat = setMAt, getMat = getMat,
       setMatInverse = setMatInverse,
       getMatInverse = getMatInverse)          ## create list of functions to operate on input matrix
}


cacheSolve <- function(x, ...) {
  
  ## Returns either the cached inverse of the matrix x or computes and then returns it

  m <- x$getMatInverse()    ## cheking if m ( cache variable) is not NULL,means inverse  is in cache, return m            
  if(!is.null(m)){                   
   
    return(m)                         
  } 
  data <- x$getMat()        ## IF m is  NULL, meaning no data in cache, this code gets executed
                            ## returns inverse of the matrix and stores in global variable m
  m <- solve(data, ...)               
  x$setMatInverse(m)                     
}
