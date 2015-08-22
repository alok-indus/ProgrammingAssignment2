## Put comments here that give an overall description of what your
## functions do
## These are pair of functions makeCacheMatrix and cacheSolve.
## First function returns a list containing functions that operate on the matrix that has been inputted
## Second function  cachesolve returns the cached value of the input matrix . If it does not find the cached value it 
## computes the inverse using the setMatInverse function returned by the makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  ## This function returns a list of functions that operate on the input matrix
  
  m <- NULL  ## m is a global variable. Initialized to NULL to indicate there is nothing in the cache
  setMAt <- function(y){  
    x <<- y             ## New matrix assigned to cache in a global variable x
    m <<- NULL          ## m set to NULL to indicate that there is new matrix so inverse is notin the cache
  }
  getMat <- function() x                        ## Returns the input matrix  x
  setMatInverse <- function(solve) m<<- solve   ## Use to put inverse calculated in cacheSolve in m
  getMatInverse <- function() m                 ## Returns the value of. It would eithr be null of inverse ofinput matrix
  list(setMat = setMAt, getMat = getMat,
       setMatInverse = setMatInverse,
       getMatInverse = getMatInverse)          ## create list of functions to operate on input matrix
}


cacheSolve <- function(x, ...) {
  
  ## Returns either the cached inverse of the matrix x or computes and then returns it

  m <- x$getMatInverse()    ## cheking if m ( cache variable) is not NULL, returns m if not NULL            
  if(!is.null(m)){                   
   
    return(m)                         
  } 
  data <- x$getMat()        ## IF m is found to be NULL then this code gets executed.
                            ## Returns inverse of the matrix and stores in global variable m
  m <- solve(data, ...)               
  x$setMatInverse(m)                     
}


