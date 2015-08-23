## Put comments here that give an overall description of what your
## functions do
## These are pair of functions, makeCacheMatrix and cacheSolve.
## First function, makeCacheMatrix,  returns a list of functions that operate on the matrix that has been input
## Second function,  cacheSolve, returns the cached value of the input matrix . If it does not find the cached value it 
## computes the inverse using solve(X) function, returns  and  stores it  in cache using setMatInverse 
## In these functions lexical scoping has been used. Variable m is assigned to NULL in setMat using <<- oprator to set global m.
## If operator <- was used then m would have become local to the function setMat and NULL  check for m in 
## cacheSolve would have given unexpected result


makeCacheMatrix <- function(x = matrix()) {
  ## This function returns a list of functions that operate on the input matrix x
  
  m <- NULL             ## m is a global cache variable. Initialized to NULL to indicate there is nothing in the cache
  setMAt <- function(y){  
    x <<- y             ## input matrix y assigned to a global  variable x
    m <<- NULL          ## m a global cache variable set to NULL indicating  there is new matrix, inverse is not in the cache
  }
  getMat <- function() x                               ## Returns the matrix in global  x
  setMatInverse <- function(Solve) m<<- Solve          ## Puts inverse in m
                                                       ##Solve is argument to the function not solve(X) function
  getMatInverse <- function() m                  ## Returns value of m. m would either be null of inverse of input matrix
  list(setMat = setMAt, getMat = getMat,
       setMatInverse = setMatInverse,
       getMatInverse = getMatInverse)            ## return list of functions to operate on  matrix x
}


cacheSolve <- function(x, ...) {
  
  ## Returns either the cached inverse of the matrix in global varialble x or computes it if not present in the cache.
  ## Then returns and stores it in cache. Note the  argument variable  x  is the list of funtions that makeCacheMatrix returns.
  ## Example
  ## cacheFun <- makeCacheMatrix(inputmatrix)  # to get the list of functions in cacheFun.
  ## cacheSolve(cacheFun)

  m <- x$getMatInverse()    ## cheking if m  is not NULL. If not NULL then m has inverse, return m            
  if(!is.null(m)){                   
   
    return(m)                         
  } 
  data <- x$getMat()        ## If m is  NULL, meaning no data in cache, this code gets executed
                            ## Returns inverse of the matrix and stores it in global cache variable m
                            ## using setMatInverse 
  m <- solve(data)              
  x$setMatInverse(m)                     
}
