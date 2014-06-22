##This function intends to cache time-consuming computations so that the computations 
##can be accessed within the cache instead of recomputing.


#This function creates a special "matrix" object that can cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix())
{
  
  set<-function(y)
  {
    x<<-y
    m<<-NULL
  }
  
  get<-function() x
  
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  
  list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
#retrieves the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) 
{
  
  m<-x$getmatrix()
  
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}