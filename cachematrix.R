## Two functions to enable matrix inverse caching
## The first function takes a matrix as inverse, and returns a corresponding object that can 
## cache the inverse
## The second returns the inverse, from cache if already invoked once.

## Creates a matrix capable of caching inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL  #initialize the inverse to null
  
  #set function, clears old values of inverse
  set<-function(y) {
    x<<-y
    inverse=NULL
  }

  #get function, returns the matrix inside
  get <-function() x
  
  #set inverse, used to set inverse the first time it is computed
  setinverse<-function(inv) inverse <<-inv
  
  #returns inverse, which could be null, the first time.
  getinverse<-function() inverse

  # a list that has the necessary references to helper functions.
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## solves for inverse for a matrix created with makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #try to get inverse through helper function.
  inv<-x$getinverse()
  #if inverse is computed earlier
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  #get the matrix object
  data<-x$get()
  #compute inverse
  inv<-solve(data)
  #cache the resulting computation for future
  x$setinverse(inv)
  
  #return inverse
  inv
}
