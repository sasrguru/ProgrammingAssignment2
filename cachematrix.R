##Caching the value of an operation can be useful in saving time
##on time consuming computations by retreiving the value from the
##cache if available## 

## makeCacheMatrix creates a list containing a function to 
## 1. set the value of the matrix 
## 2. get the value of the matrix 
## 3. set the value of inverse of the matrix 
## 4. get the value of inverse of the matrix 


makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set<- function(y){
x<<- y
inv<<- NULL
}

get<- function() x
setinverse<-function(inverse)
inv<<- inverse
getinverse<-function() inv
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function returns the inverse of a matrix
## If the inverse is stored in the cache, it retreives
## the value, otherwise it recomputes it

cacheSolve <- function(x, ...) {
   
       ## Return a matrix that is the inverse of 'x'
  
  inv<-x$getinverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
inv
  }

