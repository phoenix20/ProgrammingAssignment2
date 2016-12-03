## TWO FUNCTION IN THIS FILE ! 
## makeCacheMatrix and cacheSolve


## makeCacheMatrix function creates a special vector object which is a list of 
##four functions namely _ get , set , getInverse and setInverse.The name clearly
## states their purpose ! :) 

makeCacheMatrix <- function(x = matrix()) {
      inverse<-NULL
      ##set function#####################################################
      
      set <- function(y){
    
              x<<-y
              inverse<<-NULL
      
      }
      ##get function#####################################################  
      
      get <- function() x 
      
      ##setInverse function##############################################
      
      setInverse <- function(value) {
    
              inverse<<-value
      }
  
      ##getInverse function##############################################
      getInverse <- function() inverse
  
  
      list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## cacheSolve function checks if the value of the inverse has been previously
## computed and if it has been then it returns the cached value.In case it hasn't
## been previously computed , a fresh computation is performed and the value is 
## cached ! :)

cacheSolve <- function(x, ...) {
 
    
    inverse <- x$getInverse()
    
    if(is.null(inverse)){
           message("Computing the matrix")
          value <- solve(x$get())
          x$setInverse(value)
          return(value)
      
    }else {
           message("getting cached data")
          return(inverse)
    }
  
}
