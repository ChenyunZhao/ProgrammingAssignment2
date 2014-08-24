#makeCacheMatrix: This function creates a special "matrix" object
#that can cache its inverse.

makeCacheMatrix <- function(x=matrix()){
  i<-NULL
  #set matrix
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  #get matrix
  get<-function() x
  #set inverse matrix
  setinverse<-function(inverse)i<<-inverse
  #get inverse matrix
  getinverse<-function()i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve
#the inverse from the cache.

cacheSolve<-function(x,...){
  i<-x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i
}

#TEST FUNCTION
a<-makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
summary(a)

# Length Class  Mode    
# set        1      -none- function
# get        1      -none- function
# setinverse 1      -none- function
# getinverse 1      -none- function

a$get()
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4

#Will get the inverse matrix of a
cacheSolve(a)
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

#second time run the function, we  will get the cached value
cacheSolve(a)
# getting cached data
#       [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

