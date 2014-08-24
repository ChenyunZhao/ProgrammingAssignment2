## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x=matrix()){
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse)i<<-inverse
  getinverse<-function()i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

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

