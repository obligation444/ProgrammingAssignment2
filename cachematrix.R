## This function cache the value of the matrix that is given and inverse it 
##afterwards
## It stores the value of the matrix and the value of the matrix that is inversed 

## This function caches the value of the matrix in the set function and can get
## it from the get function.
## If it was already inversed, the inversed matrix will be cache in the 
## setinverse function and get it from the getinverse function

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setinverse<-function(inverse) m<<-inverse
getinverse<-function() m
list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function can verify the matrix is calculated previously.
## If it was, then it returns the matrix, otherwise it inverses it and show the
## result

cacheSolve <- function(x, ...) {
       m<-x$getinverse()
       if(!is.null(m)){
         message("getting data")
         return(m)
       }
       data<-x$get()
       m<-solve(data,...)
       x$setinverse(m)
       m
       ## Return a matrix that is the inverse of 'x'
}
