# makeCacheMatrix - a function with the functions (set, get,setmatrix,getmatrix), handles
#                   the setters and getters for the plain matrix and the cached matrix

#cacheSolve - a functin that checks to see if the inverse of the matrix is already in cache
#             if so it just returns the cache.  If not then it solves it and sets it in cache
#             and then returns it.  The idea is to compute the inverse if not already in cache.


makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  
  #set the value of the matrix in cache, also NULLS m from any previous sets
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  # get the value of the matrix
  get<-function(){
    return (x)
  } 
  
  #set the inverse of the matrix in cache, note the actual call to solve is in cachSolve
  setinversematrix<-function(inverse){
    m<<- inverse
  } 
  
  #get the inverse of the matrix that has been set
  getinversematrix<-function(){
    return (m)
  } 
  
  list(set=set, get=get,
       setinversematrix=setinversematrix,
       getinversematrix=getinversematrix)
}


# solves the inverse of a matrix.  returns cache if not null, otherwise solves, sets, and returns
# the inverted matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinversematrix()
  
  #check to see if the inverse of the matrix was set, if so just get return the cache
  if(!is.null(m)){
    message("m is not null so we got cached data")
    return(m)
  }
  
  #get the matrix, not the inverse
  matrix<-x$get()
  
  #call solve to find the inverse of matrix
  m<-solve(matrix, ...)
  
  #set the inverse of the matrix
  x$setinversematrix(m)
  
  return (m)
}
