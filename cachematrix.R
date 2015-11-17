##My function creates an object (matrix) able to cache its inverse.
##Then, another function (cacheSolve) calculates the inverted matrix

##This first function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

##This function calculates the inverse matrix computated by makeCachematrix function
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

##In order to use this script, the user should write the following commands:
## a<-cacheMatrix() (stores a function in a variable)
## a$set(matrix(1:4,2,2)) (creates a 2x2 matrix and stores it in a)
## cacheSolve(a) (executes our code, and solves our 'cache' matrix)
