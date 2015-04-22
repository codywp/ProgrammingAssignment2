## Creates a cached matrix and cached solved matrix to save calculation time

## makeCacheMatrix stores a matrix and its inverse and allows it to be altered

makeCacheMatrix <- function(x = matrix()) {
  inv.mat<-NULL
  set<-function(y=matrix()){
    x<<-y
    inv.mat<<-NULL
  }
  get<-function() x
  set.inv<-function(mat) inv.mat<<-mat
  get.inv<-function() inv.mat
  list( set=set, get=get, set.inv=set.inv, get.inv=get.inv)

}


## Checks to see if inverse matrix is stored, if not creates, stores, and returns it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv.mat<-x$get.inv()
  if(!is.null(inv.mat)){
    return(inv.mat)
  }
  mat<-x$get()
  inv.mat<-solve(mat)
  x$set.inv(inv.mat)
  inv.mat
}
