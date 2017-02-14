##Create and Inverse a Cached Matrix

##Create Matrix
makeCacheMatrix <- function(X=matrix()) {
  Xinv <- NULL
  set <- function(Y) {
    X <<- Y
    Xinv <<-NULL
  }
  get <- function() {X}
  setinv <- function(inv) {Xinv <<- inv}
  getinv <- function() {Xinv}
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

##Solve Matrix
cacheSolve <- function(X,...) {
  Xinv <- X$getinv()
  if(!is.null(Xinv)) {
    message("getting cached data")
    return(Xinv)
  }
  data <- X$get()
  Xinv <- solve(data,...)
  X$setinv(Xinv)
  Xinv
}
