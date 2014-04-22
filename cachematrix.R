##Programming Assignment 2: Lexical Scoping

##Below are 2 functions used to create a special object that stores a matrix and cache's its inverse.

##The function makeCacheMatrix will create an object that will store a matrix and its inverse
##It builds a set of 4 functions and returns them to the parent environment within a list
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y ##assigns input argument to the x object to the parent environment
    m <<- NULL ##assigns the NULL value to the m object in the parent environment
    
    ##if there is already a valid inversed matrix cached in m, then
    ##whenever the value of x is reset, the value of m cached will be cleared
  }
  get <- function() x ##since x is not defined within get(), it is retrieved from the parent environment of makeCacheMatrix
  setinv <- function(inv) m <<- inv ##assigns inv to m in the parent environment
  getinv <- function() m ##since m is not defined within getinv(), it is retrieved from the parent environment of makeCacheMatrix
  list(set = set, get = get, setinv = setinv, getinv = getinv) ##assigns 4 functions to a list
}


##The function cacheSolve will retrieve the matrix inverse from the cached value
##that is stored in the makeCacheMatrix object's environment.

#It 1st checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the matrix (mat) and sets the value of the inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setinv(m)
  m
}



##Testing the functions
ma3<-matrix(c(3,2,0,0,0,1,2,-2,1),nrow=3,ncol=3)
ma3
solve(ma3)

test<-makeCacheMatrix(ma3)
cacheSolve(test)
cacheSolve(test)
