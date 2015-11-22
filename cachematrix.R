## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix stores a matrix X in memory
## cacheSolve shows the inverse of a matrix if is in memory or computes the inverse and then shows the inverse

## makeCacheMatrix uses scoping rules and stores matrices in memory
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## cacheSolve uses corpcor, a library that avoids determinants and uses orthogonal descomposition
## note: this function will try to load corpcor library and if it's not installed will try to install the library

cacheSolve <- function(x, ...) {
  if(require("corpcor")){
    print("corpcor is loaded correctly")
  } else {
    print("trying to install corpcor")
    install.packages("corpcor")
    if(require(corpcor)){
      print("corpcor installed and loaded")
    } else {
      stop("could not install corpcor")
    }
  }
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  message("inverse is not in memory so the inverse (if exist) is gonna be computed")
  data <- x$get()
  inverse <- pseudoinverse(data, ...)
  x$setinverse(inverse)
  inverse
}
