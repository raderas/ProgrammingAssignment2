## Put comments here that give an overall description of what your
## functions do



## Write a short comment describing this function
## Creates a special "matrix" object that can cache its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Write a short comment describing this function
## retrieves the inverse if already cached, if not it inverts the matrix, caches it 
## and then returns the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}