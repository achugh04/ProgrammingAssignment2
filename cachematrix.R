## The functions makeCacheMatrix and so the created object has the attributes as stated above
## moreover the object so created can change the value of the cached matrix without
## sourcing the code all over again

## makeCacheMatrix creates and returns an object
## which has the functions set(),get(),getinverse(),setinverse() as attributes


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(whatevs) inv <<- whatevs
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function takes an object of type makeCacheMatrix 
## and returns the inverse of the matrix in the object

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinverse(inv)
  print(inv)
}
