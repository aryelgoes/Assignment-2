## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inv<- NULL
  set<- function(y) {
    x<<- y
    inv <<- NULL
  }
  get<- function() x
  setInverse<- function(inverse) inv<<- inverse
  getInverse<- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function
#this function should inverse the "matrix" above
#the cachesolve (below) must retrieve the inverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getInverse()
  if(!is.null(inv)) {
    message("cached data")
    return(inv)
  }
  data<- x$get()
  inv<- solve(data, ...)
  x$setInverse(inv)
  inv
}
#checking the script
m <- matrix(rnorm(16),4,4)
m1 <- makeCacheMatrix(m)
cacheSolve(m1)
