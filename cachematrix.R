
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix simply makes a matrix and create the functions set, get, setinv and getinv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y #cached value of y==x
    inv <<- NULL #cached value of inv==NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse #cached value of inv==inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## Write a short comment describing this function
#cacheSolve returns inv, that´s the cached inverse matrix. If inv is null, cacheSolve calculates the inverse, save it in inv and finally returns inv.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()#inv==x
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv) #if inv is not NULL, returns inv
  }
  data <- x$get()#data==x
  inv <- solve(data, ...)#inverse matrix of data
  x$setinv(inv)#store inv in cache with the value inv==x
  inv
}
