
# Analog to makeVector example, the makeCacheMatrix fucntion  ...  
#  1. sets the value of the matrix
#  2. gets the value of the matrix
#  3. sets the value of the inverse matrix
#  4. gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  mi <- NULL
  set <- function(y) {
    xi <<- y
    mi <<- NULL
  }
  get <- function() xi
  setinverse <- function(inv) mi <<- inv
  getinverse <- function() mi
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


# Analog to cachemean example, the cacheSolve function  ...  
# ..checks to see if inverse has already been calculated 
# reuses cached result by getting the inverse from the cache & skips computation
# Otherwise computes inverse via solve function

cacheSolve <- function(x, ...) {
  mi <- x$getinverse()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  m <- x$get()
  mi <- solve(m, ...)
  x$setinverse(mi)
  mi
}

# Validation 
m <- makeCacheMatrix()
m$set(matrix(c(1,100,100,1),2,2))
m$get()
cacheSolve(m)
cacheSolve(m) #cached

