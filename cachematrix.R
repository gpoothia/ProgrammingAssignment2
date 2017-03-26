## Code similar to the mean caching example in assignment.

## Test case:
## Create a random matrix M:
##   M = matrix( 
##   c(2, 4, 3, 1, 5, 7, 4 , 5 ,6), 
##   nrow=3, 
##   ncol=3) 
## Create the special cache obj:
##   sp <- makeCacheMatrix(M)
## Call cacheSolver first time to get calculated inverse:
##   cacheSolve(sp)
## Call it a second time to get chached inverse but with additional message: "getting cached inverse" 
## You may compare results above with direct inverse calculation:
##    solve(M)

## Create a special vector with get, set, getinv and setinv 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function that accepts the special vector. 
## First invocation will calculate inverse and cache 
## Subsequent invocation will retrieved cached inverse
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x' 
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
