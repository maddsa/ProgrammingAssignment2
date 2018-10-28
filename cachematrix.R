## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}

##  m <- matrix(rnorm(9),3,3)
##  m3 <- makeCacheMatrix(m)
##  cacheSolve(m3)
##        [,1]       [,2]       [,3]
##  [1,] 0.5731205 -0.5333043 -2.2725906
##  [2,] 0.8065996 -0.2105556 -1.3881825
##  [3,] 0.2243270 -0.5488767 -0.5919234
##  cacheSolve(m3)
##  getting cached data
##        [,1]       [,2]       [,3]
##  [1,] 0.5731205 -0.5333043 -2.2725906
##  [2,] 0.8065996 -0.2105556 -1.3881825
##  [3,] 0.2243270 -0.5488767 -0.5919234



