## The below pair of functions will cache the inverse of a matrix.


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 
  inverseM <- NULL
  
  getInverseM <- function () inverseM
  
  setInverseM <- function (x) inverseM <<- x
  
  getData <- function () x
  
  setData <- function(y){
    inverseM <<- NULL
    x <<- y
  }
  list(setData = setData, getData = getData,
       setInverseM = setInverseM,
       getInverseM = getInverseM)
  
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve will retrieve the inverse from the 
## cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseM <- x$getInverseM()
  
  if(!is.null(inverseM)){
    message("getting cached object")
    return(inverseM)
  }
  
  data <- x$getData()
  inverseM <- solve(data)
  x$setInverseM(inverseM)
  inverseM
}
