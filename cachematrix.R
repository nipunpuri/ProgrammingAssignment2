## this functions takes a matrix as an input and returns a list

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <- NULL
    }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv  ##returns inverse which is equal to inverse set in cacheSolve function
  getinverse <- function() inverse      ##returns the inverse function
  list(set = set , get = get , getinverse = getinverse , setinverse = setinverse)
}


## This function caclulates the inverse if it is not available

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)){
        message("Getting the chached inverse")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data, ...)  ##calculating inverse and saving in inverse
    x$setinverse(inverse)   ##calling the setinverse function and returning the inverse caluclated above
    inverse
        ## Return a matrix that is the inverse of 'x'