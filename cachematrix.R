## Functions that cache the inverse of a matrix
##
## Create a special "matrix", which is a list containing
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  		m <- NULL
  		set <- function(y){
  		x << -y
  		m << -NULL
}
get <- function() x
setinverse <- function(inv) m <<- inv
getinverse <- function() m
list(set = set, get = get,
   setinverse = setinverse,
   getinverse = getinverse)
}


## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)){
      	message("getting cached data")
      	return(m)
    }
    data <- x$get
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
