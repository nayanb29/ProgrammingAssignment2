## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	invMatrix = NULL
  	set = function(y) {
    	x <<- y
    	invMatrix <<- NULL
  	}
  	get = function() x
  	setInv = function(invertedMatrix) invMatrix <<- invertedMatrix
  	getInv = function() invMatrix
  	list(set = set, get = get,
       	setInv = setInv,
       	getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invMatrix = x$getInv()
 	if(!is.null(invMatrix)) {
    	message("getting cached data")
    	return(invMatrix)
 	}
  	m <- x$get()
  	invMatrix <- solve(m)
  	x$setInv(invMatrix)
  	invMatrix
}
