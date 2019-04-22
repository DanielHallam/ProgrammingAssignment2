## function that holds a closed environment for a matrix allowing the below functions
## set - sets the value of the matrix
## get - gets the value of the matrix
## setinverse - sets the inverse value of the matrix
## getinverse - gets the inverse value of the matrix
## list - lists the available functions

makeCacheMatrix <- function(x = matrix()) {
	## default inverse to nothing
	i <- NULL
	## sets matrix and resets inverse
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	## gets current value of matrix
	get <- function() x
	## sets value of inverse matrix by passing function that can perform solve
	## expect to use cacheSolve function to set inverse
	setinverse <- function(solve) {
		i <<- solve
	}
	## gets value of inverse matrix
	getinverse <- function() {
		i
	}
	## lists functions
	list(
		set = set, 
		get = get,
		setinverse = setinverse,
		getinverse = getinverse
	)
}


## calculates the inverse of a matrix
## if previously calculated, uses cached result
cacheSolve <- function(x, ...) {

	## get current value of inverse
	i <- x$getinverse()
	
	## if inverse already populated, use existing value
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	
	## calculate inverse by getting matrix and using solve function
	data <- x$get()
	i <- solve(data, ...)
	
	## set inverse
	x$setinverse(i)
	
	## return inverse matrix
	i
}
