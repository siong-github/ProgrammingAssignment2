## Put comments here that give an overall description of what your
## functions do
#ASSUMPTIONS:- Matrix is 2x2 and already defined matrix object
#makeCacheMatrix performs following function:
#1)Inputs 2x2 matrix object as 'x' into function environment
#2)get() - returns the value of matrix 'x'
#3)reinit() - reinitialises cache to null for the next new matrix inverse calculation
#3)getinverse() - get matrix data (inversed) from cache and return matrix values
#4)setinverse() - sets matrix data (inversed) to cache

makeCacheMatrix <- function(x = matrix()) {
	CACHE <- NULL
	get <- function() return(x)
	reinit <- function() {
		CACHE <-NULL
	}

	getinverse <- function() return(CACHE)
	setinverse <- function(inverse = matrix()) CACHE <<- inverse

	list(get = get, reinit=reinit, getinverse=getinverse, setinverse=setinverse)
}

# CacheSolve
# Calculates the inverse of 2x2 matrix using solve() and write to cache
# Inputs makeCacheMatrix object as 'x'
# Gets data from cache - If null, get data from matrix,calculate inverse, write in cache and return result
# If data from cache is not null - assume that cache already has inverse matrix stored and return this as result

cacheSolve <- function(x, ...){
	LocalTemp <- x$getinverse()
	if(!is.null(LocalTemp)){
		message("getting cached data")
		return(LocalTemp)
	}
	MatrixData <- x$get()
	#print(class(MatrixData))
	LocalTemp <- solve(MatrixData)
	x$setinverse(LocalTemp)
	return(LocalTemp)
}
