#makeCacheMatrix() creates a special "matrix" object (by returning a list) that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) {
		inv <<- inverse
	}
    getinv <- function() inv
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}


#cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix() 

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
	#if used for already calculated data - retrieved via getinv() at makeCacheMatrix()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}


##  TESTS:


##	Invertible 2x2 matrix
##
##	> a<-matrix(1:4,2,2) 
##	> b<-makeCacheMatrix(a)
##	> cacheSolve(b)
##	     [,1] [,2]
##	[1,]   -2  1.5
##	[2,]    1 -0.5


##  Retrieving cached data from previous hit for the invertible 2x2 matrix
##
##	> cacheSolve(b)
##	getting cached data
##	     [,1] [,2]
##	[1,]   -2  1.5
##	[2,]    1 -0.5


##  Invertible 3x3 matrix
##
##	> a<-rbind(c(1,2,3),c(0,1,5),c(5,6,0))
##	> b<-makeCacheMatrix(a)
##	> cacheSolve(b)
##		 [,1] [,2] [,3]
##	[1,]   -6  3.6  1.4
##	[2,]    5 -3.0 -1.0
##	[3,]   -1  0.8  0.2


##	Not invertible matrix (solve works with square matrices)
##
##	> a<-matrix(1:9,3,3)
##	> b<-makeCacheMatrix(a)
##	> cacheSolve(a)
##	Error in solve.default(data, ...) : 
##	  Lapack routine dgesv: system is exactly singular: U[3,3] = 0##

