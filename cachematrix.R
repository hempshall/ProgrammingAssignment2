## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	
	set <- function(y){
  		x <<- y
  		m <<- NULL
	}
	get<-function(){
		x
	}
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

## The solve(X)function calculates the inverse of a matrix
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x=matrix(), ...) {

    	m <- x$getmatrix()
    	if(is.null(m)){
		matrix<-x$get()
		m<-solve(matrix, ...)
		x$setmatrix(m)
	}
	else{
	      message("getting cached data")
	}
	m
}

## The following code defines a matrix and inverts the cached matrix

x <- c(1,3)
y <- c(2,4)
Matrix22 <- cbind(x,y)
a<-makeCacheMatrix()
a$set(Matrix22)
cacheSolve(a)
