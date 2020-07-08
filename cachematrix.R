## A pair of functions that cache the inverse of a matrix
## Function create a special matrix object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv<- NULL
	set <- function(y){
		x<<-y
		inv<<- NULL
		}
		get<-function()x
		setInverse<-function(solveMat)inv <<- solveMat
		getInverse<- function() inv
		list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## This function gives inverse of special matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv<- x$getInverse()
		if(!is.null(inv)){
			message(" cached data")
				return(inv)
		}
		data<- x$get()
		inv<-solve(data)
		x$getInverse(inv)
		inv
}
