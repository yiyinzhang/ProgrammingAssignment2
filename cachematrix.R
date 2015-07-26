## Put comments here that give an overall description of what your
## functions do

## Below are two functions that are used to create a special object 
## that stores a matrix and caches its inverse.

## Write a short comment describing this function

## The first function, makeCacheMatrix creates a special "matrix", which 
## is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv.mat=NULL
	set=function(y){
		x<<-y
		inv.mat<<-NULL
	}
	get=function()x
	setinverse=function(inverse)inv.mat<<-inverse
	getinverse=function()inv.mat
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse from
## the cache and skips the computation. Otherwise, it calculates the inverse
## of the data and sets the value of the inverse in the cache via the 
## setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv.mat=x$getinverse()
	if(!is.null(inv.mat)){
		message("getting cached data")
		return(inv.mat)
	}
	data=x$get()
	inv.mat=solve(data,...)
	x$setinverse(inv.mat)
	inv.mat
}
