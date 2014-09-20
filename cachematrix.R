## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The two functions asked by this excercise are provided below
## The two examples given in the assinment page are taken as reference

##  Comments for the first function: makeCacheMatrix()
##  creates a list containing a function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        iValue <- NULL
        set <- function(y) {
                x <<- y
                iValue <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) iValue <<- solve
        getinverse <- function() iValue
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
      
}

##  Comments for the second function: cacheSolve()
##  The following function calculates the inverse of the matrix gven in the above function.
##  It first checks to see if the inverse matrix has already been calculated.
##  If so, it gets the inverse matrix and skips the computation.
##  Otherwise, it calculates the inverse matrix, sets the value in the cache
##  via the setinverse function.

cacheSolve <- function(x, ...) {
        iValue <- x$getinverse()
        if(!is.null(iValue)) {
                message("getting cached data.")
                return(iValue)
        }
        data <- x$get()
        iValue <- solve(data, ...)
        x$setinverse(iValue)
        iValue
}
