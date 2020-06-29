## makeCacheMatrix creates cached values for a new matrix and its inverse values
## which can then be returned when using the cacheSolve function.

## makeCacheMatrix sets and gets the matrix values and then it sets and gets its
## inverse. This function is required to use the following cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) i<<-inverse
        getinverse<-function() i
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve function returns the inverse values cached in the makeCacheMatrix
## function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        if(!is.null(i)) {
                message('getting cached data')
                return(i)
        }
        else {
                data<-x$get()
                i<-solve(data,...)
                x$setinverse(i)
                i
        }
}
