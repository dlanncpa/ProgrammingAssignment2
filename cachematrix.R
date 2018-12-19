## makeCacheMatrix creates a special matrix object that can cache its inverse.

## cashSolve computes the inverse of the special matrix returned by
## makeCacheMatrix. If the inverse has already been calculated, then cashSolve
## will return the inverse from the cache.

## Creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set<-function(y){
        x<<-y
        inv<<-NULL
    }
    get<-function()x
    setInverse<-function() inv<<-solve(x)
    getInverse<-function() inv
    list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## cashSolve computes the inverse of the special matrix returned by
## makeCacheMatrix. If the inverse has already been calculated, then cashSolve
## will return the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getInverse()
    if (!is.null(inv)) {
        message("getting cache data")
        return(inv)
    }
    data<-x$get()
    inv<-solve(data, ...)
    x$setInverse(inv)
    inv
}
