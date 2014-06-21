## The following pair of functions provide you with the means to compute 
## the inverse of a matrix and cache the inverse.  
## In the case that the inverse has already been computed, the 
## inverse will pulled from the cached data.  


## The function makeCacheMatrix takes as an input a square, invertible matrix. 
## The output is a list of four functions: set, get, setinverse, getinverse.


makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<- function(y){
                x<<-y
                i<<-NULL
        }
        get<- function() x
        setinverse<- function(inverse) i<<-inverse
        getinverse<- function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}

## The cacheSolve function, in conjuction with the makeCacheMatrix function will 
## compute the inverse of a matrix. 
## The input of cacheSolve is the output of makeCacheMatrix.  
## If the inverse is not cached, cacheSolve will compute it directly.  
## If the inverse is cached, it will take it from the cache instead of 
## re-computing it. 


cacheSolve <- function(x, ...) {
        
        i<-x$getinverse()
        if(!is.null(x$getinverse())){
                message("getting cached inverse")
                i
        }
        
        matrix<-x$get()
        i<-solve(matrix)
        x$setinverse(i)
        i
        
}
