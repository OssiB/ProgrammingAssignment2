## Creates a list  of methods: 
## -set ->the value of the matrix
## -get-> the value of the matrix
## -setinverse->sets the value of the inverse   
## -getinverse->gets the value of the

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
    set<-function(y){
    	x<<-y
    	inv<<-NULL
    }
    get<-function()x
    setinverse<-function(solve) inv<<-solve
    getinverse<-function()m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## uses m
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)){
            message("getting cached inverse")
            inv
        }
        # inverse is not cached
        data<-x$get()
        #solve function returns inverse of the matrix
        inv<-solve(data,...)
        x$setinverse(inv)
        inv

}
