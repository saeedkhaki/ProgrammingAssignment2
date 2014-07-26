#set matrix
#set inverse
#get matrix
#get inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        get<-function() x
        setinverse <-function(solve) inv <<- solve
        getmatrix<-function() inv
        list(set=set, get=get,
             setinverse=setinverse,
             getmatrix=getmatrix)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x=matrix(), ...) {
        
        inv <-x$getmatrix()
        
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinverse(inv)
        inv
        
}














