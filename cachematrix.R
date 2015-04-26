## to write the function makeCacheMatrix
makeCacheMatrix <- function(x = martix()) {
    i <- NULL
    
## set function - to set inv to null and the input argument to x,  
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

## get function - to return inv    
    get <- function() x
    
## setinv function - to set the inv with the new value
    setinv <- function(solve) i <<- solve
    
## getinv function - to get the inv value
    getinv <- function() i
    
## the return list
    list (set = set, get = get, setinv = setinv, getinv = getinv)
}

## the function cacheSolve
cacheSolve <- function(x,...) {
    
    i <<- x$getinv()
    #print(class(inv))

    if (!is.null(i)) {
        print("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data,...)
    x$setinv(i)
    i
}