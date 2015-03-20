## function to check if inverse of a matrix exists, else cache the recent inverse computation

makeCacheMatrix <- function(x = matrix()) {
        n<-nrow(x)
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x) {
        y<-makeCacheMatrix(x)
        inv <- y$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- y$get()
        inv <- solve(data)
        y$setinverse(inv)
        inv
}
