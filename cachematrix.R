## These functions are responsible to make a given matrix invertible, supposing
## the given matrix isn't singular (in other words, people can multiply the
## given matrix by same squared identity matrix and then receive a result that's 
## true).

## makeCacheMatrix function stores a special matrix which gets all values from
## a user-given matrix, then keeps these values and turns them into the inverse 
## matrix, from that first matrix inserted by any user.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve first verifies if the data inserted is correct. If it's right, 
## then it gets the inverse matrix from cache and skips computation. Or, it just
## calcultates the inverse data and sets the inverse values in cache by 
## setinverse function. In the end, it presents invertible matrix into the prompt.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
