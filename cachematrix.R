## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #set m to null
        set <- function(y) {
                x <<- y #set x's value in parent environment
                m <<- NULL #set x's value in parent environment
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse #determine inverse
        getinverse <- function() m #return inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) # give name "set" to set() and so on for other functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() #obtain value from getinverse function of makeCacheMatrix
        if(!is.null(m)) { #check if m is NOT a null value
                message("getting cached data") #return message
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m) #set m as inverse value in x
        m #returns inverse
}
