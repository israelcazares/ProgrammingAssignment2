## The makeCacheMatrix function receives a matrix as parameter
## and creates a list that contains functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

	 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)

}


## The cacheSolve function returns the inverse of a matrix 'x'
## but first checks if the inverse has been computed.
## If the inverse matrix has been computed then the result is returned
## and the computation is skipped, if not the function computes the inverse
## and sets the value of the inverse in the cache with setmatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
