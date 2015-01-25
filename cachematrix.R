## [Creates cache in order to retrieve the later when needed]
## Same methodology with the vector example
## 1. Set the matrix 2. Get the matrix 3. Set the inverse 4. Get the inverse 


makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list( set=set, get = get,
             setinv = setinv,
             getinv = getinv)
        
        
        
        
}


cacheSolve <- function(x,...) {
        ## Call the get method to see if there is an earlier calculation
        m <- x$getinv()
        if(!is.null(m)) {
                
                if (identical(x$set(y),x$get())==TRUE){
                ## If there is earlier calculation or same data set it provides a below message and returns already claculated number
                   message("getting cached data")
                   return(m)
                }
        }
        ## if there is no earlier calculation, the code needs to solve the inverse and set it for future use
        y <-x$get() ## get the y for setting the old matrix
        x$set(y)   ## set the old matrix so that we can compare later if the new matrix is the same
        m <- solve(y, ...)
        x$setinv(m)
        m
}
