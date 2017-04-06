#makeVector <- function(x = numeric()) {
#        m <- NULL
#        set <- function(y) {
#                x <<- y
#                m <<- NULL
#        }
#        get <- function() x
#        setmean <- function(mean) m <<- mean
#        getmean <- function() m
#        list(set = set, get = get,
#             setmean = setmean,
#             getmean = getmean)
#}



# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse_1 <- NULL
        set <- function(y) {
                x <<- y
                inverse_1 <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse_1 <<- inverse
        getinverse <- function() inverse_1
        list(set=set, 
             get=get, 
             setinverse=setinverse, 
             getinverse=getinverse)
}


#cachemean <- function(x, ...) {
#        m <- x$getmean()
#        if(!is.null(m)) {
#                message("getting cached data")
#                return(m)
#        }
#        data <- x$get()
#        m <- mean(data, ...)
#        x$setmean(m)
#        m
#}

               
#The following function inverses the special "matrix" created with the above function. 
#However, it first checks to see if the invers has already been done. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it computes the inverse, sets the value in the cache via the setinverse function.
                
cacheSolve <- function(x, ...) {
        inverse_1 <- x$getinverse()
        if(!is.null(inverse_1)) {
                message("getting cached data.")
                return(inverse_1)
        }
        data <- x$get()
        inverse_1 <- solve(data)
        x$setinverse(inverse_1)
        inverse_1
}









