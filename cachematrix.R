## Welcome to my code! Thanks for all who participated in the forum, some of the comments were really helpful!
## The logic is the same as in the example provided
## First of all a function of functions makeCacheMatrix is constructed
## The second function takes a possible inverse, checks if it is already computed, 
##  if so the computed version is returned, otherwise, the inverse is computed and both returned and stored in the cache

## The function that creates a list of functions, one of these functions (get) includes the data of the matrix
## First line: The inverse is set to zero first
## Second line: fuction that gets the data
## Third line: setinverse: stores the inverse in environment if computed
## Fourth line: getinverse: gets the inverse
## List that includes all function
## I did not use the set part as in the example as it is not explicitly necessary in my understanding for the task performed


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() x
    setinverse <- function(inverse) {
                        inv <<- inverse                                                 
                    }
    getinverse <- function() inv
    list(get=get, setinverse=setinverse, getinverse=getinverse)
}


## function that checks if inverse exists, if so it is returned including the message "getting cached data", 
## otherwise it is computed and stored in the cache and returned
## First line: get possible inverse
## Second line: Check if inverse is there or empty
## Third line: if inverse exists print message
## Fourth line: print inverse
## Sixth line: get the data of the original matrix
## Seventh line: compute inverse
## Eigth line: store inverse in chache
## Nineth line: print inverse

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
                  }
    data <- x$get()
    inv  <- solve(data, ...)
    x$setinverse(inv)
    inv
}
