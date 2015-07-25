## Here are two functions makeCacheMatrix() and cacheSolve(). The first one, makeCacheMatrix(), is a function that generates a list of 4 functions and stores the inverse of input argument. The second function, cacheSolve(), computes and returns the inverse of input of makeCacheMatrix(). If the inverse already exists, cacheSolve() returns it  directly without any calculation. 

## makeCacheMatrix() stores 4 functions set(), get(), setinv(), and getinv(), and the inverse 'invm' of input 'm'. 
makeCacheMatrix <- function(m = matrix()) {
		invm <- NULL
        set <- function(x) { 
                m <<- x
                invm <<- NULL
        } #set(): replace input 'm' with 'x' and reset the inverse 'invm'
        get <- function() m #get(): print 'm'.
        setinv <- function(inv) invm <<- inv #setinv(): set the value of 'invm' by 'inv'.
        getinv <- function() invm #getinv(): print 'invm'.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve() computes the inverse of input of makeCacheMatrix() and stores the result in makeCacheMatrix(). If the inverse already exists, it shows a message and returns the inverse without any computation.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv() #get the information of inverse.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } #if the inverse exists, returns it without computation.
        data <- x$get() #get the information of input of makeCacheMatrix().
        m <- solve(data, ...) #find the inverse.
        x$setinv(m) #store the inverse into makeCacheMatrix().
        m #print the inverse.
}
