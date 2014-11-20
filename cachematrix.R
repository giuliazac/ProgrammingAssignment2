## Here are two functions. 
## The first one allows to store the original value of a matrix called x and what will be its inverse value (originally set to NULL). 
## The second one assesses x and if its inverse has already been calculated, it reports the value, otherwise it calculates it.

## makeCacheMatrix is made up of four functions which allow to: set the value of the matrix, get the value of the matrix, set the value of the inverse, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {       # I'm creating a function and its argument x is a matrix
      m<- NULL                                    # m will be the inverse of x and it's reset to NULL every time makeCacheMatrix is called
      
      set<- function(y){                          # The set function allows me to change the input x as I want. This function takes an input y,
            x<<- y                                # saves the input matrix y in x
            m<<- NULL                             # and resets the inverse of the matrix to NULL.  
      }
      get<- function() {x}                        # This function returns the value of the original matrix     
      setinverse<- function(solve) {m<<-solve}    # This function is called by cachesolve() during the first cachesolve() access and it will store the resulted value using superassignment
      getinverse<- function() {m}                 # This function returns the cached value to cachesolve() on subsequent access
      
      list(get=get, setinverse=setinverse, getinverse=getinverse) #This is a list of the internal functions so a calling function knows how to access to those methods
}

## cacheSolve first checks if the inverse of the matrix has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cachesolve<-function(x, ...) {                    # The input x is an object (a matrix) created by makeCacheMatrix
      m<- x$getinverse()                          # It accesses the object x and gets the value of the inverse
      
      if(!is.null(m)) {                           # If the inverse was already cached so it is not a NULL value
            message("getting cached data")        # I send this message to the console
            return(m)                             # and I get the previous calculated inverse
      }
      
      data<- x$get()                              # If x$getinverse() returned NULL,
      m<- solve(data, ...)                        # it calculates the inverse
      x$setinverse(m)                             # and stores it in x (because setinverse() in makeCacheSolve does it)
      
      m                                           # Finally, I obtain the inverse to the code that called this function
}
