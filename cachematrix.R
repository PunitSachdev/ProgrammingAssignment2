## Put comments here that give an overall description of what your
## functions do
## The two R functions below namely makeCacheMatrix and cacheSolve make use of R lexical
## scoping rules to cache results between function calls. They also use <<- operator that can be
## used to assign a value to a variable in an environment that is different from the current/calling 
## environment.


# makeCacheMatrix function accepts x a matrix as its argument caches the matrix
## and its inverse in locally scoped variables, returns a list containing function pointers
## to get and set those variables
makeCacheMatrix <- function(x = matrix()) {
  
  #initialize variable mi to store matrix inverse
  mi <- NULL
  # function to store the variable containing the matix and its inverse in its own env
  set <- function(y)
  {
    x <<- y
    mi <<- NULL
  }
  # get matrix variable passed to the main function as argument
  get <- function() x
  # function to set mi variable in its own env
  setmtinverse <- function(micalcval) mi <<- micalcval
  # get the mi value either NULL or inverted matix
  getmtinverse <- function() mi
  # return a list with function pointers
  list(set = set,get = get,setmtinverse = setmtinverse,getmtinverse = getmtinverse)

}


## cacheSolve function accepts a variable containing the pointer to the function makeCacheMatrix as an argument
## It checks whether makeCacheMatrix function pointer already has the matix and its inverse cached
## if not it calculates the inverse of the matix and caches it into a variable defined in the scope of makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mi <- x$getmtinverse()
  # check if matrix inverse is cached, if its it returns the cached variable
  if(!is.null(mi))
  {
    message("getting cached data")
    return(mi)
  }
  # else get the matix, calculate the inverse and cache it in a variable
  midata <- x$get()
  mi <- solve(midata)
  x$setmtinverse(mi)
  mi
}
