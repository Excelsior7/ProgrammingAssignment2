###

# The motivation behind the makeCacheMatrix function is to be able to create a self-contained
# environment (closure setup) in which a matrix of interest and its inverse can be stored with the added benefit of limiting 
# potential corruption of their values.

# The motivation behind the cacheSolve function is to cache (in the closure setup) the correct inverse of 
# the matrix cached in the closure setup passed as an argument, with the added benefit of limiting 
# time-consuming and/or unnecessary computations by directly returning the inverse if it already exists.

###


# Description : makeCacheMatrix is a function whose execution environment are also a closure setup
# for gets and sets (the closures) that interact in this environment with the following values : a matrix and its inverse.

# Return value : makeCacheMatrix return a list of closures allowing to interact with the closure setup.

## 
# I'd like to briefly justify the code in setInverseMatrix by the flaw that this code avoids: in the code 
# given as an example by our instructors, we can (if we wanted) cache via setInverseMatrix, a matrix that is not the inverse 
# of the matrix already in the cache of the closure setup, and that's precisely the reason for cacheSolve 
# to avoid this problem. 
# So described in words, the setInverseMatrix function can only be called from cacheSolve, otherwise it 
# refuses to update matrix_inverse.

# Note to readers: if you have any recommendations on my approach, I'd be happy to take them into account, thanks.
##

makeCacheMatrix <- function(matrix = matrix()) {
  
  matrix_inverse <- NULL;
  
  getMatrix <- function() { matrix; }
  setMatrix <- function(new_matrix) { 
    matrix <<- new_matrix; 
    matrix_inverse <<- NULL;
  }
  
  getInverseMatrix <- function() { matrix_inverse; }
  setInverseMatrix <- function(matrix_inverse_new) { 
    
    isCallAccepted <- stri_detect(deparse(sys.call(which = sys.parent(1))), regex = 'cacheSolve\\(.+\\)');
    
    if(isCallAccepted) { matrix_inverse <<- matrix_inverse_new; }
    else { stop("You need to go through the cacheSolve function to change the inverse of the matrix.") }
  }
  
  list(getMatrix = getMatrix, setMatrix = setMatrix, 
       getInverseMatrix = getInverseMatrix, setInverseMatrix = setInverseMatrix);
}


# Description : cacheSolve is one interface to the closure setup passed as argument 
# and has the function to compute and cache the inverse of the matrix cached in the closure setup
# if it is not already cached in it.

# Return value : The inverse of the matrix cached in the closure setup.

cacheSolve <- function(cachematrix_closures, ...) {
  
  matrix_inverse <- cachematrix_closures$getInverseMatrix();
  
  if(is.null(matrix_inverse) == TRUE) {
    
    matrix_inverse_new <- solve(cachematrix_closures$getMatrix(), ...);
    cachematrix_closures$setInverseMatrix(matrix_inverse_new);
    
    return(matrix_inverse_new);
  }
  
  message("The inverse of the matrix has already been calculated and cached.");
  matrix_inverse;
  
}