## Put comments here that give an overall description of what your
## functions do

##  The makeCacheMatrix 
##	A. Accepts a Square Matrix as input (presumed to be always square and does not check if it is square)
##	B. Stores the matrix in the global variables area using <<- assignment operator
##	C. Defines a list of variables  
##		get returning the Matrix, 
##		set setting /overwriting the matrix, 
##		setInv setting the inverse of the matrix and storing in variable m,
##		getInv returning the inverse matrix

##  cacheSolve 
##	A.  This function takes a square matrix as input (presumed to be always square and does not check if it is square)
##	B.  calls the getInv bound to the matrix
##	C.  if the stored matrix is same as x then return the stored inverse matrix
##	D.  Otherwise, create the inverse, call the setInv followed by getInv;  if the Determinant is zero then return matrix(0)


## Write a short comment describing this function
## This function makeCacheMatrix returns a list of four functions: get, set, setInv and getInv, that store the original and inversed matrices in the global x and m variables.

makeCacheMatrix <- function(x = matrix()) {

##  set -- save the passed Matrix in global x, empty the global m which holds the inverse

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
##  get return the global x

        get <- function() x

##  setInv  take the passed parameter x ,  and store in global m
	setInv <- function(inv)	m <<- inv

##  getInv  return m

        getInv <- function() m

#   function returns set, get, setInv and getInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## Write a short comment describing this function
## This function uses the makeCacheMatrix's get, set, getInv and setInv fuctions to return either the cached inverse matrix or create one and return that inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

##  get the m and x into data from store
        m <- getInv()
        data <- get()


##  Check if the matrixes are identical and the inv is stored and is not null then return the inverse

        if ( identical(x, data ) ) {
	       if(!is.null(m)  ) {
	                message("getting cached data")
	                return(m)
 	       }
	}


##  make sure the determinant of the matrix is not zero, if zero then return a string saying inverse of the matrix is invalid as determinant is zero, and set m to null
	if ( det(x) == 0 ) {
		set(x)
		message("Invalid Inverse of Matrix as det(x) is Zero")
                m<-matrix(0)
                setInv(m)
	} else {
		message("solving inverse...")
		set(x)
	        m <- solve(x, ...)
	        setInv(m)
	}

        m
}
