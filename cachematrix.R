# Submission for programming assignment 2 of R Programming Specialisation
# Coursera Nov 2015, John Hopkins Data Science course.
# 
# The below 2 functions are designed to help in calculating the inverse of matrix.
# Matrix inversion requires large computation especially when there are too many
# rows and columns. Re-running an inverse code for the same matrix (a big matrix) may
# result in the same inverse but we end up running the code again, which is time
# consuming. To simplify this, cache method is involved to reproduce the same 
# inverse matrix if there is no change in the original matrix.
#
# Here we introduce the <<- operator which can be used to assign a value to an 
# object in an environment that is different from the current environment. 
# 
# The first function below creates a special matrix and inputs it into a list 
# containing the following :
#     1. set the matrix
#     2. get the matrix
#     3. set the inverse of the matrix
#     4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    # Setting and getting the matrix
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function()   x
    
    # Setting and getting the inverse of the matrix  
    
    setinver <- function(inver)        m <<- inver
    getinver <- function()         m
    
    # Then we create a list of the above 4 functions. 
    # This is to name the sub function inside the main one.
    
    list(
        set = set, get = get,
        setinver = setinver,
        getinver = getinver
    )
}


# You can assign the above function to an object. eg :
#     b <- matrix(1:4,2,2)  -- which is an invertible matrix
#     a <- makeCacheMatrix(b)

# a$get() will have the output of the matrix 'b'
# a$getinver() will have NULL value for the initial run. 


# The below function is used for computation of the inverse. Initial run will
# result in an inverse of the special matrix. But on subsequent run, the function
# checks if there exists a cached data. If true, it will return the same result.
# Else the calculation of the inverse is initiated again.

# There is a message printed if the cached data of the inverse exists 
# for the special matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinver()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    
    # A warning or error message can be placed here to check/warn if the special 
    # matrix is not invertible.    
    
    x$setinver(m)
    m
}

# Other notes :
#      - If the function is written as one single line, 
#        eg : cacheSolve(makeCacheMatrix(matrix(1:4,2,2)))
#        it will run the code perfectly, but it will not refer the cache data
#        (even if it is the same matrix) as the $getinver() variable is NULL 
#        everytime you define the makeCacheMatrix function.