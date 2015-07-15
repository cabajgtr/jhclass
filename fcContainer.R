fcStore <- function(x = list()) {
     ##   First initialize fc and acc (which will store the fcast and related accuracy item) before we set it
     fc <- NULL
     acc <- NULL
     ##   $set() will store a new "primary" matrix in the object.  x will already have been set when
     ##   you first create an makeCacheMatrix object, this will change it and clear any cache 'v'
     appendAcc <- function(a) {
          if(is.null(acc)) acc <<- data.frame(a)     
               else acc <<- rbind(acc,data.frame(a))
               
          }
     appendFC <- function(f) fc <<- c(fc,f)

     ## $get() simply returns the stored lists
     getAcc <- function() acc
     getFC <- function() fc
     ## this definese the output of the function, which will now be a "list" of functions
     list(appendAcc = appendAcc, 
          appendFC = appendFC,
          getAcc = getAcc,
          getFC = getFC)
}