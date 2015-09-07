euclidian <- function(x,y){
  stopifnot(is.numeric(x)&&is.numeric(y))
  i <- 1
  j <- 1
  if (x>y){
    for (i in 1 : y){
      if (y%%i==0 && x%%i==0){
        a <- i
      }
    }
    return(a)
  }else if(x==y){
    return(x)
  }else if(x<y){
    for (j in 1 : x){
      if (y%%j==0 && x%%j==0){
        b <-j
      }
    } 
    return(b)
  }
}