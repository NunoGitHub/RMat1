taylor <- function(x, grau){
  fx <- cos(x)
  formula <- 1 + (funcao(grau,x)) 
  return (formula)
}

funcao <-function(grau, x){
  b = 0
  if (grau >= 0){
    d <- derivar( grau)
    p <- x^grau
    f <- fibo(grau, b)
    grau <- grau- 1
    return ((d/f)*p + funcao(grau,x))
  }
  
  return (0)
}


derivar <- function(grau){
  if ((grau %% 2) ==0 ){
  return( 0)
  }
  else {
    return( 1)
    
  }
}


  

fibo <- function(grau, b){
  if( grau == 0)
    return (1)
  if (grau > 0){
    b <- grau
    return(b * fibo(grau-1,b))
  }
}
