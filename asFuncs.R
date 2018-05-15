taylor <- function(x, grau){
  fx <- cos(x)
  formula <- 1 + (funcao(grau,x)) 
  return (formula)
}

funcao <-function(grau, x){
  a = 0
if(grau == 0){return(0)}
  
  if (grau > 0){
    d <- derivar( grau, a)
    p <- x^parametro
    f <- fibo(grau)
    grau <- grau- 1
    return ((d/f)*p + funcao(grau,x))
  }
  
  return (0)
}

derivar <- function(grau,  a){
  if (grau >=0){
    a <- (cos(0))
    grau <- grau -1
    return(deriv(derivar(grau, a),"x"))
    
    }
  
  return (a)
  
}
fibo <- function(grau){
  if( grau == 0)
    return (1)
  if (grau > 0){
    grau<- grau - 1
    return(grau * fibo(grau))
  }
}
