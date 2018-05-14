taylor <- function(x, grau){
  fx <- cos(x)
  fx0 <- cos(0)
  formula <- 1 + (funcao(grau, fx0, x)) 
  return (formula)
}



funcao <-function(grau, fx0, x){
  a = 0
if(grau == 0){return(0)}
  
  if (grau > 0){
    d <- derivar( grau, fx0, a)
    p <- x^parametro
    f <- fibo(grau)
    grau <- grau- 1
    return ((d/f)*p + funcao(grau,fx0, x))
  }
  
  return (0)
}

derivar <- function(grau, x, a){
  if (grau >0){
    a <- deriv(fx0)
    grau <- grau -1
    return(deriv(derivar(grau, fx0, a)))
    
    }
  
  return (0)
  
}
fibo <- function(grau){
  if( grau == 0)
    return (1)
  if (grau > 0){
    grau<- grau - 1
    return(grau * fibo(grau))
  }
}
