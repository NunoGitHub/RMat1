taylor <- function(x, grau){
  fx <- cospi(0)
  formula <- 1 + (funcao(grau, fx, x)) 
  return (formula)
}

funcao <-function(grau, fx, x){

if(grau == 0){return(0)}
  
  if (grau > 0){
    d <- derivar( grau, fx)
    p <- x^parametro
    f <- fibo(grau)
    grau <- grau- 1
    return ((d/f)*p + funcao(grau,fx, x))
  }
  
  return (0)
}

derivar <- function(grau, fx){
  if (grau >0){
    grau <- grau -1
    return(deriv(derivar(grau, fx)))
    
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
