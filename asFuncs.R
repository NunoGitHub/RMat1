taylor <- function(x, grau){
  formula <- 1+ (funcao(grau,x)) 
  return (formula)
}

funcao <-function(grau, x){
  b = 0
  if (grau > 0){
    d <- derivar( grau)
    p <- x^grau
    f <- fibo(grau, b)
    grau <- grau- 1
    return ((d/f)*p + funcao(grau,x))
  }
  
  return (0)
}


derivar <- function(grau){
  if ((grau %% 2) !=0 ){
    return( 0)
  }
  else {if(grau%%4==0){
    return( 1)
  }
    else{
      return(-1)
    }
    
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


taylorImprimir <- function(grau){
  formula <- (Imprime2(grau))
  return (formula)
}


Imprime2 <- function(grau){
  a<-c(0, 0)
  return (polynom::polynomial(c(taylorImprimir2(grau, a))))
}


taylorImprimir2 <- function (grau, a){
  
  b = 0
  ca = 0
  while (ca <= grau){
    d<- derivar2(ca)
    f<-fibo2(ca, b)
    quo<-(d/f)
    a=append(a,c(quo), ca )
    ca <- ca + 1
    
  }
  
  return (a) 
}

derivar2 <- function(ca){
  if ((ca %% 2) !=0 ){
    return( 0)
  }
  else {if(ca%%4==0){
    return( 1)
  }
    else{
      return(-1)
    }
    
  }
}

fibo2 <- function(ca, b){
  if( ca == 0)
    return (1)
  if (ca > 0){
    b <- ca
    return(b * fibo2(ca-1,b))
  }
}


grafico<- function(grau, grau2){
  x <- seq(-5*pi,length.out=100)
  y <- cos(x)
  
  plot(taylorImprimir(grau), type = "l", 300,xlim = c(-10, 10), 
       ylim = c(-10, 10), main = "P4(x) a red---P10(x) a azul f(x)=cos(x)a preto ", col  = "red", xlab = "x", ylab = "y")
  par(new=TRUE)
  plot(taylorImprimir(grau2), type = "l", 300,xlim = c(-10, 10), 
      ylim = c(-10, 10), main = "P4(x) a red---P10(x) a azul f(x)=cos(x)a preto ", col  = "blue", xlab = "x", ylab = "y")
  par(new=TRUE)
  plot(x,y,type="l",col  = "black",main = "P4(x) a red---P10(x) a azul f(x)=cos(x)a preto ",xlim = c(-10, 10), 
       ylim = c(-10, 10))
   

}

