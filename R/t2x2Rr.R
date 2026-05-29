#' @noRd
#' @title Estimacion del riesgo relativo  (a partir de una tabla de contingencia 2x2)
#' @description Procedimiento interno para tablas de contingencia 2x2. Calculo del riesgo relativo con IC. (Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres)
#' @param x vector con frecuencias de una tabla 2x2 (o11, o12, o21,o22)
#' @param alfa real en (0,1): Nivel de error de los intervalos (alternativa a conf)
#' @param conf real en (0,1): Nivel de confianza de los intervalos (alternativa a alfa)
#' @param decs entero: Numero de decimales en las salidas
#' @return vector con [1] estimacion clasica, [2] metodo mejorado,[3] L-IC, [4] U-IC
#' @examples
#' # formato de x <- c(o11, o12, o21, o22)
#' t2x2Rr(x=c(20,26,60,294))
#'

t2x2Rr<-function(x=NULL,decs=0, alfa=0.05, conf=0.95)
{

  epairsget(p=alfa,q=conf, pmin=0.0001,pdefault=0.05)->par_alfa
  if(par_alfa[[1]][1]){
    alfa<-par_alfa[[2]][1]
    conf<-par_alfa[[2]][2]
  } else {stop("valor de alfa o de conf incongruente")}

  zalfa   <- qnorm(1 - (alfa / 2))
  zalfa2<-zalfa^2

  ########################################################################
  h=0
  o11<-x[1]+h
  o12<-x[2]+h
  o21<-x[3]+h
  o22<-x[4]+h
  f1 <- o11+o12
  f2 <- o21+o22
  c1 <- o11+o21
  c2 <- o12+o22
  t  <- f1+f2
  r_estim<-NA
  if(o12*c1 >0 && o11*c2>0) {r_estim <- o11*c2/(o12*c1)}

  h=0.5
  o11<-x[1]+h
  o12<-x[2]+h
  o21<-x[3]+h
  o22<-x[4]+h
  f1 <- o11+o12
  f2 <- o21+o22
  c1 <- o11+o21
  c2 <- o12+o22
  t  <- f1+f2
  r_estimh <- o11*c2/(o12*c1)
  if(is.na(r_estim)) r_estim<-r_estimh


  radical <- (1/o11)+(1/o12)-(1/c1)-(1/c2)
  r_lnse  <- sqrt(radical)
  r_zlnsesup <- exp( zalfa * r_lnse )
  r_zlnseinf <- exp(-zalfa * r_lnse )
  r_lci<- r_estimh * r_zlnseinf
  r_uci<- r_estimh * r_zlnsesup

  res<-vector("list",length = 4)
  res[[1]]<-r_estim
  res[[2]]<-r_estimh
  res[[3]]<-r_lci
  res[[4]]<-r_uci
  res
}#end function
