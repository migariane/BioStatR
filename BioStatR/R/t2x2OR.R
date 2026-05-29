#' @noRd
#' @title Estimacion de la odds ratio (a partir de una tabla de contingencia 2x2)
#' @description Procedimiento para tablas de contingencia 2x2. Calculo de la odds ratio con IC - Metodo clasico. (Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres)
#' @param x vector con frecuencias de una tabla 2x2 (o11, o12, o21,o22)
#' @param alfa real en (0,1): Nivel de error de los intervalos (alternativa a conf)
#' @param conf real en (0,1): Nivel de confianza de los intervalos (alternativa a alfa)
#' @param decs entero: Numero de decimales en las salidas
#' @return vector con [1] estimacion clasica, [2] metodo mejorado,[3] L-IC, [4] U-IC
#' @examples
#' # formato de x x<- c(o11, o12, o21, o22)
#' t2x2or(x=c(20,26,60,294))
#'

t2x2or<-function(x=NULL,decs=0, alfa=0.05, conf=0.95)
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
  or_estim<-NA
  if(o11*o22 >0 && o21*o12>0) {or_estim <- o11*o22/(o12*o21)}

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
  or_estimh <- o11*o22/(o12*o21)
  if(is.na(or_estim)) or_estim <- or_estimh

  radical <- (1/o11)+(1/o12)+(1/o21)+(1/o22)
  or_lnse  <- sqrt(radical)
  or_zlnsesup <- exp( zalfa * or_lnse )
  or_zlnseinf <- exp(-zalfa * or_lnse )
  or_lci<- or_estimh * or_zlnseinf
  or_uci<- or_estimh * or_zlnsesup

  res<-vector("list",length = 4)
  res[[1]]<-or_estim
  res[[2]]<-or_estimh
  res[[3]]<-or_lci
  res[[4]]<-or_uci
  return(res)
}#end function
