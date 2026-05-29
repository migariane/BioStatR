#' @noRd
#' @title Estimacion de la diferencia de Berkson (a partir de una tabla de contingencia 2x2)
#' @description Procedimiento para tablas 2x2. Calculo de la diferencia de Berkson con IC. (Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres)
#' @param x vector con frecuencias del interior de la tabla (o11, o12, o21,o22)
#' @param alfa real en (0,1): Nivel de error de los intervalos (alternativa a conf)
#' @param conf real en (0,1): Nivel de confianza de los intervalos (alternativa a alfa)
#' @param decs entero: Numero de decimales en las salidas
#' @return vector con [1] estimacion clasica, [2] m?todo mejorado,[3] L-IC, [4] U-IC
#' @examples
#' # formato de x <- c(o11, o12, o21, o22)
#' t2x2dberkson(x=c(20,26,60,294))

t2x2dberkson<-function(x=NULL,decs=0, alfa=0.05, conf=0.95)
{

  par_alfa<-epairsget(p=alfa,q=conf, pmin=0.0001,pdefault=0.05)

  if(par_alfa[[1]][1]){
    alfa<-par_alfa[[2]][1]
    conf<-par_alfa[[2]][2]
  } else {stop("valor de alfa o de conf incongruente")}

  zalfa <- qnorm(1 - (alfa / 2))
  zalfa2<-zalfa^2

  ########################################################################
  ifelse(x[1]>0 && x[2]>0 && x[3]>0 & x[4]>0, h<-0, h<-0.5)
  o11<-x[1]+h
  o12<-x[2]+h
  o21<-x[3]+h
  o22<-x[4]+h
  f1 <- o11+o12
  f2 <- o21+o22
  c1 <- o11+o21
  c2 <- o12+o22
  t  <- f1+f2
  db_estim<-NA
  if(o11*o22 >0 && o21*o12>0) {db_estim <- (o11*o22-o12*o21)/(c1*c2)}

  h=zalfa2/4
  o11<-x[1]+h
  o12<-x[2]+h
  o21<-x[3]+h
  o22<-x[4]+h
  f1 <- o11+o12
  f2 <- o21+o22
  c1 <- o11+o21
  c2 <- o12+o22
  t  <- f1+f2
  db_estimh <- (o11*o22-o12*o21)/(c1*c2)
  if(is.na(db_estim)) db_estim <- db_estimh

  radical <- (o11*o21/(c1^3))+(o12*o22/(c2^3))
  db_se   <- sqrt(radical)
  db_zse  <- zalfa * db_se

  db_lci<- db_estimh - db_zse
  db_uci<- db_estimh + db_zse

  res<-vector("list",length = 4)
  res[[1]]<-db_estim
  res[[2]]<-db_estimh
  res[[3]]<-db_lci
  res[[4]]<-db_uci
  res
}#end function
