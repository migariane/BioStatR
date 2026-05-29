
#' @noRd
#' @title Estimacion del riesgo atribuible (a partir de una tabla de contingencia 2x2)
#' @description Procedimiento para tablas de contingencia 2x2. Calculo del riesgo atribuible con IC. (Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres)
#' @param x vector con frecuencias de una tabla 2x2 (o11, o12, o21,o22)
#' @param alfa real en (0,1): Nivel de error de los intervalos (alternativa a conf)
#' @param conf real en (0,1): Nivel de confianza de los intervalos (alternativa a alfa)
#' @param decs entero: Numero de decimales en las salidas
#' @param retro valor logico: indicador de estudio retrospectivo
#' @return vector con [1] estimacion clasica, [2] metodo mejorado,[3] L-IC, [4] U-IC
#' @examples
#' # formato de x <- c(o11, o12, o21, o22)
#' t2x2Ra(x=c(20,26,60,294))
#'

t2x2Ra<-function(x=NULL, retro=FALSE,alfa=0.05, conf=0.95,decs=0)
{

  epairsget(p=alfa,q=conf, pmin=0.0001,pdefault=0.05)->par_alfa
  if(par_alfa[[1]][1]){
    alfa<-par_alfa[[2]][1]
    conf<-par_alfa[[2]][2]
  } else {stop("valor de alfa o de conf incongruente")}
  zalfa   <- qnorm(1 - (alfa / 2))
  zalfa2<-zalfa^2

  ########################################################################
  ifelse((x[1]==0||x[2]==0||x[3]==0||x[4]==0),h<-0.5,h<-0)
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
  res<-vector("list",length = 4)
  res[[1]]<-NA
  res[[2]]<-NA
  res[[3]]<-NA
  res[[4]]<-NA

  if (retro)
  {#hay que permutar filas por columnas
    o11<-x[1]+h
    o12<-x[3]+h
    o21<-x[2]+h
    o22<-x[4]+h
    f1 <- o11+o12
    f2 <- o21+o22
    c1 <- o11+o21
    c2 <- o12+o22
    t  <- f1+f2

   r_estim <- (o11*o22-o12*o21)/(f1*o22)
   r_estimh<- r_estim

   radical <- (1/o12)+(1/o22)-(1/f1)-(1/f2)
   r_lnse  <- sqrt(radical)

   r_zlnsesup <- exp( zalfa * r_lnse )
   r_zlnseinf <- exp(-zalfa * r_lnse )

   a<- 1- ((1-r_estim) * r_zlnseinf)
   b<- 1- ((1-r_estim) * r_zlnsesup)

   r_lci<-min(a,b)
   r_uci<-max(a,b)

   res[[1]]<-r_estim
   res[[2]]<-r_estimh
   res[[3]]<-r_lci
   res[[4]]<-r_uci
  }
  else
  {r_estim <- ((o11*o22)-(o12*o21))/(f1*c2)
   r_estimh<-r_estim

   radical <- (o21 + r_estim *(o11+o22))/(t*o12)
   r_lnse  <- sqrt(radical)

   r_zlnsesup <- exp( zalfa * r_lnse )
   r_zlnseinf <- exp(-zalfa * r_lnse )

   a<- 1-(1-r_estim) * r_zlnseinf
   b<- 1-(1-r_estim) * r_zlnsesup

   r_lci<-min(a,b)
   r_uci<-max(a,b)


   res[[1]]<-r_estim
   res[[2]]<-r_estimh
   res[[3]]<-r_lci
   res[[4]]<-r_uci
   }

  res
}#end function


