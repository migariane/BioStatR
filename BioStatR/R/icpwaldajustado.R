#' @noRd
#' @title IC(p) - metodo de Wald ajustado
#' @description Obtencion del intervalo de confianza para una proporcion binomial a partir del metodo clasico de Wald ajustado (Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres)
#' @param x valor entero: numero de casos favorables (numerador de la proporcion binomial)
#' @param n valor entero: numero de casos posibles (denominador de la proporcion binomial)
#' @param conf valor real < 1: nivel de confianza (par?metro alternativo al error alfa, en tanto por uno). Por defecto =.95.
#' @param alfa valor real < 1: error alfa (parametro alternativo al nivel de confianza, en tanto por uno). Por defecto =.05.
#' @return Intervalo de confianza de Wald ajustado para estimar la proporcion binomial
#' @examples
#' icpwaldajustado(x=25, n=210)
#' icpwaldajustado(x=25, n=210, conf=.99)
#' icpwaldajustado(x=25, n=210, alfa=.01)
#'

icpwaldajustado<-function(x=0,n=0, conf=.95, alfa=.05)
{
  #if(conf!=.95){alfa<- 1-conf} else {if (alfa!=0.05){conf<- 1-alfa}}
  epairsget(p=alfa,q=conf, pmin=0.0001,pdefault=0.05)->par_alfa
  if(par_alfa[[1]][1]){
    alfa<-par_alfa[[2]][1]
    conf<-par_alfa[[2]][2]
  } else {stop("valor de alfa o de conf incongruente")}

  zalfa<- qnorm(1 - (alfa / 2))
  if(n>1 && n>x){
    res<-vector("list",length = 2)
    p<-(x+2)/(n+4)
    sep<-sqrt( (x+2) * (n-x+2) /(n+4))/(n+4)
    res[[1]]<-p-zalfa*sep
    if(res[[1]]<0){res[[1]]<-0}
    res[[2]]<-p+zalfa*sep
    if(res[[2]]>1){res[[2]]<-1}
    res
  }
  else {cat("\n ERROR: No valid data \n")}
}
