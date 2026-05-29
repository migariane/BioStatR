#' @noRd
#' @title IC(p) - metodo de Wilson
#' @description Obtencion del intervalo de confianza para una proporcion binomial a partir del metodo de Wilson con correccion por continuidad (Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres)
#' @param x valor entero: numero de casos favorables (numerador de la proporcion binomial)
#' @param n valor entero: numero de casos posibles (denominador de la proporcion binomial)
#' @param conf valor real entre 0 y 1: nivel de confianza (parametro alternativo al error alfa, en tanto por uno). Por defecto =.95.
#' @param alfa valor real entre 0 y 1: error alfa (parametro alternativo al nivel de confianza, en tanto por uno). Por defecto =.05.
#' @return Intervalo de confianza de Wilson con cpc para estimar la proporcion binomial
#' @examples
#' icpwilson(x=25, n=210)
#' icpwilson(x=25, n=210, conf=.99)
#' icpwilson(x=25, n=210, alfa=.01)
#'

icpwilson<-function(x=0,n=0, conf=.95, alfa=.05)
{
  #if(conf!=.95){alfa<- 1-conf} else {if (alfa!=0.05){conf<- 1-alfa}}
  epairsget(p=alfa,q=conf, pmin=0.0001,pdefault=0.05)->par_alfa
  if(par_alfa[[1]][1]){
    alfa<-par_alfa[[2]][1]
    conf<-par_alfa[[2]][2]
  } else {stop("valor de alfa o de conf incongruente")}

  zalfa<- qnorm(1 - (alfa / 2))
  if(n>1 && n>x)
  {
    res<-vector("list",length = 2)
    xa<-x-0.5
    xb<-x+0.5
    z2<-zalfa^2
    res[[1]]<-(xa+(z2/2)-zalfa*sqrt((z2/4)+xa*(1-(xa/n))))/(n+z2)
    if(res[[1]]<0){res[[1]]<-0}
    res[[2]]<-(xb+(z2/2)+zalfa*sqrt((z2/4)+xb*(1-(xb/n))))/(n+z2)
    if(res[[2]]>1){res[[2]]<-1}
    res
  }
  else {cat("\n ERROR: No valid data \n")}
}
