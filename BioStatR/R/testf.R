#' @title Test de Fisher (F-test) para comparar dos varianzas
#' @description Permite obtener el intervalo de confianza a partir de una variable o bien de las medidas resumidas (Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres)
#' @param n1 valor entero: tamano muestra 1
#' @param n2 valor entero: tamano muestra 2
#' @param s1 valor real: desviacion tipica de la muestra 1
#' @param s2 valor real: desviacion tipica de la muestra 2
#' @return Fexp, gl1, gl2 y valor p del test de Fisher para el cociente de varianzas
#' @export testf
#' @examples
#' testf(s1=15, n1=120, s2=12, n2=65)

##F-test
###################
testf<-function(s1=0,n1=0,s2=0,n2=0)
{
  res<-vector("list",length = 4)
  if((n1<=1 || n2<=0 || s1<=0||s2<=0))
  {
    cat("\n ERROR: F-test  - no valid parameters \n")
  }

  else {
   if(s1>=s2) {
    v1<-s1^2
    gl1<-n1-1
    v2<-s2^2
    gl2<-n2-1}
   else {
    v1<-s2^2
    gl1<-n2-1
    v2<-s1^2
    gl2<-n1-1}
   fexp<-v1/v2
   pval<-2*(1-pf(fexp,gl1,gl2))
   res[[1]]<-fexp
   res[[2]]<-gl1
   res[[3]]<-gl2
   res[[4]]<-pval
   res
  }
 }
