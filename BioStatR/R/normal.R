#' @title Estudio de la normalidad de una variable
#' @description Representa el histograma de una variable x, superponiendo la densidad normal ajustada asi como un estimador del nucleo de la densidad. Permite obtener el diagrama QQ y el resultado del test de Shapiro-Wilk
#' @param x vector de datos a analizar
#' @param obs valor logico: TRUE=representar el histograma
#' @param mod valor logico: TRUE=representar el modelo ajustado de densidad normal
#' @param dens valor logico: TRUE=Superponer estimador de nucleo de la densidad
#' @param ks valor real: factor multiplicador del numero de intervalos calculados (el calculo se hace por la regla de Sturges o la de Freedman-Diaconis)
#' @param ky valor real: factor multiplicador de la escala del eje de ordenadas
#' @param qq valor logico: representa el diagrama QQ normal
#' @param sw valor logico: realiza el test de Shapiro-Wilk
#' @param decs valor entero: numero de decimales en la salida
#' @param ... parametros de configuracion de la funcion grpsggp
#' @importFrom graphics hist lines
#' @importFrom stats density dnorm qnorm qqline qqnorm qt quantile  pf pnorm pt qchisq qf qnorm shapiro.test
#' @return Histograma con densidad normal superpuesta, diagrama probabilistico normal, test de Shapiro-Wilk
#' @export testnormal
#' @examples
#' x=rnorm(500,10,2)
#' testnormal(x);
#' testnormal(x,ks=3);
#' testnormal(x,dens=FALSE)
#' testnormal(x,mod=FALSE,dens=FALSE,ks=3)
#' testnormal(x,obs=FALSE)
#' testnormal(x,col="white")
#' testnormal(x,sw=TRUE)
#' testnormal(x,sw=TRUE,qq=TRUE,ks=3)
#' testnormal(x,sw=TRUE,qq=TRUE,ks=3)


testnormal=function(x,obs=TRUE, mod=TRUE, dens=TRUE,ks=1,ky=1.2,qq=FALSE,sw=TRUE,decs=3, ...){

  # sintesis de la muestra
  x<-x[!is.na(x)]
  n<-length(x)
  m<-mean(x)
  stdev=sd(x)
  # Curva normal
  xn<-seq(min(x),max(x),length=500)
  yn<-dnorm(xn,m,stdev)
  # encuadre grafico
  maxy<-ky*max(yn)
  # numero de intervalos
  nbreaks.s<-(1+log2(n))
  nbreaks.fd<-2*IQR(x)/n^(1/3)
  nbreaks<-ks*max(nbreaks.s,nbreaks.fd)
  if (nbreaks<2) nbreaks=2
  # diagramas
  if (obs==TRUE)
  {hist(x, ylim=c(0,maxy),freq=FALSE,breaks=nbreaks,main=bquote(~ n == .(n)),xlab="Valores observados",ylab="Densidad",...)}
  else
  {hist(x, ylim=c(0,maxy),freq=FALSE,breaks=nbreaks,main=bquote(~ n == .(n)),xlab="Rango observado",ylab="Densidad",col="white",border="white",...)}
  if (mod) lines(xn,yn,col="red",lwd=2)
  if (dens) lines(density(x),col="blue",lty=2,lwd=2)

  if(qq){
     qqnorm(x,main=bquote(~ n == .(n)),xlab="Percentiles te\u00f3ricos",ylab="Percentiles observados" )
     qqline(x)
  }

  if(sw){
    if(n>3 && n<5000){
    sw<-shapiro.test(x)
    cat("\n")
    cat("# Test de normalidad de Shapiro-Wilk  \n ")
    cat("------------------------------------- \n ")
    tol=10^(-decs)
    if (sw[[2]]<tol) {ptxt=paste("p < ",tol, sep="")} else {ptxt=paste("p = ",round(sw[[2]],decs), sep="")}
    cat(paste("  n = ",n,",  W = ",round(sw[[1]],decs),",  ",ptxt,sep=""),"\n")
    ret<-c(sw[[1]],sw[[2]])}
    else {cat("\n");message("[!] El test de normalidad de Shapiro-Wilk requiere un tama\u00f1o muestral mayor a 3 y menor a 5000.  \n \n")}
    # cat("\n");cat("[Info] Instale el paquete nortest para m\u00e1s pruebas de normalidad \n")
  }

}
