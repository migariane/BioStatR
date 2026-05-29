#' @title Tamano de muestra para estimar la media de una variable aleatoria con distribucion normal con una precision determinada
#' @description Permite estimar el tamano muestral para estimar una media con una precision deseada a partir de la informacion dada por una muestra piloto, que puede aparecer como variable o bien dando sus medidas resumidas. Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres.
#' @param x vector de datos de la muestra piloto
#' @param n valor entero: tamano muestral de la muestra piloto cuando se indican sus parametros muestrales resumidos
#' @param m valor real: media de la de la muestra piloto (previamente calculada)
#' @param s valor real: desviacion tipica de la muestra piloto  (previamente calculada)
#' @param d valor real: precision deseada para el intervalo de confianza
#' @param conf valor real < 1: nivel de confianza (parametro alternativo al error alfa, en tanto por uno). Por defecto =.95.
#' @param alfa valor real < 1: error alfa (parametro alternativo al nivel de confianza, en tanto por uno). Por defecto =.05.
#' @param decs valor entero: precision decimal para la salida de resultados. Por defecto = 4.
#' @param vac valor logico: TRUE=se trata de una variable aleatoria continua; FALSE= la variable es discreta y se aplica cpc. Por defecto = TRUE.
#' @param eco valor logico: si eco=TRUE la funcion genera un informe (no devuelve valores), si eco=FALSE devuelve el tama?o de muestra estimado
#' @return Informe (si eco=T) con el tamano de muestra estimado para obtener un intervalo de confianza para estimar una media con la precision deseada
#' @export nm
#' @examples
#' nm(x=c(25.4, 14.6, 23.1, 26.0, 14.4, 24.3, 36.1, 21.0, NA, 41.9,NA), d=1.5)
#' nm(d=1.0, n=100, m=25.3, s=4.1)
#' nm(d=1.0, n=100, m=25.3, s=4.1, conf=.99)
#' nm(d=1.0, n=100, m=25.3, s=4.1, alfa=.01)
#'
# tamano de muestra para la media
#####################################
nm<-function(x=0,n=0,m=0,s=0,d=0,conf=.95,alfa=.05, decs=4, vac=TRUE,eco=TRUE){
  tab<-"  "
  lf<-0
  if (length(x)>1)
  { lf<-length(x)
  x<-x[!is.na(x)]
  m<-mean(x)
  s<-sd(x)
  n<-length(x)}
  else {if(x!=0 && m==0) {m<-x}}

  if (n>1 && s>0 && d>0 ) {
    #if(conf!=.95){alfa<-1-conf} else {if (alfa!=0.05){conf<-1-alfa}}
    epairsget(p=alfa,q=conf, pmin=0.0001,pdefault=0.05)->par_alfa
    if(par_alfa[[1]][1]){
      alfa<-par_alfa[[2]][1]
      conf<-par_alfa[[2]][2]
    } else {stop("valor de alfa o de conf incongruente")}


    talfa<- qt(1 -(alfa / 2), n - 1)
    se<-s/sqrt(n)
    if (vac){cpc<-0} else {cpc<-1/(2*n)}
    do<-talfa*se + cpc
    ne<-((talfa*s)/d)^2
    ne<-trunc(ne+1,0)
    if(eco){
    cat("\n")
    showtitle("Tama\u00f1o de muestra para la estimaci\u00f3n de la media de una VA normal o su aproximaci\u00f3n",1)
    showtitle("Muestra piloto:",2)
    mss<-0

    if(lf!=0 && lf!=n){
      #valores faltantes
      mss=lf-n
      cat(paste(tab,"Declarados ",lf," casos, ",sep=""))
      if (mss==1) {cat("hay",mss,"valor faltante \n")} else
      {cat("hay",mss,"valores faltantes \n")}

    }
    if(mss>0) cat(paste(tab,"Tama\u00f1o muestral v\u00e1lido:  n = ",n,sep=""),"\n")
    else cat(paste(tab,"Tama\u00f1o muestral:  n = ",n,sep=""),"\n")
    cat(paste(tab,"Media: m = ",roundf(m,decs),sep=""),"\n")
    cat(paste(tab,"Desviaci\u00f3n t\u00edpica: s = ",roundf(s,decs),sep=""),"\n")
    cat(paste(tab,"Error estandar de la media: sem = ",roundf(se,decs),sep=""),"\n")
    cat(paste(tab,"Precisi\u00f3n observada: d = " ,roundf(do,decs),sep=""),"\n")

    showtitle("Estimaci\u00f3n del tama\u00f1o muestral:",2)
    if(vac==FALSE){
        tol=1/(10^decs)
        if (cpc>tol) {scpc=round(cpc,decs)} else {scpc=paste("<",tol)}
        cat(paste(tab,"Se aplica cpc = ","\u00b1","1/(2n) = ",scpc," para variable discreta ",sep=""),"\n")
    }
    cat(paste(tab,"Precisi\u00f3n deseada: \u03b4 = ",roundf(d,decs),sep=""),"\n")
        cat(paste(tab,"Tama\u00f1o muestral necesario: n \u2265 ",ne,sep=""),"\n")
    if(ne<n){
      cat(paste(tab,"La muestra actual es suficiente para obtener una precisi\u00f3n de",d," unidades",sep=""), "\n")}
    cat("\n")
    }
    else
    { res<-ne
      return(res)
    }
  }
  else {stop("ERROR: No valid data \n")}
}
