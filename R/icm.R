#' @title Estimacion de la media de una variable aleatoria con distribucion normal
#' @description Permite obtener el intervalo de confianza a partir de una variable o bien de las medidas resumidas. Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres.
#' @param x vector de datos cuya media se va a estimar
#' @param n valor entero: tamano muestral cuando se indican los datos resumidos
#' @param m valor real: media de la variable (previamente calculada)
#' @param s valor real: desviacion tipica de la variable  (previamente calculada)
#' @param conf valor < 1: nivel de confianza (parametro alternativo al error alfa, en tanto por uno). Por defecto =.95.
#' @param alfa valor < 1: error alfa (parametro alternativo al nivel de confianza, en tanto por uno). Por defecto =.05.
#' @param decs valor entero: precision decimal para la salida de resultados. Por defecto = 4.
#' @param vac valor logico: TRUE=se trata de una variable aleatoria continua; FALSE= la variable es discreta y se aplica cpc. Por defecto = TRUE.
#' @param d valor real < 1: precision deseada para el intervalo de confianza. Si d>0 se invoca a la funcion nm() para estimar el tamano de muestra
#' @param eco valor logico: si eco=TRUE la funcion genera un informe (no devuelve valores), si eco=FALSE devuelve los limites del IC y su precision
#' @return Informe (si eco=T) con el intervalo de confianza para estimar la media poblacional de una variable aleatoria normal. Limites inferior y superior del IC y su precision.
#' @export icm
#' @examples
#' icm(x=c(25.4, 14.6, 23.1, 26.0, 14.4, 24.3, 36.1, 21.0, NA, 41.9))
#' icm(x=c(25,14,23,26,14,24,36,21,NA,41), vac=FALSE)
#' icm(n=100, m=25.3, s=4.1)
#' icm(n=100, m=25.3, s=4.1, conf=0.99)
#' icm(n=100, m=25.3, s=4.1, alfa=0.01)
#'
#' icm(n=100, m=25.3, s=4.1, alfa=.01, eco=FALSE)->IC
#' IC
#'
#IC para la media
###################
icm<-function(x=0,n=0,m=0,s=0,conf=.95,alfa=.05, decs=3, d=0, vac=TRUE, eco=TRUE){
  lf <- 0
  li=NA #Lim inf
  ls=NA #Lim sup
  dd=NA #precision
  if (length(x)>1)
    {  lf<-length(x)
       x<-x[!is.na(x)]
       m<-mean(x)
       s<-sd(x)
       n<-length(x)}
  else
  { if(x!=0 && m==0) {m<-x}  }

  if (n>1 && s>0)
  {

    # if(conf!=.95){alfa<-1-conf} else {if (alfa!=0.05){conf<-1-alfa}}
    epairsget(p=alfa,q=conf, pmin=0.0001,pdefault=0.05)->par_alfa
    if(par_alfa[[1]][1]){
      alfa<-par_alfa[[2]][1]
      conf<-par_alfa[[2]][2]
    } else {stop("valor de alfa o de conf incongruente")}

    talfa<- qt(1 -(alfa / 2), n - 1)
    se<-s/sqrt(n)
    dd<-talfa*se
    if (vac==FALSE) {dd<-dd+(1/(2*n))}
    li<-m-dd
    ls<-m+dd
    if (eco){
    cat("\n")
    cat("Intervalo de confianza bilateral para la media de una VA normal \n")
    cat("----------------------------------------------------------------\n")
    cat("Informaci\u00f3n muestral: \n")
    if(lf!=0 && lf!=n){
        #valores faltantes
        mss=lf-n
        cat("  Declarados ",lf,"casos. ")
        if (mss==1) {cat("Hay",mss,"valor faltante \n")} else
                    {cat("Hay",mss,"valores faltantes \n")}

    } else {
      if(length(x)>1) {cat("  No hay valores faltantes \n")}
    }
    cat("  Tama\u00f1o muestral: n = ",n,"\n")
    cat("  Media: m = ",roundf(m,digits = decs),"\n")
    cat("  Desviaci\u00f3n t\u00edpica: s = ",roundf(s,digits = decs),"\n")
    cat("  Error est\u00e1ndar de la media: sem = ",roundf(se,digits = decs),"\n")
    cat("\n")
    cat("Estimaci\u00f3n: \n")
    if (vac==FALSE) {
      cpc=1/(2*n)
      tol=1/(10^decs)
      if (cpc>tol) {scpc=round(cpc,digits=decs)} else {scpc=paste("<",tol)}
    cat("  Se aplica cpc = ","\u00b1","1/(2n) = ",scpc," para variable discreta \n",sep="")
      }
    cat(" ",conf*100,"%-IC(\u00b5):  (",round(li,digits = decs),", ",round(ls,digits = decs),")",sep="","\n")
    cat("  Precisi\u00f3n obtenida:" ,round(dd,digits = decs),"\n")
    cat("\n")

    if(d>0){
      nm(n=n,m=m,s=s,d=d)

     }
    }
    res<-as.list(c(li,ls,dd))
  }
  else {cat("\n ERROR: No valid data \n")}
}
