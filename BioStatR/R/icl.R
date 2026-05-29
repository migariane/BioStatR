#' @title Estimacion del parametro de la distribucion de Poisson
#' @description Obtencion del intervalo de confianza para el parametro de la distribucion de Poisson por los metodos exacto y aproximando a la normal (transformacion de la raiz).
#' @param x valor entero: vector de observaciones de una v.a. con distribucion de Poisson o valor medio observado en una muestra de tamano n.
#' @param n valor entero: tamano de la muestra piloto cuando x representa su media.
#' @param conf valor < 1: nivel de confianza (alternativo al error alfa, en tanto por uno). Por defecto =.95.
#' @param alfa valor < 1: error alfa (alternativo al nivel de confianza, en tanto por uno). Por defecto =.05.
#' @param d valor real < 1: precision deseada para el intervalo de confianza. Si d>0 se invoca a la funcion nl()
#' @param decs valor entero: precision decimal para la salida de resultados. Por defecto = 4.
#' @param eco valor logico: si eco=TRUE la funcion genera un informe (no devuelve valores), si eco=FALSE devuelve el IC exacto y el basado en la aprox normal con su precision respectiva
#' @return Informe con intervalos de confianza exacto y aproximado a la normal (con cpc) (eco=TRUE) o los limites y precision correspondientes a cada intervalo
#' @export icl
#' @examples
#' # Introduciendo datos observados
#' # Una sola observacion (muestra de tamano n=1)
#' icl(3)
#' # muestra con mas de una observacion
#' icl(c(3,6,3,1,2,5))
#' # Introduciendo media x de n datos observados
#' icl(x=25, n=210)
#'
#' icl(x=25, n=210, eco=FALSE)->IC
#' IC
#'
#' # solicitud del tamano de muestra necesario para estimar con una precision d=1 unidad
#' icl(x=25, n=210, d=1)
#'

icl<-function(x=0,n=0,conf=.95,alfa=1-conf, decs=4, d=0, eco=TRUE){
  lf <- 0

  lciex=NA #Lim inf
  uciex=NA #Lim sup
  ddex=NA #precision

  lcinorm=NA #Lim inf
  ucinorm=NA #Lim sup
  ddnorm=NA #precision

  #if(conf!=.95){alfa<-1-conf} else {if (alfa!=0.05){conf<-1-alfa}}
  epairsget(p=alfa,q=conf, pmin=0.0001,pdefault=0.05)->par_alfa
  if(par_alfa[[1]][1]){
    alfa<-par_alfa[[2]][1]
    conf<-par_alfa[[2]][2]
  } else {stop("valor de alfa o de conf incongruente")}



  mss<-0
  iserr<-FALSE
  tipoentrada <- 0
  nocongruencia<-FALSE

  if (length(x)>1) #se pasa vector de valores
  {  lf<-length(x)
     x<-x[!is.na(x)]
     lx<-length(x)
     if((n>0)&&(n!=lx)){nocongruencia<-TRUE}
     n<-lx
     mss<-(lf-n)
     if(n>0){sumax<-sum(x)
             media<-mean(x)
     }
     tipoentrada<-1 #vector
  }

  else{     #length(x)==1 y hay que evaluar n
    if((n<0) || (x<0)){
      cat("ERROR - Valores incorrectos en los par\u00e1metros \n")
      iserr<-TRUE
    }
    else{
    if(n==0){ #muestra de tamaño 1
       n<-1
       media<-x
       sumax<-x
       tipoentrada<-2 #muestra con n=1
     }
    else { #x es la media de una muestra de tamaño n
      tipoentrada<-3 # se indican media y n
      media<-x
      sumax<-n*x
     }}
  }

  if(iserr==FALSE){
    #ic exacto
    if(sumax==0){lciex<-0
                 uciex<-qchisq(alfa, 2*sumax+2,lower.tail =FALSE)/(2*n)
                 ddex=(uciex-lciex)/2 # semiamplitud
    }
    else{
      lciex<-qchisq(1-(alfa/2), 2*sumax,lower.tail =FALSE)/(2*n)
      uciex<-qchisq(alfa/2, 2*sumax+2,  lower.tail =FALSE)/(2*n)}
      ddex=(uciex-lciex)/2 # semiamplitud

    #ic aprox
   if (n>=1)
   {
    #IC aprox. normal
    zalfa<- qnorm(1 -(alfa / 2),lower.tail = TRUE)
    #cat("zalfa=",zalfa,"\n")
    lcinorm<- (((zalfa/2) - sqrt( sumax   )  )^2)/n
    ucinorm<- (((zalfa/2) + sqrt( sumax +1)  )^2)/n
    ddnorm<-(ucinorm-lcinorm)/2
   }

  if(eco){

  cat("\n")
  cat("Intervalo de confianza bilateral para el par\u00e1metro  \u03bb de una VA con distribuci\u00f3n de Poisson \n")
  cat("----------------------------------------------------------------------------------------------\n")
  cat("Informaci\u00f3n muestral: \n")

  # entrada como vector (length(x)>0)
  if(tipoentrada==1){  #entra vector de datos
    if(nocongruencia==TRUE){
      cat("  Incongruencia: Se ignora el valor del par\u00e1metro n, asumiendo como tal la longitud del vector x \n")
    }
      # valores faltantes
      if (mss>0){
          cat("  Declarados ",lf,"casos. ")
          if (mss==1) {cat("   Hay",mss,"valor faltante \n")} else  {if (mss>1) {cat("   Hay",mss,"valores faltantes \n") }}
      }
      else {cat("  No hay valores faltantes \n")}
  }
  else {
    if (tipoentrada==2){
      cat("  Muestra de una sola observaci\u00f3n  \n")
    }
    else { #tipo entrada=3
      cat("  Se indica una \u00fanica observaci\u00f3n muestral",n," \n")
    }
  }

    cat("  Tama\u00f1o muestral: n = ",n,"\n")
    cat("  Media observada: m = ",round(media,digits = decs),"\n")
    cat("\n")
    cat("Estimaci\u00f3n: \n")
    cat("  [1] M\u00e9todo exacto: \n")
    cat("     ",conf*100,"%-IC(\u03bb):  (",round(lciex,digits = decs),", ",round(uciex,digits = decs),")\n")
    cat("      Semiamplitud del intervalo:" ,round(ddex,digits = decs),"\n")
    cat("\n")


    cat("  [2] Aproximaci\u00f3n a la normal (transformaci\u00f3n de la raiz): \n")
    if(sumax>=15){
      cat("      Validez de la aproximaci\u00f3n: \u03A3x = ",sumax," \u2a7e 15 (v\u00e1lida) \n")}
    else {
      cat("      Validez de la aproximaci\u00f3n: \u03A3x = ",sumax," < 15 --- NO es v\u00e1lida --- \n")}

    cat("     ",conf*100,"%-IC(\u03bb):  (",round(lcinorm,digits = decs),", ",round(ucinorm,digits = decs),")\n")
    cat("      Precisi\u00f3n obtenida:" ,round(ddnorm,digits = decs),"\n")
    cat("\n")

    if(d>0){
       if (mss>0) {cat("> Se invoca a la estimaci\u00f3n del tama\u00f1o muestral suprimiendo los valores faltantes de la muestra piloto \n")}
       nl(x=x,n=n,alfa=alfa,d=d)
    }
   }
   else{
    res<-as.list(c(lciex,uciex,ddex,lcinorm,ucinorm,ddnorm))
    return(res)
   }

      } #end is error = false
  else # hay error
    {cat("\n ERROR: No valid data \n")}

}

