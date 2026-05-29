#' @title Estimacion de una proporcion binomial por los metodos: Wilson (con cpc), Wald (con cpc) y Wald ajustado
#' @description Obtencion del intervalo de confianza para una proporcion binomial considerando los metodos de Wilson (con cpc), Wald (con cpc) y Wald ajustado. Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres
#' @param x valor entero: numerador de la proporcion binomial (si se da la informacion resumida) o valor a seleccionar del vector v (si se ha dado un vector de datos)
#' @param n valor entero: denominador de la proporcion binomial (si se da la informacion resumida)
#' @param level entero o texto: es el valor o la etiqueta del nivel de seleccion de x cuando este ultimo es un vector
#' @param conf valor < 1: nivel de confianza (alternativo al error alfa, en tanto por uno). Por defecto =.95.
#' @param alfa valor < 1: error alfa (alternativo al nivel de confianza, en tanto por uno). Por defecto =.05.
#' @param d valor real < 1: precision deseada para el intervalo de confianza. Si d>0 se invoca a la funcion np()
#' @param decs valor entero: precision decimal para la salida de resultados. Por defecto = 4.
#' @param tabla valor logico: si tabla=TRUE el informe tiene forma de tabla. En cualquier caso devuelve los limites del IC y su precision
#' @return Informe con los intervalos de confianza de Wilson, Wald (ambos con cpc) y Wald ajustado. Limites de cada intervalo y su precision en forma de tabla.
#' @export icp
#' @examples
#' # Introduciendo frecuencias
#' icp(x=25, n=210)
#' icp(x=25, n=210, conf=0.90, decs=8)
#'
#' # Introduciendo datos
#' datos<-c(1,1,1,2,2,2,2,2,1,1,1,2,1,2,2,2,2,2,1,1,1,2,1,2,1,2,1,2,2,2,2,2,1,1)
#'   icp(x=datos, level=1)
#'   icp(x=datos, level=1, conf=.99)
#' sexo<-as.factor(c("H","H","M","M","H","M","M","H","H","M","H","M","H"))
#'   icp(x=sexo, level="M")
#'
#' # Cambiar la salida por una tabla resumen (manejable como data.frame)
#' icp(x=sexo, level="M",tabla=TRUE)
#'
#' # Invocacion a la estimacion del tamano muestral
#' icp(x=sexo, level="M", d=0.1)
#'

icp<-function(x=0,n=0,level="",conf=.95, alfa=.05,decs=4,d=0,tabla=FALSE)
{
  tab<-"  "
  # if(conf!=.95){alfa<- 1-conf} else {if (alfa!=0.05){conf<- 1-alfa}}
  epairsget(p=alfa,q=conf, pmin=0.0001,pdefault=0.05)->par_alfa
  if(par_alfa[[1]][1]){
    alfa<-par_alfa[[2]][1]
    conf<-par_alfa[[2]][2]
  } else {stop("valor de alfa o de conf incongruente")}


  zalfa<- qnorm(1 - (alfa / 2))
  v<-x

  txtlevel=""
  lf <- 0
  if (length(v)>1) #si se pasa una variable en v, se obtiene n de ella y x o level indica la clase de interes
  { lf<-length(v)
    v<-v[!is.na(v)]
    n <-length(v)

    if (level!=""){
       n0<-length(v[v==level])
       txtlevel <- paste("(nivel =",level,")",sep="")
    }
    else
      { if(x!=0){
        n0<-length(v[v==x])
        txtlevel <- paste("(nivel = ",x,")",sep="")
      }

    }

  }
  else
  {n0<-x}

  #exacto
    exacto<-vector("list",length = 2)
    exacto<-icpexact(n0,n,conf)
    exactest<-(exacto[[1]]+exacto[[2]])/2
    exactprec<-(exacto[[2]]-exacto[[1]])/2

  if(n>1 && n>n0){
    # wilson
    wilson<-vector("list",length = 2)
    if ((n0>5) && (n-n0>5)) {
      fwilson<-T
      wilson <-icpwilson(x=n0,n,conf,alfa)
      wilsonest<-(wilson[[1]]+wilson[[2]])/2
      wilsonprec<-((wilson[[2]]-wilson[[1]])/2)
    }
    else
    {
      fwilson<-F
      wilson<-list(NA,NA)
      wilsonprec<-NA
      wilsonest<-NA

    }

    # wald
    wald<-vector("list",length = 2)
    if ((n0>20) && (n-n0>20)) {
      fwald<-T
      waldest<-n0/n
      wald <-icpwald(x=n0,n=n,conf=conf, alfa=alfa)
      waldprec<-((wald[[2]]-wald[[1]])/2)
    }
    else {
      fwald<-F
      waldest<-n0/n
      wald<-list(NA,NA)
      waldprec<-NA
    }

    # wald ajustado
    waldaj<-vector("list",length = 2)
    waldajest<-(n0+2)/(n+4)
    waldaj <-icpwaldajustado(x=n0,n=n,conf=conf, alfa=alfa)
    waldajprec<-((waldaj[[2]]-waldaj[[1]])/2)

   if(tabla==FALSE){
    cat("\n")
    cat("Intervalo de confianza para una proporci\u00f3n binomial \n")
    cat("--------------------------------------------------- \n")
    cat("\n")
    cat("Informaci\u00f3n muestral: \n")
    cat(tab,"Tama\u00f1o de muestra: n = ",n, "\n",sep="")
    if(lf>n){cat(tab,"Valores faltantes:",lf-n,"\n",sep="")}
    txtcasos<-paste("Casos observados: ",txtlevel,"x = ",sep="")
    cat(tab,"Estimaci\u00f3n puntual cl\u00e1sica: p=x/n = ",round(n0/n,decs),", q=(1-p)=",round((n-n0)/n,decs),"\n",sep="")
    cat(tab,txtcasos,n0,"\n",sep="")

    showtitle("M\u00e9todo exacto (Clooper-Pearson):",lev=2)
      cat(tab,"Pseudo-estimaci\u00f3n puntual: p' = ",round(exactest,decs),", q'=(1-p')=",round(1-exactest,decs),"\n",sep="")
      cat(tab,conf*100,"%-IC(\u03c0): (",round(exacto[[1]],decs),", ",round(exacto[[2]],decs),") \n",sep="")
      cat(tab,"Semiamplitud: ",round(exactprec,decs),"\n",sep="")

    showtitle("M\u00e9todo de Wilson (con cpc):",lev=2)
    if(fwilson){
      cat(tab,"Pseudo-estimaci\u00f3n puntual: p' = ",round(wilsonest,decs),", q'=(1-p')=",round(1-wilsonest,decs),"\n",sep="")
      cat(tab,conf*100,"%-IC(\u03c0): (",round(wilson[[1]],decs),", ",round(wilson[[2]],decs),") \n",sep="")
      cat(tab,"Semiamplitud: ",round(wilsonprec,decs),"\n",sep="")
    }
    else
    {cat(tab,"No aplicable: x=",n0,if(n0<5){"<5"},", n-x=",n-n0,if(n-n0<5){"<5"},"\n",sep="")}

    showtitle("M\u00e9todo de Wald (con cpc):",lev=2)
    if(fwald){
      cat(tab,"Estimaci\u00f3n puntual (cl\u00e1sica): p=x/n = ",round(waldest,decs),", q=(1-p)=",round(1-waldest,decs),"\n",sep="")
      cat(tab,conf*100,"%-IC(\u03c0): (",round(wald[[1]],decs),", ",round(wald[[2]],decs),") \n",sep="")
      cat(tab,"Precisi\u00f3n: ",round(waldprec,decs),"\n",sep="")
    }
    else
    {cat(tab,"No aplicable: x=",n0,if(n0<20){"<20"},", n-x=",n-n0,if(n-n0<20){"<20"},"\n")}


    showtitle("M\u00e9todo de Wald ajustado (Agresti-Coull):",lev=2)
    cat(tab,"Estimaci\u00f3n puntual: p=(x+2)/(n+4) = ",round(waldajest,decs),", q=(1-p)=",round(1-waldajest,decs),"\n",sep="")
    cat(tab,conf*100,"%-IC(\u03c0): (",round(waldaj[[1]],decs),", ",round(waldaj[[2]],decs),") \n",sep="")
    cat(tab,"Precisi\u00f3n: ",round(waldajprec,decs),"\n",sep="")
   }
    # cnf<-paste("(",round(conf*100,0),"%)",sep="")
    metodo<-c("Clooper-Pearson",
              "Wilson",
              "Wald",
              "Agresti-Coull")

    puntual<-round(c(exactest,wilsonest,waldest,waldajest),decs)
    icinf<-round(c(exacto[[1]],wilson[[1]],wald[[1]],waldaj[[1]]),decs)
    icsup<-round(c(exacto[[2]],wilson[[2]],wald[[2]],waldaj[[2]]),decs)
    precision<-round(c(exactprec,wilsonprec,waldprec,waldajprec),decs)
    result<-data.frame(puntual,icinf,icsup,precision)
    row.names(result)<-paste(tab,metodo,sep="")
    cat("\n")
    #return(result)}

    if(d>0){
      np(x=n0,n=n,d=d)
    }
    res<-result

  }
  else {cat("\n ERROR: No valid data \n")}
  if(tabla) {
    showtitle("Intervalos de confianza para una proporci\u00f3n binomial ",lev=1)
    cat(paste(tab,"M\u00e9todo (conf.=",conf*100,"%)",sep=""),"\n")
    return(result)}
}


