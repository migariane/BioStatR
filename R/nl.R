#' @title Tamano de muestra para estimar el parametro de la distribucion de Poisson
#' @description Estimacion del tamano de muestra para estimar el parametro de la distribucion de Poisson con la precision deseada
#' @param x valor entero: observacion o vector de observaciones de una v.a. con distribucion de Poisson o valor medio observado en una muestra de tamano n
#' @param n valor entero: tamano de la muestra piloto cuando x representa su media
#' @param d valor real entero: precision deseada para estimar el parametro de la distribucion de Poisson
#' @param lmax valor real: maximo valor del parametro de la Poisson cuando se cuenta con esta informacion
#' @param conf valor < 1: nivel de confianza (alternativo al error alfa, en tanto por uno). Por defecto =.95.
#' @param alfa valor < 1: error alfa (alternativo al nivel de confianza, en tanto por uno). Por defecto =.05.
#' @param decs valor entero: precision decimal para la salida de resultados.
#' @param eco valor logico: si eco=TRUE devuelve informe, de lo contrario valores estimados de n, sin y con cpc
#' @return Estimacion del tamano de muestra sin y con correccion por continuidad (cpc)
#' @export nl
#' @examples
#' # Introduciendo datos observados
#' # una observacion (muestra de tamano 1). Precision deseada de 1 unidad
#' nl(3, d=1)
#' # si se dispone del valor maximo del parametro (precision = 1 unidad)
#' nl(lmax=4.5, d=1)
#' # muestra con mas de una observacion
#' nl(c(3,6,3,1,2,5),d=1)
#' # Introduciendo media x de n datos observados
#' nl(x=25, n=210, d=2)
#'
#'

nl<-function(x=NA,n=0,d=0,lmax=0,conf=.95,alfa=.05, decs=3, eco=TRUE){
  lf <- 0
  uciex<-0 #Lim sup

  iserr<-FALSE
  #if(conf!=.95){alfa<-1-conf} else {if (alfa!=0.05){conf<-1-alfa}}
  epairsget(p=alfa,q=conf, pmin=0.0001,pdefault=0.05)->par_alfa
  if(par_alfa[[1]][1]){
    alfa<-par_alfa[[2]][1]
    conf<-par_alfa[[2]][2]
  } else {stop("valor de alfa o de conf incongruente")}



  mss<-0
  tipoentrada <- -1
  nocongruencia<-FALSE

  if(length(x)==1){
    if(is.na(x)){ tipoentrada <- 0}
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
      }


    }
  }
  else
  {
    if (length(x)>1) #se pasa vector de valores
     {lf<-length(x)
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

    }
  }
  if(d<=0){#no se indica la precision
      cat("ERROR - valor incorrecto de la precisi\u00f3n deseada d \n")
      iserr<-TRUE}

  if(iserr==FALSE){
    nmax<-0
    nex<-0

    if (tipoentrada>0){ # hay datos muestrales
       # lim superior del ic exacto
       uciex<-qchisq(alfa, 2*sumax+2,lower.tail =FALSE)/(2*n)
    }

    zalfa<- qnorm(1 -(alfa),lower.tail = TRUE)
    zalfa2<- qnorm(1 -(alfa/2),lower.tail = TRUE)
    #cat("zalfa = ",zalfa,"    zalfa2 =",zalfa2,"\n")

    if(lmax>0){
      n0max<-lmax*(zalfa2/d)^2
      nmax<-(n0max/4)*(1+sqrt(1+(2/(d*n0max))))^2
      n0max<-trunc(n0max+1,0)
      nmax <-trunc(nmax+1,0)
    }

    if(uciex>0){
      n0ex<-uciex*(zalfa2/d)^2
      nex <-(n0ex/4) * (1+sqrt(1+(2/(d*n0ex))))^2
      n0ex<-trunc(n0ex+1,0)
      nex <-trunc(nex+1,0)
    }
    if(eco){
    cat("\n")
    cat("Tama\u00f1o de muestra necesario para estimar el par\u00e1metro \u03bb\n")
    cat("de una VA con distribuci\u00f3n de Poisson con precisi\u00f3n \u03b4 \n")
    cat("----------------------------------------------------------------------\n")

    if(lmax>0){
      cat("Estimaci\u00f3n con el valor m\u00e1ximo propuesto para el par\u00e1metro: \n")
      cat("  Valor m\u00e1ximo propuesto: \u03bb = ",lmax," \n")
      cat("  Precisi\u00f3n deseada: \u03b4 = ",d," \n")
      cat("  Tamano muestral sin cpc: n \u2a7e ",n0max,  "\n")
      cat("  Tamano muestral con cpc: n \u2a7e ",nmax,  "\n")
      cat("\n")
    }

    if(tipoentrada>0){
          cat("Muestra piloto: \n")

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
    }

    if(uciex>0){
      cat("  Estimaci\u00f3n considerando la informaci\u00f3n muestral: \n")
      cat("    ",conf*100,"%-max(\u03bb) = ",round(uciex,digits = decs)," (m\u00e9todo exacto) \n")
      cat("     Precisi\u00f3n deseada: \u03b4 = ",d," \n")
      cat("     Tamano muestral sin cpc: n \u2a7e ",n0ex,  "\n")
      cat("     Tamano muestral con cpc: n \u2a7e ",nex,  "\n")
    }
    cat("\n")
    }


   else{
     res<-as.list(c(n0ex,nex))
     return(res)
   }


  } #end is error = false
  else # hay error
  {stop("\n ERROR: No valid data \n")}

}
