#' @title Tamano de muestra para estimar una proporcion binomial
#' @description Obtencion del tamano muestral para estimar una proporcion binomial con la precision deseada a partir de informacion piloto (metodo de Wald ajustado) o sin ella. Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres.
#' @param v vector: numero de casos favorables (numerador de la proporcion binomial)
#' @param x valor entero: numerador de la proporcion binomial (si se da la informacion resumida) o valor de seleccion (si se da un vector)
#' @param n valor entero: denominador de la proporcion binomial (si se da la informacion resumida)
#' @param d valor real < 1: precision deseada para el intervalo de confianza
#' @param level texto: si se indica v como factor, level es la etiqueta del nivel de seleccisn
#' @param conf valor < 1: nivel de confianza (parametro alternativo al error alfa, en tanto por uno). Por defecto =.95.
#' @param alfa valor < 1: error alfa (parametro alternativo al nivel de confianza, en tanto por uno). Por defecto =.05.
#' @param decs valor entero: precision decimal para la salida de resultados. Por defecto = 4.
#' @param eco valor logico: si eco=TRUE devuelve informe, de lo contrario, los valores estimados de n con y sin informacion
#' @return Informe con el tamano de muestra necesario para estimar un proporcion binomial con la precision deseada
#' @export np
#' @examples
#' np(x=25, n=210, d=.05,decs=3)
#' np(x=115, n=210, d=.10)
#' np(x=25, n=210, d=.05, conf=.90, decs=5)
#' np(x=25, n=210, d=.05, conf=.90, decs=5, eco=FALSE)
#'


#tama\u00f1o de muestra para estimar una proporci\u00f3n binomial
#####################################################
np<-function(x=0,n=0,d=0,v=0,level="",conf=.95,alfa=.05, decs=4, eco=TRUE){
  tab<-"  "
  #if(conf!=.95){alfa<- 1-conf} else {if (alfa!=0.05){conf<- 1-alfa}}
  epairsget(p=alfa,q=conf, pmin=0.0001,pdefault=0.05)->par_alfa
  if(par_alfa[[1]][1]){
    alfa<-par_alfa[[2]][1]
    conf<-par_alfa[[2]][2]
  } else {stop("valor de alfa o de conf incongruente")}


  zalfa<- qnorm(1 - (alfa / 2))
  lf <- 0
  if (length(v)>1) #si se pasa una variable en v, se obtiene n de ella y x o level indica la clase de interes
  { piloto<-TRUE
    lf<-length(v)
    v<-v[!is.na(v)]
    n <-length(v)
    n0<-length(v[v==x])
    if (level !=""){n0<-length(v[v==level])}
  }
  else
  { if(x>=0 && n>0)
    {piloto<-TRUE
     n0<-x
    }
    else
    {piloto<-FALSE
     po<-0.5}
  }

  if (piloto){
    waldaj<-vector("list",length = 2)
    waldaj <-icpwaldajustado(x=n0,n=n,conf=conf, alfa=alfa)
    dobs<- (waldaj[[2]]- waldaj[[1]])/2
    if(waldaj[[1]]<=.5 && waldaj[[2]]>=.5)
    {po<-.5}
    else
    {ifelse(waldaj[[1]]>=.5,po<-waldaj[[1]],po<-waldaj[[2]])}
  }

  if(d<=0 || d>1) {cat("ERROR: No valid data\n")}
  else{

    nnoinfo<-(zalfa/(2*d))^2
    ninfo<-((zalfa/d)^2)*(po*(1-po))
    if(eco){
      cat("\n")
      cat("Tama\u00f1o de muestra para estimar una proporci\u00f3n binomial \n")
      cat("-------------------------------------------------------\n")
      cat("\n")
      if(piloto){
        cat("Informaci\u00f3n muestral \n")

        if(lf!=0 && lf!=n){
          #valores faltantes
          mss=lf-n
          cat(tab,"Declarados ",lf,"casos. ",sep="")
          if (mss==1) {cat(tab,"Hay",mss,"valor faltante \n",sep="")} else
          {cat(tab,"Hay",mss,"valores faltantes \n",sep="")}

        } else {
          if(length(x)>1) {cat(tab,"No hay valores faltantes \n", sep="")}
        }


        cat(tab,"Tama\u00f1o de la muestra: n = ", n, "\n",sep="")
        cat(tab,"Casos: x = ",n0, "\n",sep="")
        cat(tab,"Inferencia para la proporci\u00f3n basada en el m\u00e9todo de Wald ajustado: \n",sep="")
        cat(tab,conf*100,"%-IC(\u03c0): (",roundf(waldaj[[1]],decs),", ",roundf(waldaj[[2]],decs),") \n",sep="")
        cat(tab,"precisi\u00f3n observada: d = ",roundf(dobs,decs)," (",roundf(dobs*100,2),"%) \n",sep="")

        cat("\n")
        cat("Tama\u00f1o muestral requerido para \u03b4 = ",d," (",roundf(d*100,2),"%), conf.= ",conf*100,"% \n",sep="")
        if (po==.5){
          cat(tab,"No se distinguen casos con y sin informaci\u00f3n (la muestra actual es compatible con p =",po,"): n= ",trunc(ninfo+1,0),"\n",sep="")
        }
        else {
          cat(tab,"- Basado en la muestra actual (po = ",roundf(po,decs),"):   n \u2265 ",trunc(ninfo+1,0),"\n",sep="")
          cat(tab,"- Sin considerar la informaci\u00f3n previa: n \u2265 ",trunc(nnoinfo+1,0),"\n",sep="")}
      }
      else
      {
        cat("Tama\u00f1o muestral requerido para  \u03b4 = ",d,"(",roundf(d*100,2),"%), conf.= ",conf*100,"% \n",sep="")
        cat(tab,"sin informaci\u00f3n previa: n = ",trunc(nnoinfo+1,0),"\n",sep="")
      }
      cat("\n")
      }
    else {
      res<-as.list(c(ninfo,nnoinfo))
      res
    }
  }

}

