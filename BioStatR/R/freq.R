#' @title Obtencion de la tabla de frecuencias absolutas y relativas de las categorias de una variable
#' @description Permite obtener la tabla de frecuencias observadas para un vector de datos x o cada columna de un data.frame x (Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres).
#' @param x vector: vector o data.frame a describir
#' @param acum valor logico: TRUE=proporciona la frecuencia relativa acumulada
#' @param cuts valor entero: numero de intervalos a realizar. Si se omite se utiliza el criterio de Sturges
#' @param agrup valor logico: si agrup=FALSE no se hace agrupacion en intervalos aunque haya mas de 10 categorias
#' @param decs valor entero: numero de decimales a mostrar en la salida
#' @param grf valor logico: si TRUE/FALSE se proporciona/omite salida grafica
#' @param ... parametros de configuracion de la funcion grpsggp
#' @return si x es un vector, se devuelve un data.frame con la tabla de frecuencias. Si x es un data.frame, se muestra la tabla de frecuencias de cada columna, pero la funcion no devuelve ningun objeto
#' @export freq
#' @examples
#' dat<-c(12,15,13,12,11,14,15,15,15,12,11,13,14,15,NA)
#' freq(dat)
#'
#' cats<-c('a','b','c','b','c','b','c','a','c','c','a','a','a')
#' freq(cats,acum=FALSE,grf=TRUE)
#'
#' dat2<-rnorm(550,212.3,6.3)
#' freq(dat2, agrup=TRUE,cuts=5)
#'
#' t<-rbinom(25,20,0.65)
#' freq(t,agrup=FALSE,cuts=5,decs=2)
#'
#' nrm<-rnorm(50,250,2)
#' bnm<-rbinom(50,80,0.5)
#' df<-data.frame(nrm,bnm)
#' freq(nrm)
#' freq(bnm,agrup=TRUE,grf=TRUE)
#' freq(df,acum=FALSE,grf=TRUE,hnmin=60)
#'
#'
freq<-function(x=NULL,acum=TRUE,cuts=0,agrup=TRUE, decs=3,grf=TRUE,...){


  # funciones internas #######################################
  ### 1
  getfreq<-function(x,acum,decs){
    lf<-length(x)
    x<-x[!is.na(x)]
    n<-length(x)
    X<-table(x)
    cuts=nrow(X)
    df<-as.data.frame(X)

    Prop<-round(df$Freq/n,decs)
    df<-data.frame(df,Prop)
    Acum<-vector(length=cuts)
    if(acum){
      Acum[1]=Prop[1]
      for(i in 2:cuts){Acum[i]=Acum[i-1]+Prop[i]}
      Acum<-round(Acum,decs)
      df<-data.frame(df,Prop.Acum=Acum)
    }
  ###  #df$Pc<-round(df$Pc,decs)
    if(lf>n){cat("Valores faltantes:",lf-n,"\n")}
    cat("n=",n,'\n','\n')
    return(df)
  }
  ### 2
  getcuts<-function(x,cuts){
    xa=trunc(min(x))
    xb=trunc(max(x))+1
    n=length(x)
    sturges<-1+3.322*log10(n)
    if(cuts==0){cuts<-round(sturges,0)}
    cont=TRUE
    i=1;
    while(cont)
    { rg<-(xb-xa)
    resto=rg%%cuts
    if(resto==0){cont=FALSE}
    else{
      if(i%%2==0) {xa<-xa-1}
      else        {xb<-xb+1}
      i<-i+1
      if(i>10){cont=FALSE}
    }
    }
    amp<-rg/cuts
    bk<-c(xa)
    for(j in 2:(cuts+1)){bk<-c(bk,bk[j-1]+amp)}
    return(bk)
  }
  ### 3
  getints<-function(x,acum,cuts,decs){
    lf<-length(x)
    x<-x[!is.na(x)]
    n<-length(x)
    #if(length(cuts)>1) {ct<-sort(cuts)} else
    ct<-getcuts(x,cuts)
    tfi<-as.data.frame(table(x=factor(cut(round(x,0),breaks=ct))))
    tfi[["Prop"]]<-with(tfi,round(Freq/n,decs))
    if(acum){
      cuts=nrow(tfi)
      Acum<-vector(length=cuts)
      Acum[1]<-tfi$Prop[1]
      for(i in 2:cuts){Acum[i]=Acum[i-1]+tfi$Prop[i]}
      Acum<-round(Acum,decs)
      tfi<-data.frame(tfi,Prop.Acum=Acum)
    }
    tfi$Prop<-round(tfi$Prop,decs)

    if(lf>n){cat("Valores faltantes:",lf-n,"\n")}
    cat("n =",n,'\n','\n')
    return(tfi)
  }
  ##### 4
  ggrf<-function(x,ggid,xname){

    if(is.numeric(x)) {
      ggid<-1
      rgx<-range(x)[2]-range(x)[1]
     if(is.integer(x) && rgx<20) {
       ggid<-10
     }
    }else{
     ggid<-10}

    grpsggp(x=x,f=NULL, ggid=ggid,lbls=c(xname,""),...)
  }

  ##################################################
  if(is.null(x)) stop("Sin datos que resumir")
  #constantes
   gg_hist<-1   #histograma
   gg_bar <-10  #diagrama de barras


  cat("\n")
  cat("Distribuci\u00f3n de frecuencias\n")
  cat("--------------------------------\n")

  nc<-ncol(as.data.frame(x))

  if(nc>1){ #hay mas de una columna, se hace una tabla de frecs para cada col
   for(i in 1:nc) {
     clases<-length(unique(x[,i]))
     cat('\n')
     xname<-names(x)[i]
     cat("Variable: ",xname,'\n')
     if(clases>10 && agrup){
       result<-getints(x[,i],acum,cuts,decs)
       ggid<-gg_hist
     } else {
       result<-getfreq(x[,i],acum, decs)
       ggid<-gg_bar
     }
     print(result)
     if(grf) {ggrf(x[,i],ggid,xname)}
   }

  }  else  { # solo hay una columna
    clases<-length(unique(x))
    xname=deparse(substitute(x))
    cat("Variable: ",xname,'\n')
    if(clases>10 && agrup){
      result<-getints(x,acum,cuts,decs)
      ggid<-gg_hist #histograma
    } else {
      result<-getfreq(x,acum,decs)
      ggid<-gg_bar
    }

    print(result)
    if(grf) {ggrf(x,ggid,xname)}
  }
  ##return(result)

}

