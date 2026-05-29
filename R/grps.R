#' @title Tabla de medias y desviaciones por niveles de un factor
#' @description Obtencion de las medidas descriptivas, n, media, desviacion tipica para de una variable para cada nivel de un factor. (Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres)
#'   Se pueden indicar como parametro adicional (...) hnmin, que indica el n minimo requerido para hacer el histograma
#' @param x vector: variable cuantitativa a resumir
#' @param f vector: factor cuyos niveles segmentan a la variable cuantitativa (si falta se da la descriptiva de x)
#' @param ic valor logico: si ic=TRUE, proporciona el intervalo de confianza para la media (sin correccion de tipo Bonferroni)
#' @param grf valor logico: indica si se requiere salida grafica o no
#' @param alfa valor real entre 0 y 1: nivel de error (alternativo a conf)
#' @param conf valor real entre 0 y 1: nivel de confianza para la elaboracion de intervalos (alternativo a alfa)
#' @param decs entero: precision decimal de la salida
#' @param ... parametros de configuracion de la funcion grpsggp
#' @return Tabla con medidas descriptivas (n, media, y dt, y opcionalmente el IC) por niveles del factor f (si no hay factor se analiza la variable x)
#' @export grps
#' @examples
#' nivel<-c(1.1,2.1,2.2,3.2,0.1,.2,1.0,0.4,0.7,1.3,1.5,3.1,2.4,3.6,1.1,2.4,
#'          3.2,2.6,1.5,6.1,2.1,1.9,1,2.1,1.3,4.1,1.2)
#' grupo<-c("A","A","A","A","A","A","A","B","B","B","B","B","B","C","C","C",
#'          "C","C","C","C","C","C","D","D","D","D","D")
#' grps(x=nivel)
#' grps(x=nivel, f=grupo)
#' grps(x=nivel, f=grupo, ic=TRUE)
#' grps(x=nivel, hnmin=10)
#'
#
grps<-function(x=NULL,f=NULL,ic=FALSE,grf=TRUE,alfa=0.05,conf=0.95,decs=3,...)
  {
  if(is.null(x)){stop("No se han indicado datos validos")}

  t<- NULL
  tf <-NULL
  foot1<-""
  foot2<-""
  tab=" "

  # CODIFICACION DE CONDICIONES DE ENTRADA
  par_alfa<-epairsget(p=alfa,q=conf,pdefault=0.05)
  if(par_alfa[[1]][1]){
    alfa<-par_alfa[[2]][1]
    conf<-par_alfa[[2]][2]
  } else {stop("Valor de alfa o de conf incongruente")}

  haymiss<-FALSE
  nx<-length(x)
  validx<-x[!is.na(x)]
  haymiss<-(length(validx)<nx)


  if (is.null(f)) {
    names<-c(deparse(substitute(x)), " " )
    k<- 1 #k = numero de grupos
    f<- factor(rep(1,nx))
    stitle<-paste("Descriptiva de ",names[[1]],sep="")
    dataf<-na.exclude(data.frame(f,x))
  }else{
    names<-c(deparse(substitute(x)),deparse(substitute(f)) )
    f<- as.factor(f)
    k<- nlevels(f)

    # medidas por grupo
    stitle<-paste("Descriptiva de ",names[[1]]," por ",names[[2]],sep="")
    if(length(f)!=length(x)) stop(paste("Las variables ",names[[1]]," y ",names[[2]]," no tienen la misma longitud",sep=""))

    dataf<-na.exclude(data.frame(f,x))

    ntotal<-   tapply(x, f, length)
    n     <-   tapply(dataf$x, dataf$f, length) #n valido
    nmiss <-   ntotal-n

    m <- roundf(tapply(x, f, mean, na.rm = TRUE),decs)
    s <- roundf(tapply(x, f, sd,   na.rm = TRUE),decs)

    if(haymiss){
        t <-data.frame(n_total=ntotal, n_valido=n, n_faltante=nmiss,media=m,dt=s)
     } else {
        t <-data.frame(n=n,media=m, dt=s)
    }
  }

  ntotal_f <-nx
  n_f      <-length(validx)
  nmiss_f  <-ntotal_f-n_f

  m_f <- roundf(mean(x, na.rm = TRUE),decs)
  s_f <- roundf(  sd(x, na.rm = TRUE),decs)

  if(haymiss){
    tf <- data.frame(n_total=ntotal_f,n_valido=n_f,n_faltante=nmiss_f,media=m_f, dt=s_f)
  } else{
    tf <- data.frame(n=n_f,media=m_f, dt=s_f)
  }
  rownames(tf)[nrow(tf)]<-"Total"

  if(ic){
    if (k>1){
      se<-vector(mode="numeric",length=length(m))
      ic_inf=vector( length=k)
      ic_sup=vector( length=k)
      for(i in 1:k) {
        if(haymiss) gsize<-t$n_valido[i] else gsize<-t$n[i]
        interv<-icm(n=as.numeric(gsize), m=as.numeric(t$m[i]),s=as.numeric(t$dt[i]),decs=decs,conf=conf, eco=FALSE)
        ic_inf[i]=round(interv[[1]],decs)
        ic_sup[i]=round(interv[[2]],decs)

      }

      foot1<-paste("* ICs para las medias ",greek("m"),"[i] y ",greek("m")," global al ", round(conf*100,0),"% de confianza", sep="")
      foot2<-paste("  (sin correcci\u00f3n por inferencia m\u00faltiple)",lev="")
      cat("\n")
      t<-data.frame(t,ic_inf,ic_sup)
    }

    #sin division por grupos
    if (k==1) {
      foot1<-paste("* IC para la media ",greek("m")," al ",round(conf*100,0),"% de confianza", sep="")
      foot2<-""
    }
    interv_f<-icm(n=as.numeric(n_f), m=as.numeric(m_f),s=as.numeric(s_f),decs=decs,conf=conf, eco=FALSE)
    ic_f_inf=roundf(interv_f[[1]],decs)
    ic_f_sup=roundf(interv_f[[2]],decs)


    tf<-data.frame(tf,ic_inf=ic_f_inf,ic_sup=ic_f_sup)
  }
  if (k>1) tt<-rbind(t,tf) else tt<-tf


  #Salida de las medidas
  showtitle(stitle,lev=1)

  row.names(tt)<-paste(tab,row.names(tt),sep="")
  if(k>1) cat(paste(tab,names[[2]],sep=""),"\n")
  print(tt)
  if(ic){
    cat(paste(tab,"___________",sep=""),"\n")
    cat(paste(tab,foot1,sep=""),"\n")
    if(k>1) cat(paste(tab,foot2,sep=""),"\n")
    cat("\n")
  }

 #diagramas
  if(grf){
    se_t<-sd(validx,na.rm=TRUE)
    if(ic){
      if(k>1) {grpsggp(x=dataf$x,f=dataf$f,  se=se,  ggid=c(9,7,5,2),   lbls=names,...)}
      else    {grpsggp(x=dataf$x,f=NULL,  se=se_t,   ggid=c(1,5),       lbls=names,...) }
    } else {
      if(k>1) {grpsggp(x=dataf$x,f=dataf$f,  ggid=c(6,4,3,2),    lbls=names,...)}
      else    {grpsggp(x=dataf$x,f=NULL,     ggid=c(1,3),        lbls=names,...)}
    }
  }
  invisible(tt)
}



