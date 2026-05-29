#' @noRd
#' @title Diagramas por grupos
#' @description Obtencion de los diagramas de distribucion segmentados, o no, por factor de agrupacion
#' @param x vector: variable cuantitativa a resumir
#' @param f vector: factor cuyos niveles segmentan a la variable cuantitativa (si falta se da la descriptiva de x)
#' @param se valor real: error estandar a representar
#' @param ggid valor entero: indicador del grafico a representar
#' @param lbls vector de caracteres: nombres de los ejees
#' @param bins entero: numero de intervalos para el histograma
#' @param hnmin entero: tamano minimo/maximo de muestra para representar el histograma/diagrama de puntos
#' @return Representacion grafica
#' @importFrom stats na.exclude
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_pointrange aes facet_wrap geom_bar geom_boxplot geom_col geom_dotplot
#' @importFrom ggplot2 geom_errorbar geom_freqpoly geom_histogram geom_hline geom_jitter
#' @importFrom ggplot2 geom_linerange geom_point geom_pointrange geom_smooth geom_violin ggplot
#' @importFrom methods show
#' @export grpsggp

grpsggp<-function(x=NULL,f=NULL,se=NULL,ggid=1,lbls=NULL,bins=NULL,hnmin=50)

  {
  # entra x como vector que puede estar segmentado por f (2 muestras) o no segmentado (1 muestra)


  #Nombre de las variables
  nx<-length(x)
  xname<-deparse(substitute(x))
  ifelse(is.null(f), fname<-"", fname<-deparse(substitute(f)))
  if(!is.null(lbls)){
    xname<-lbls[1]
    if(length(lbls)>1) fname<-lbls[2]
  }

  # prevencion de datos faltantes y tipo
  if(is.null(f)) {f<-rep(" ",nx)}
  dat<-na.exclude(data.frame(f,x))
  x<-dat$x
  f<-dat$f
  if(is.numeric(x)){
    m <- tapply(x, f, mean, na.rm = TRUE)
    if(is.null(se)){
      s <- tapply(x, f, sd,na.rm = TRUE)}
    else{
      s=se
    }
    if(length(m)!=length(s)) stop("vectores de m() ",length(m)," y se() ",length(s)," de diferente dimensi\u00f3n")
    n <- tapply(x, f, length)
    li<-m-s
    ls<-m+s
    level<-levels(f)
    if (is.null(level)) level<-c(" ")
    ms<-data.frame(level,n,m,s,li,ls)
  } else {
    li<-NA
    ls<-NA
    level<-NA
    m<-NA
    s<-NA
    ms<-NA
  }

  gg<-list()
  ggbase_x <-ggplot2::ggplot(data=dat, mapping=aes(x))
  ggbase_fx<-ggplot2::ggplot(data=dat, mapping=aes(f,x))

  k=1 #histograma o puntos
  if(nx>hnmin) {
     gh<- geom_histogram(color="gray82",fill="lightskyblue3")
     if(!is.null(bins)) {gh <- geom_histogram(binwidth=bins)}
  } else{
     gh<- geom_dotplot()

  }

  gg[[k]]<-ggbase_x+
    gh+
    labs(x=xname,y="casos")

  k=2 #histograma por grupos
  if(length(ggid[ggid==k])>0){
     gg[[k]]<-gg[[1]]+
       facet_wrap(f,ncol=1)+
       labs(x=xname,y="casos", title="Distribuci\u00f3n por grupos")
  }


  k=3 #boxplot
  if(length(ggid[ggid==k])>0){
    gg[[k]]<-ggbase_fx+geom_boxplot()+
    labs(x=fname,y=xname)
  }

  k=4 #dispersion
  if(length(ggid[ggid==k])>0){
    gg[[k]]<-ggbase_fx+geom_jitter()+
    labs(x=fname,y=xname, title="Dispersi\u00f3n por grupos")

 }

  k=5 #dispersion con IC
  if(length(ggid[ggid==k])>0){
    gg[[k]]<-ggplot(data=dat)+
      geom_jitter(mapping=aes(f,x),color="darkgray")+
      geom_errorbar(data=ms,mapping=aes(level,ymin=li,ymax=ls),color="blue",width=0.3)+
      labs(x=fname,y=xname)
    }

  k=6 # violin
  if(length(ggid[ggid==k])>0){
    gg[[k]]<-ggbase_fx+geom_violin()+
    labs(x=fname,y=xname)
  }

  k=7 #violin con ic
  if(length(ggid[ggid==k])>0){
    gg[[k]]<-ggplot()+
      geom_violin(data=dat,mapping=aes(f,x))+
      geom_errorbar(data=ms,mapping=aes(level,ymin=li,ymax=ls),color="blue",width=0.3)+
      labs(x=fname,y=xname)
  }

  k=8 #poligono de frecuencias
  if(length(ggid[ggid==k])>0){
    gg[[k]]<-ggplot(dat, aes(x, colour = f)) +
      geom_freqpoly(binwidth = 0.5)+
      labs(x=xname,y="casos")
   }
  k=9 #barras con IC

  if(length(ggid[ggid==k])>0){

     gg2<-ggplot(ms,mapping=aes(level, m, ymin = li, ymax = ls))+
      geom_pointrange(mapping=aes(y=m,ymin=li,ymax=ls))+
      geom_errorbar(mapping=aes(level,ymin=li,ymax=ls),width=0.3)+

      labs(x=fname,y=xname);
      ifelse(min(x)>=0, gg[[k]] <-gg2+geom_col(mapping=aes(x=level,y=m),alpha=0.15),
                        gg[[k]] <-gg2)
  }

  k=10 #barras

  if(length(ggid[ggid==k])>0){
    gg[[k]]<-ggplot(data=dat, mapping=aes(x))+
      geom_bar(color="gray82",fill="lightskyblue3")+
      labs(x=xname,y="casos")
  }

  ng=length(ggid)
  for(i in 1:ng) {
    if(!is.null(gg[[ ggid[i] ]])){
    suppressMessages(show(gg[[ ggid[i] ]] ) )
    }
  }

}


