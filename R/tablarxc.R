#' @title Analisis de una tabla de contingencia RxC
#' @description Analisis de tablas de contingencia RxC (con R y/o C mayores a 2. No valido para tablas 2x2). (Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres)
#' @param frecs data.frame: data.frame con frecuencias observadas
#' @param fvar vector de enteros o factores: Variable por filas
#' @param cvar vector de enteros o factores: Variable por columnas
#' @param fcat vector de cadenas de texto:  Nombres de fila
#' @param ccat vector de cadenas de texto:  Nombres de columna
#' @param tablas vector de caracter: "E"=frecuencias esperadas, "R","S","X", residuos de Pearson, estandarizados y contribucion X2; "F", "C", "T" porcentajes por filas, columnas y totales
#' @param decs entero: Numero de decimales en las salidas
#' @param o vector de enteros: frecuencias de la tabla de contingencia. Requiere especificar el numero de filas con fnum
#' @param fnum valor entero: numero de filas cuando se indican las frecuencias a traves del parametro o
#' @return Informe test para analisis de tablas RxC mediante test Chi2
#' @export tablarxc
#' @examples
#'# [1] Introduccion de datos como variables
#'v1<-c(1,1,2,1,2,1,1,1,1,2,1,1,1,1,1,1,1,1,2,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
#'v2<-c(1,2,3,3,3,2,1,2,1,3,1,2,3,3,3,1,2,1,3,1,2,1,2,3,2,3,1,2,1,1,3,1,1,3,3,1,2,1,1,2,1,2,2,2)
#'tablarxc(fvar=v1,cvar=v2)
#'# Obtencion de tablas de frecuencias esperadas y porcentajes por filas
#'tablarxc(fvar=v1,cvar=v2, tablas=c("E","F"))
#'# Uso de nombres de categoria
#'tablarxc(fvar=v1,cvar=v2, tablas=c("E","F"), fcat=c("Trat1","Trat2"),ccat=c("peor","igual","mejor"))
#'
#'# [2] Introduccion directa de las frecuencias (previamente construir por columnas un dataframe)
#'peor <-c(8,9,10)
#'igual<-c(6,8,12)
#'mejor<-c(4,9,10)
#'tabla<-data.frame(peor,igual,mejor)
#'tablarxc(frecs=tabla, tablas=c("E","F","S"), fcat=c("Trat1","Trat2"))
#'
#'# [3] Introduccion de frecuencias observadas como vector
#'obs<-c(12,35,13,25,8,10)
#'tablarxc(o=obs, fnum=2)
#'
#'
#'
tablarxc<-function(frecs=NULL, fvar=NULL,cvar=NULL, o=NULL,fnum=0, fcat=NULL, ccat=NULL,tablas="",decs=3)
{
  tab="  "
  chi2<-"\u03c7\U00b2"
  err<-FALSE

  # Depuracion previa
  if (!is.null(frecs)){
    if(is.null(nrow(frecs))){
      if(length(frecs)>0){
        o<-frecs
        frecs<-NULL
      }
      }
  }
  if (!is.null(o)){
      if (is.table(o)||is.data.frame(o)) {
        frecs<-o
        o<-NULL
      }
      else{
        if(fnum==0){
          stop("Al introducir las frecuencias como vector, hay que indicar el num de filas de la tabla con fnum = .")
        }
      }
  }

  if (!is.null(frecs)){
    # entrada como data.frame
      nr<-nrow(frecs)
      nc<-ncol(frecs)
      if(is.null(nrow(frecs))){stop("El p\u00e1rametro frecs se debe usar para introducir tablas. La introducci\u00f3n del vector de frecuencias debe hacerse con o")}
      x<-as.table(matrix(0,nr,nc))
      for(i in 1:nr) {
        for (j in 1:nc){
          x[[i,j]]<-frecs[[i,j]]
        }
      }
    fcat<-rownames(frecs)
    ccat<-colnames(frecs)
  }
  else {
  if (!is.null(fvar) && !is.null(cvar))
  {# entrada como filas y columnas
     x<-table(fvar,cvar)
     nr<-nrow(x)
     nc<-ncol(x)
  }
    else
    {  if (!is.null(o)){
       #error check
      if(!is.null(nrow(o))){
        err<-TRUE
        stop("Datos incorrectos. Para introducir datos en forma de tabla utilice el par\u00e1metro frecs. \n")
      }
      if(fnum==0 && err==FALSE){
        err<-TRUE
        stop("Datos incorrectos. Con el par\u00e1metro o se debe indicar el n\u00famero de filas de la tabla con el p\u00e1rametro fnum  \n")
      }

      if(err==FALSE){

       #los datos se meten como o=c()
       nr<-fnum
       if(length(o)%%nr==0){
         nc<-length(o)/nr
         x<-as.table(matrix(data=o,nrow=nr,byrow=TRUE))

         if(is.null(ccat)){
           s<-"C1"
           for(i in 2:nc){
              s<-c(s,paste("C",i,sep=""))
           }
           ccat<-s
         }
         if(is.null(fcat)){
           s<-"F1"
           for(i in 2:nr){
             s<-c(s,paste("F",i,sep=""))
           }
           fcat<-s
         }
       }
       else {
        stop("Dimensiones incorrectas para el vector de observaciones \n")
        err<-TRUE
       }
      }

  }

  }
  }

 if(err==FALSE){

   nr<-nrow(x)
   nc<-ncol(x)



  if(nr==2 && nc==2){
    cat("-------------------------------------------------------------------------\n")
    cat(" La tabla observada tiene dimensiones 2x2, por lo que el test para tablas\n")
    cat(" mayores NO es adecuado.        \n")
    cat(" A continuaci\u00f3n, se realiza el test para tablas 2x2 considerando que el \n")
    cat(" estudio es de tipo transversal. Para profundizar en el an\u00e1lisis, use \n")
    cat(" la funci\u00f3n tabla2x2() con los par\u00e1metros adecuados. \n")
    cat("-------------------------------------------------------------------------\n")
    cat("\n")
    tabla2x2(o11=x[1,1],o12=x[1,2],o21=x[2,1],o22=x[2,2], fcat=fcat, ccat=ccat, estudio="T", tablas="", medidas=FALSE, decs=decs)

    err<-TRUE
  }
  if(err==FALSE){

    # test<-chisq.test(fvar,cvar)
    showtitle("Test Chi-cuadrado para tablas RxC",lev=1)
    showtitle("Frecuencias observadas",lev=2)
    print.table(trxcshow(x,tipo="F",fcat=fcat,ccat=ccat,decs=decs))
    esperadas<-trxcshow(x,tipo="E",fcat=fcat,ccat=ccat,decs=6)
    # test
    X2exp<-0
    neless1 <-0
    neless5 <-0
    emin<-9999
    for(i in 1:nrow(x)) {
      for(j in 1:ncol(x)){
        emin<-min(emin,esperadas[[i,j]])
        if(esperadas[[i,j]]<=1)
          {neless1<-neless1+1}
        else{
          if(esperadas[[i,j]]<=5) neless5<-neless5+1
        }
      }
    }

    if(length(tablas[tablas=="E"])>0 ||length(tablas[tablas=="e"])>0)
    {
      showtitle("Frecuencias esperadas",2)
      print.table(trxcshow(x,tipo="E",fcat=fcat,ccat=ccat,decs=2))
    }

    showtitle("Test chi-cuadrado ",lev=2)
    cat(paste(tab,"Validez: Frecuencia m\u00ednima esperada = ",round(as.numeric(emin),2),sep=""),"\n")
    cat(paste(tab,"         ",neless1," frecuencias esperadas son menores a 1",sep="","\n"))
    cat(paste(tab,"         ",neless5," son menores a 5 (el ",round(neless5*100/(nr*nc),1),"% de la tabla)",sep=""),"\n")
    if(neless1>1 || neless5/(nr*nc)>0.2){
      cat(paste(tab,"[!] El test ",chi2," NO es v\u00e1lido con estos datos \n"))
    }
    if(neless1==0){
      for(i in 1:nrow(x)) {
        for(j in 1:ncol(x)){
          X2exp<-X2exp+  ((as.numeric(x[[i,j]])^2) / as.numeric(esperadas[[i,j]]))
        }
      }
      X2exp<-X2exp-sum(x)
      gl<-(nr-1)*(nc-1)
      pval<-pchisq(X2exp, df=gl,lower.tail = FALSE)
      ptol<-0.001
      ptxt<-ifelse( pval<=ptol,  paste("p < ",ptol,sep=""), paste("p = ",round(pval,decs),sep="") )
      cat(paste(tab,chi2,"(",gl," gl) = ",round(X2exp,decs),", ",ptxt,sep=""),"\n")
    } else  {
      cat("No se calcula el estad\u00edstico de contraste \n")
    }

    if(length(tablas[tablas=="R"])>0 ||length(tablas[tablas=="r"])>0)
    {
      showtitle("Residuos de Pearson",lev=2)
      print.table(trxcshow(x,tipo="P",fcat=fcat,ccat=ccat,decs=decs))
    }

    if(length(tablas[tablas=="S"])>0 ||length(tablas[tablas=="s"])>0)
    {
      showtitle("Residuos estandarizados",lev=2)
      print.table(trxcshow(x,tipo="S",fcat=fcat,ccat=ccat,decs=decs))
    }

    if(length(tablas[tablas=="X"])>0 ||length(tablas[tablas=="x"])>0)
    {
      showtitle("Contribuci\u00f3n a X2 ",lev=2)
      print.table(trxcshow(x,tipo="X",fcat=fcat,ccat=ccat,decs=decs))
    }

    if(length(tablas[tablas=="F"])>0 ||length(tablas[tablas=="f"])>0)
    {
      showtitle("Porcentajes por filas",lev=2)
      print.table(trxcshow(x,tipo="R",fcat=fcat,ccat=ccat,decs=decs))
    }
    if(length(tablas[tablas=="C"])>0 ||length(tablas[tablas=="c"])>0)
    {
      showtitle("Porcentajes por columnas",lev=2)
      print.table(trxcshow(x,tipo="C",fcat=fcat,ccat=ccat,decs=decs))
    }

    if(length(tablas[tablas=="T"])>0 ||length(tablas[tablas=="t"])>0)
    {
      showtitle("Porcentajes totales",lev=2)
      print.table(trxcshow(x,tipo="T",fcat=fcat,ccat=ccat,decs=decs))
    }
    }# end if err==false
   }# end if err==false
cat("\n")
}
