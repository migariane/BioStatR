#' @title Analisis de una tabla de contingencia 2x2
#' @description Analisis de tablas 2x2. Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres.
#' @param o vector de enteros: Vector de frecuencias observadas con la organizacion (o11,o12,o21,o22)
#' @param o11 entero: frecuencia observada en la fila 1 y columna 1
#' @param o12 entero: frecuencia observada en la fila 1 y columna 2
#' @param o21 entero: frecuencia observada en la fila 2 y columna 1
#' @param o22 entero: frecuencia observada en la fila 2 y columna 2
#' @param frecs data.frame: data.frame con frecuencias observadas
#' @param fvar vector de enteros o factores: Variable por filas
#' @param cvar vector de enteros o factores: Variable por columnas
#' @param fcat vector de cadenas de texto:  Nombres de fila
#' @param ccat vector de cadenas de texto:  Nombres de columna
#' @param estudio caracter: Tipo de estudio "T"=transversal, "P"=prospectivo, "R"=retrospectivo
#' @param tablas vector de caracter: "E"=frecs. esperadas, "R","S","X", residuos de Pearson, estandarizados y contribucion X2; "F", "C", "T" porcentajes por filas, columnas y totales
#' @param alfa real en (0,1): Nivel de error de los intervalos (alternativa a conf)
#' @param conf real en (0,1): Nivel de confianza de los intervalos (alternativa a alfa)
#' @param decs entero: Numero de decimales en las salidas
#' @param ptol decimal en (0,1): Tolerancia para resumir el valor de p
#' @param medidas valor logico: si es TRUE/FALSE se muestran/omiten las medidas de asociacion
#' @return Informe analisis de tablas 2x2 mediante tests Chi2 y exacto de Fisher con medidas de asociacion
#' @importFrom stats fisher.test pchisq
#' @export tabla2x2
#' @examples
#'# [1] Formato esperado de la tabla (presencia de enfermedad
#'# (variable respuesta) vs exposicion a factor de riesgo (var. factor))
#'#
#'#             Expuestos      No Expuestos
#'#--------------------------------------------
#'# Enfermos        o11           o12
#'# Sanos           o21           o22
#'# --------------------------------------------
#'#
#'#[1a] Introduccion de frecuencias individuales en estudio transversal,
#'#     se piden porcentajes por filas
#'tabla2x2(o11=20, o12=26, o21=60, o22=294, estudio = "T", tablas="F")
#'
#'#[1b] Los mismos datos introducidos como vector, pidiendo tambien
#'#     las frecuencias esperadas
#'tabla2x2(o=c(20, 26, 60,294), estudio = "T",fcat=c("Peso normal","Peso bajo"),
#'         ccat=c("fuma","No fuma"), tablas=c("F","E","S"))
#'tabla2x2(frecs=c(20, 26, 60,294), estudio = "T", tablas=c("F","E"))
#'
#'#[2]  Datos como variables o columnas de un data.frame
#'w1<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
#'w2<-c(1,1,1,2,1,1,1,1,2,1,1,1,1,1,2,1,2,2,1,2,2,1,2,2,1,2,2,1,2,2,1,2,1,2,2,2)
#'tabla2x2(fvar=w1,cvar=w2)
#'#     Estudio retrospectivo solicitando porcentajes por filas
#'v1<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)
#'v2<-c(1,1,1,2,1,1,1,1,2,1,1,1,1,1,2,1,2,2,1,2,2,1,2,2,1,2,2,1,2,2,1,2,1,2,2,2)
#'tabla2x2(fvar=v1,cvar=v2, estudio="R", tablas="F")
#'

tabla2x2<-function(frecs=NULL,fvar=NULL,cvar=NULL,o=NULL,o11=0, o12=0, o21=0, o22=0, fcat=c("F1","F2"), ccat=c("C1","C2"), estudio="T", tablas="",medidas=TRUE,alfa=0.05, conf=0.95, decs=3, ptol=0.001)
{

  tab="  "
  chi2<-"\u03c7\U00b2"


  indent<-2

  epairsget(p=alfa,q=conf, pmin=0.0001,pdefault=0.05)->par_alfa
  if(par_alfa[[1]][1]){
    alfa<-par_alfa[[2]][1]
    conf<-par_alfa[[2]][2]
  } else {stop("valor de alfa o de conf incongruente")}


  txtconf<-paste(roundf(conf*100),"%",sep="")
  lblf1<-fcat[1]
  lblf2<-fcat[2]
  lblc1<-ccat[1]
  lblc2<-ccat[2]

  if(is.null(o) && !is.null(frecs)){o<-frecs}

  testudio<-"indefinido"
  if (estudio=="T" || estudio=="t") {testudio<-"transversal"
                                     estudio <-"t" }
  if (estudio=="P" || estudio=="p") {testudio<-"prospectivo"
                                     estudio <-"p" }
  if (estudio=="R" || estudio=="r") {testudio<-"retrospectivo"
                                     estudio <-"r" }

  # lectura de datos
  entrada<-0
  vectores<-(!is.null(fvar) && !is.null(cvar))
  if(vectores) #entran columnas o vectores de datos
  {entrada<- 1
   tabla<-table(fvar,cvar)
   if(nrow(tabla)==2 && ncol(tabla)==2) #la tabla es 2x2
   { o11<-tabla[[1,1]]
     o12<-tabla[[1,2]]
     o21<-tabla[[2,1]]
     o22<-tabla[[2,2]]
     rownames(tabla)<-fcat
     colnames(tabla)<-ccat
   }
  else
   {
    entrada<- -1
   } # la tabla no es 2x2
  } #  end (entran columnas o vectores de datos)
  else # entran frecuencias
  {t<- o11+o12+o21+o22
   if(t>0)
    {
      entrada<-2
      tabla<-matrix(data=c(o11,o12,o21,o22),nrow=2,ncol=2,byrow = TRUE, dimnames=list(fcat,ccat))
    }
  else
  { if (length(o)==4)
    { entrada <- 3
      o11<-o[1]
      o12<-o[2]
      o21<-o[3]
      o22<-o[4]
      tabla<-matrix(data=o,nrow=2,ncol=2,byrow = TRUE, dimnames=list(fcat,ccat))
    }
  else
    {
      if(is.table(o)){
        if(nrow(o)==2 && ncol(o)==2)
        {
          o11<-o[1,1]
          o12<-o[1,2]
          o21<-o[2,1]
          o22<-o[2,2]
          tabla<-o
        } else
        {stop("La tabla introducida no es 2x2")

        }
      }
      else{


      entrada<-0
      tabla<-NULL}}
    } # al final no hay datos
  }

  #######
  if(entrada<=0)
    {  if (entrada==0){ stop("ERROR - No se han indicado frecuencias validas \n")}
       if (entrada<0) {
           stop("ERROR - La tabla generada tiene dimensiones ",nrow(tabla),"x",ncol(tabla)," se detiene el proceso \n","        Utilice la funci\u00f3n tablarxc() para tablas con alguna dimensi\u00f3n mayor a 2. \n")
         }
    }
  else #entrada >0
    {
      # ConstrucciC3n tabla
      f1 <- o11+o12
      f2 <- o21+o22
      c1 <- o11+o21
      c2 <- o12+o22
      t  <- f1+f2
      m<-matrix(data=c(o11,o12,o21,o22),nrow=2,ncol=2,byrow = TRUE)


      if(estudio=="t")
      {cpc<-0.5 }   # Estudio transversal
      else
      { if(estudio=="p")
      {   cpc <- ifelse(c1==c2,2,1)} # Estudio Prospectivo
        else
        { cpc <- ifelse(f1==f2,2,1)} # Estudio Retrospectivo
      }

      mine <- min(f1,f2)*min(c1,c2)/t
      if(estudio=="t")
        {cvalidez<-ifelse(t>500,6.2,3.9)}
      else
        {cvalidez<-ifelse(t>500,14.9,7.7)}
      validez <- ifelse(mine>=cvalidez,TRUE,FALSE)

      # X2
      x2<- t*((abs(o11*o22-o12*o21)-cpc)^2)/(f1*f2*c1*c2)
      gl<- 1
      pval<-pchisq(x2,gl,lower.tail=FALSE)
      ptxt<-""
      ptxt<-ifelse( pval<=ptol,  paste("p < ",ptol,sep=""), paste("p = ",roundf(pval,decs),sep="") )

      x2y<-t*((abs(o11*o22-o12*o21)-t*0.5)^2)/(f1*f2*c1*c2)
      pvaly<-pchisq(x2y,gl,lower.tail=FALSE)
      ptxty<-""
      ptxty<-ifelse( pvaly<=ptol,  paste("p < ",ptol,sep=""), paste("p = ",roundf(pvaly,decs),sep="") )
      x2n<-t*((abs(o11*o22-o12*o21))^2)/(f1*f2*c1*c2)
      pvaln<-pchisq(x2y,gl,lower.tail=FALSE)
      ptxtn<-""
      ptxtn<-ifelse( pvaly<=ptol,  paste("p < ",ptol,sep=""), paste("p = ",roundf(pvaln,decs),sep="") )


      tab<-""
      for (i in 1:indent) tab<-paste(tab," ",sep="")
      showtitle("An\u00e1lisis de tablas 2x2",1)
      showtitle("Frecuencias observadas",2)
      tobs<-trxcshow(x=m,fcat=fcat,ccat=ccat,tipo="F")
      print.table(tobs)
      if(length(tablas[tablas=="E"])>0 ||length(tablas[tablas=="e"])>0)
        {
            showtitle("Frecuencias esperadas",2)
            tesp<-trxcshow(x=m,fcat=fcat,ccat=ccat,tipo="E",decs=2)
            print.table(tesp)
        }
      cat("\n")
      showtitle(paste("Test Chi-cuadrado para un estudio ",testudio,sep=""),2)
      cat("\n")
      cat(paste(tab,chi2," = ", roundf(x2,decs),", ",sep=""),paste(" gl = ",gl,", ",sep=""), paste(ptxt ,", (cpc = ",cpc,")",sep=""), "\n")
      cat(paste(tab,"Validez: Frecuencia m\u00ednima esperada = ", roundf(mine,2),sep=""),paste(ifelse(validez," > "," < "),cvalidez,sep=""),ifelse(validez,"",paste(" Test ",chi2," no valido",sep="")),"\n")
      cat("\n")
      fisher<-fisher.test(matrix(c(o11,o12,o21,o22),nrow=2,byrow=TRUE))
      pfisher<-fisher[[1]]
      ptxt<-ifelse( pfisher<=ptol,  paste("p < ",ptol,sep=""), paste("p = ",roundf(pfisher,decs),sep="") )
      cat(paste(tab,"Test exacto de Fisher (bilateral): ",ptxt, sep=""), "\n")
      cat("\n")
      cat(paste(tab,"--- Otros criterios ",chi2,": ",sep=""),"\n")
      cat(paste(tab,chi2," = ", roundf(x2n,decs),", ",sep=""),paste(" gl = ",gl,", ",sep=""),paste(ptxtn,", (sin cpc)",sep=""),"\n")
      cat(paste(tab,chi2," = ", roundf(x2y,decs),", ",sep=""),paste(" gl = ",gl,", ",sep=""),paste(ptxty,", (cpc de Yates = ",roundf(t/2,2),")",sep=""),"\n")

      # Tablas de residuos
      ifelse(decs==4,ndecs<-3,ndecs<-decs)
      if(length(tablas[tablas=="R"])>0 ||length(tablas[tablas=="r"])>0)
      {
        showtitle("Residuos de Pearson",2)
        print.table(trxcshow(x=m,fcat=fcat,ccat=ccat,decs=ndecs, tipo="P"))
      }

      ifelse(decs==4,ndecs<-3,ndecs<-decs)
      if(length(tablas[tablas=="S"])>0 ||length(tablas[tablas=="s"])>0)
      {

        showtitle("Residuos estandarizados",2)
        print.table(trxcshow(x=m,fcat=fcat,ccat=ccat,decs=ndecs, tipo="S"))
      }

      if(length(tablas[tablas=="X"])>0 ||length(tablas[tablas=="x"])>0)
      {
        showtitle(paste("Contribuci\u00f3n a",chi2,sep=""),2)
        print.table(trxcshow(x=m,fcat=fcat,ccat=ccat,decs=ndecs, tipo="X"))

      }

      # Tablas de porcentajes
      ifelse(decs==4,ndecs<-3,ndecs<-decs)
      if(length(tablas[tablas=="F"])>0 ||length(tablas[tablas=="f"])>0)
      {
        showtitle("Porcentajes por filas",2)
        print.table(trxcshow(x=m,fcat=fcat,ccat=ccat,decs=ndecs, tipo="R"))
      }
      if(length(tablas[tablas=="C"])>0 ||length(tablas[tablas=="c"])>0)
      {
        showtitle("Porcentajes por columnas",2)
        print.table(trxcshow(x=m,fcat=fcat,ccat=ccat,decs=ndecs, tipo="C"))
      }
      if(length(tablas[tablas=="T"])>0 ||length(tablas[tablas=="t"])>0)
      {
        showtitle("Porcentajes totales",2)
        print.table(trxcshow(x=m,fcat=fcat,ccat=ccat,decs=ndecs, tipo="T"))
      }


    if(medidas){

      if(estudio=="t"){ #metodo de wald ajustado
        showtitle(paste("Estimaci\u00f3n de la prevalencia",greek("p")," en un estudio transversal",sep=""),2)
        zalfa<- qnorm(1 - (alfa / 2))
        prev.res<-vector("list",length = 2)
        prev.p<-(f1+2)/(t+4)
        prev.sep<-sqrt( (f1+2) * (t-f1+2) /(t+4))/(t+4)
        prev.res[[1]]<-prev.p-zalfa*prev.sep
        if(prev.res[[1]]<0){prev.res[[1]]<-0}
        prev.res[[2]]<-prev.p+zalfa*prev.sep
        if(prev.res[[2]]>1){prev.res[[2]]<-1}
        cat(paste(tab,"M\u00e9todo de Wald ajustado: \n",sep=""))
        cat(paste(tab,"p=",roundf(prev.p,decs),"; ",txtconf,"-IC(",greek("p"),")=(",roundf(prev.res[[1]],decs),", ",roundf(prev.res[[2]],decs),")",sep=""),"\n")

      }


    # MEDIDAS DE ASOCIACION
      showtitle(paste("Medidas de asociaci\u00f3n para un estudio ",testudio,sep=""),2)
      if(estudio=="t" || estudio=="p")
      { # Diferencia de Berkson
        cat(paste(tab,"[!] Las medidas de riesgo se calculan como riesgo de la categor\u00eda  \n",sep=""))
        cat(paste(tab,"    en la 1a columna (frente a la 2a) para la categor\u00eda en la 1a \n",sep=""))
        cat(paste(tab,"    fila (frente a la 2a)\n",sep=""))
        cat("\n")
        dberkson<-t2x2dberkson(c(o11,o12,o21,o22),conf=conf,decs=decs)
        cat(paste(tab,"Riesgo absoluto (diferencia de Berkson; m\u00e9todo de Agresti-Caffo): \n",sep=""))
        cat(paste(tab,"d=",roundf(dberkson[[1]],decs),"; ",txtconf,"-IC(d)=(",roundf(dberkson[[3]],decs),", ",roundf(dberkson[[4]],decs),")",sep=""),"\n")
        cat("\n")
        # Riesgo relativo
        rr<-t2x2Rr(c(o11,o12,o21,o22),conf=conf,decs=decs)
        cat(paste(tab,"Riesgo relativo: \n",sep=""))
        cat(paste(tab,"Rr=",roundf(rr[[1]],decs),"; ",txtconf,"-IC(Rr)=(",roundf(rr[[3]],decs),", ",roundf(rr[[4]],decs),")",sep=""),"\n")
      }


      ifelse(estudio=="r",ret<-TRUE,ret<-FALSE)
      if(estudio=="t" || estudio=="r"){
      # Ratribuible
      ra<-t2x2Ra(c(o11,o12,o21,o22),retro=ret,conf=conf,decs=decs)
      cat("\n")
      cat(paste(tab,"Riesgo atribuible",ifelse(ret==TRUE,"*: ",":"),"\n",sep=""))
      cat(paste(tab,"Ra=",roundf(ra[[1]],decs),"; ",txtconf,"-IC(Ra)= (",roundf(ra[[3]],decs),", ",roundf(ra[[4]],decs),")",sep=""),"\n")
      if (ret){
        cat(paste(tab,"* La estimaci\u00f3n de Ra para estudios retrospectivos es una aproximaci\u00f3n v\u00e1lida si la prevalencia de la enfermedad es baja: P(E) < 10%",sep=""),"\n")
      }}

        # OR
        or<-t2x2or(c(o11,o12,o21,o22),conf=conf,decs=decs)
        cat("\n")
        cat(paste(tab,"Raz\u00f3n del producto cruzado (odds ratio):\n",sep=""))
        cat(paste(tab,"OR=",roundf(or[[1]],decs),"; ",txtconf,"-IC(OR)= (",roundf(or[[3]],decs),", ",roundf(or[[4]],decs),")",sep=""),"\n")
        if(ret)
        {cat(paste(tab,"* La estimaci\u00f3n para OR sirve de aproximaci\u00f3n al riesgo relativo siempre que la prevalencia de la enfermedad sea P(E) < 10%",sep=""),"\n")

        }

      }
    } #end entrada>0
} #end tabla2x2

