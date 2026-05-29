#' @title Test de student con una y dos muestras (independientes o apareadas)
#' @description t-test con una y dos muestras, independientes o relacionadas  (se han omitido acentos deliberadamente por la incompatibilidad de caracteres de texto)
#' @param m0 valor real: valor a contrastar en el test de una muestra o magnitud de la diferencia (efecto bruto) en los test con dos muestras
#' @param n valor entero: tamano muestral cuando se indican los datos resumidos de una sola muestra
#' @param m  vector o valor real: vector de datos cuando se indica una muestra o la variable a analizar en el caso de muestras independientes. Media de la variable a analizar, previamente calculada, cuando se indica una sola muestra con la informacion resumida
#' @param n1 valor entero: tamano de la muestra 1 cuando se indican los datos resumidos de dos muestras independientes,
#' @param n2 valor entero: tamano de la muestra 2 cuando se indican los datos resumidos de dos muestras independientes
#' @param s valor real: desviacion tipica de la variable en el test con una muestra.
#' @param m1 vector o valor real: vector de datos de la primera muestra cuando se indican dos muestras apareadas. El contraste sera Media(m1)-Media(m2)=m0, o medias muestrales cuando se indican dos muestras independientes con las medidas de sintesis
#' @param m2 vector o valor real: vector de datos de la segunda muestra cuando se indican dos muestras apareadas. El contraste sera Media(m1)-Media(m2)=m0, o medias muestrales cuando se indican dos muestras independientes con las medidas de sintesis
#' @param s1 valor real: desviacion tipica de las muestras 1 para la comparacion de 2 muestras independientes especificadas por sus medidas de sintesis.
#' @param s2 valor real: desviacion tipica de las muestras 2 para la comparacion de 2 muestras independientes especificadas por sus medidas de sintesis.
#' @param par valor logico: si los tamanos de m1 y m2 son iguales se asumen muestras apareadas, pero si par=FALSE se asumen independientes
#' @param grupos variable binaria o factor con dos niveles: variable de agrupacion en la comparacion de dos muestras independientes especificadas con los datos individuales de cada caso
#' @param vac valor logico: TRUE=se trata de una variable aleatoria continua; FALSE= la variable es discreta y se aplica cpc_ Por defecto = TRUE.
#' @param conf valor real < 1: nivel de confianza para la elaboracion del IC para la estimacion de la media o del tamaño del efecto
#' @param alfa valor real < 1: error alfa (parametro alternativamente al nivel de confianza, en tanto por uno). Por defecto =.05.
#' @param potencia valor real <1: potencia deseada para estudiar la fiabilidad de la decision por la hipotesis nula y el tamaño de muestra.
#' @param beta valor real: error de tipo II, parametro alternativo a la potencia
#' @param delta valor real: tamano del efecto a detectar, es decir, a declarar significativo con la potencia deseada
#' @param decs valor entero: precision decimal para la salida de resultados. Por defecto = 4.
#' @param grf valor logico: Si TRUE/FALSE se genera/omite la salida grafica
#' @return Informe con medidas descriptivas, test de normalidad (si se aportan datos individuales), test del cociente de varianzas de Fisher (si procede), t-test con estimacion del tamano del efecto bruto, estudio de la potencia y estimacion de tamano muestral.
#' @importFrom stats cor sd var shapiro.test na.omit
#' @export testt
#' @examples
#' # [A] Test con una muestra
#' # [A.1] Con los datos individuales
#' datos<-c(76,54,12,47,13,15,25,14,19,32,7)
#' testt(m=datos, m0=25)
#' testt(m=datos, m0=29,delta=3)
#' testt(m=datos, m0=29,delta=3,vac=FALSE)
#'
#' # [A.2] Con informacion muestral sintetizada
#' testt(m=37, s=5, n=158, m0=25)
#' testt(m=37, s=7, n=158, m0=36,delta=3,potencia=0.95)
#'
#' # [B] Test con dos muestras independientes
#' # [B.1] 2 muestras independientes con datos individuales
#' sexo<-c( 1,2,2,2,1,1,2,2,1,1)
#' peso<-c(54,64,76,84,45,74,76,95,63,62)
#' testt(m=peso, grupos=sexo)
#' fuma<-c("si","no","si","no","no","no","no","si","no","si")
#' testt(m=peso, grupos=fuma,delta=5,potencia=0.95)
#'
#' grupo1<-c(12.5,7.4,8.3,4.6,5.1,7.8,9.2,4.6)
#' grupo2<-c(8.7,14.8,13.5,16.1,7.1,19.2,21.5,22.4,18.7)
#' testt(m1=grupo1, m2=grupo2)
#'
#' # [B.2] 2 muestras independientes con datos sintetizados
#' testt(n1=123, m1=25, s1=6, n2=87, m2=20, s2=8)
#'
#' # [C] Test con dos muestras relacionadas
#' # [C.1] 2 muestras apareadas con datos individuales
#' pre_tratamiento<-c(3.2,4.5,1.7,2.6,1.7,4.3,2.1,3.8,4.9,5.1,NA)
#' post_tratamiento<-c(3.8,4.1,2.2,3.1,2.7,9.3,7.9,3.1,5.7,5.3,NA)
#' testt(m1=pre_tratamiento, m2=post_tratamiento, par=TRUE)
#' testt(m1=pre_tratamiento, m2=post_tratamiento, par=TRUE, delta=2,m0=0.1)
#' testt(m1=pre_tratamiento, m2=post_tratamiento, par=TRUE, delta=2,m0=0.1, vac=FALSE)
#'
#' # [C.2] 2 muestras apareadas con datos sintetizados (es el test con una sola
#' #       muestra, habitualmente m0=0)
#' testt(m=0.65, s=1.2, n=17)
#'
#' # [D] Estudio de la fiabilidad por H0 y estimacion del tamano de muestra (basta
#' #    anadir el parametro delta a cualquiera de las opciones anteriores)
#' testt(m=peso, grupos=fuma, delta=5, potencia=0.95)
#' testt(m1=pre_tratamiento, m2=post_tratamiento, delta=0.5, potencia=0.85)
#' testt(m1=pre_tratamiento, m2=post_tratamiento, delta=0.5, beta=0.15)
#'

###################
#t-test
###################
testt<-function(m=NULL,m1=NULL,m2=NULL,n=0,n1=0,n2=0,s=0,s1=0,s2=0,par=FALSE,m0=0, grupos=NULL,conf= 0.95, vac=TRUE,alfa=.05, delta=0, potencia=0.8,beta=0.2, decs=3,grf=TRUE)
{
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# cabecera
  {
  err<-FALSE
  caso<-99 # caso indefinido
  #tol.txt<-paste(10^-decs)
  tab="  "
  tab2=paste(tab,tab,sep="")

  # CODIFICACION DE CONDICIONES DE ENTRADA
  par_alfa<-epairsget(p=alfa,q=conf,pdefault=0.05)
  if(par_alfa[[1]][1]){
    alfa<-par_alfa[[2]][1]
    conf<-par_alfa[[2]][2]
  } else {stop("Valor de alfa o de conf incongruente")}

  par_beta<-epairsget(p=beta,q=potencia,pdefault=0.20)
  if(par_beta[[1]][1]){
    beta<-par_beta[[2]][1]
    potencia<-par_beta[[2]][2]
  } else {stop("Valor de beta o de potencia incongruente")}

  if(n<0 || n1<0 || n2<0 || s1<0 || s2<0 || decs<0){stop("Valores negativos no procedentes")}


  if(delta<0) { delta<-abs(delta);
                wmsge(wtext=paste("Se considera el tama\u00f1o del efecto como una magnitud positiva: \u03b4 =",delta,sep=""))}
  }# end cabecera
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# IDENTIFICACION DEL PROBLEMA
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  done<-FALSE

  # Test con una muestra - - - - - - - - - - - - - -
  if(!done && !is.null(m) && is.null(grupos)){
      if(length(m)==1) {
        if(n>0 && s>0){
          caso<-10                                                              # CASO 10 = 1 muestra con datos de sintesis
          done<-TRUE
        }else{
          stop("Solo hay un valor en m y no se indica n y s")
        }
      }else{
          caso<-11                                                              #CASO 11  = 1 muestras como vector m
          done<-TRUE
      }
  }

  # Test con dos muestras - - - - - -
  if(!done && !is.null(grupos)){
    if(!is.null(m)){
      if(length(m)>1) {
        if(length(m)==length(grupos)){
          caso<-21                                                              # CASO 21 = 2 muestras independientes: vector m y agrupacion en grupos
          done<-TRUE
        }else{
          stop("Las variables indicadas en m y grupos deben tener la misma longitud")
        }
      } else stop("La variable m tiene un solo dato y se ha indicado una variable de agrupaci\u00f3n en grupos")
    }
  }

  if(!done && !is.null(m1) && !is.null(m2)) {
    if(!done && s1>0 && n1>0 && s2>0 && n2>0 && length(m1)==1 && length(m2)==1) {
      caso <- 23
      done<-TRUE
    }                                                                           #CASO 23 = 2 muestras indeps. medidaS sintetizadas
    if(!done && length(m1)>1 && length(m2)>1){
      if(par){ # 2 muestras apareadas en m1 y m2
        if(length(m1)==length(m2)) {
          caso <- 20                                                            # CASO 20 =  2 MUESTRAS APAREADAS
          done<-TRUE
        } else {
          stop(paste("Se ha indicado par=TRUE, pero los vectores ",deparse(substitute(m1))," y ",deparse(substitute(m2))," no tienen la misma longitud", sep=""))
        }
      } else {
        cat(paste("\n[!] Las muestras ",deparse(substitute(m1))," y ",deparse(substitute(m2))," tienen el mismo tama\u00f1o, pero no se ha indicado",sep=""),"\n")
        cat(paste("    par=TRUE. Se asume que las muestras son independientes."),"\n")
        caso<-22                                                                # CASO 22 = 2 MUESTRAS INDEPS. en m1 y m2
        done<-TRUE
      }
    }
  }

 # ***************************************
  if (caso==99) stop("No se puede identificar el problema con los par\u00e1metros introducidos")

  # # # # # # # # # # # # # ## # # # # # #
  # Test con una muestra
  # # # # # # # # # # # # # ## # # # # # #
  if (caso==10 || caso==11) {# test con 1 Muestra

       if(length(m)>1) varname<-deparse(substitute(m)) else varname<-"Muestra"
       testt1(m=m,n=n,s=s,m0=m0, vac=vac,vname=varname, alfa=alfa, delta=delta, beta=beta, decs=decs, grf=grf)
  }

  # # # # # # # # # # # # # ## # # # # # #
  # Test con dos muestras apareadas
  # # # # # # # # # # # # # ## # # # # # #
  if (caso==20)
  {  vnames<-c(deparse(substitute(m1)), deparse(substitute(m2)))
     testt2p(m1=m1,m2=m2,m0=m0, vac=vac,vnames=vnames, alfa=alfa, delta=delta,beta=beta,decs=decs,grf=grf)
  } # end if caso == 20


  # # # # # # # # # # # # # ## # # # # # #
  # Test dos muestras independientes
  # # # # # # # # # # # # # ## # # # # # #
  if (caso==21){# CASO 21 = 2 muestras independientes: vector m y agrupacion en grupos
    varnames<-c(deparse(substitute(m)), deparse(substitute(grupos)))
    test2i(m=m, grupos=grupos,m0=m0, vac=vac,alfa=alfa, delta=delta,beta=beta,vnames=varnames, decs=decs,grf=grf,caso=caso)
  }

  if (caso==22){# CASO 22 = 2 MUESTRAS INDEPS. en m1 y m2
    varnames<-c(deparse(substitute(m1)), deparse(substitute(m2)))
    test2i(m1=m1,m2=m2,m0=m0, vac=vac,alfa=alfa, delta=delta,beta=beta,vnames=varnames, decs=decs,grf=grf,caso=caso)
  }

  if (caso==23){# CASO 23 = 2 muestras indeps. medidaS sintetizadas
    varnames<-c("Muestra 1","Muestra 2") # c(deparse(substitute(m1)), deparse(substitute(m2)))

    test2i(n1=n1,m1=m1,s1=s1,n2=n2,m2=m2,s2=s2,vac=vac,m0=m0,alfa=alfa, delta=delta,beta=beta,vnames=varnames, decs=decs,grf=grf,caso=caso)
  }


}

