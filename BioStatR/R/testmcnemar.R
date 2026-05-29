#' @title test de homogeneidad de dos proporciones apareadas (McNemar)
#' @description Comparacion de dos proporciones apareadas. Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres.
#' @param n vector de enteros: frecuencias observadas con el formato n=c(n11,n12,n21,n22)
#' @param n11 entero: frecuencia observada en la fila 1 y columna 1
#' @param n12 entero: frecuencia observada en la fila 1 y columna 2
#' @param n21 entero: frecuencia observada en la fila 2 y columna 1
#' @param n22 entero: frecuencia observada en la fila 2 y columna 2
#' @param pre vector de observaciones en el pretest
#' @param post vector de observaciones en el posttest
#' @param fcat vector de cadenas de texto:  Nombres de fila
#' @param ccat vector de cadenas de texto:  Nombres de columna
#' @param alfa real en (0,1): Nivel de error de los intervalos (alternativa a conf) y error de tipo I para la determinacion de n
#' @param conf real en (0,1): Nivel de confianza de los intervalos (alternativa a alfa)
#' @param beta real en (0,1): Nivel de error de tipo II para la determinacion de n
#' @param potencia real en (0,1): Nivel de potencia deseada (alternativa a beta)
#' @param delta real: diferencia a detectar
#' @param decs entero: Numero de decimales en las salidas
#' @param lbls vector de los nombres de variable pre- y post- (llamada desde testp)
#' @return Informe analisis de dos proporciones apareadas mediante test de McNemar, intervalo de conf. para la diferencia y estimacion de tamano muestral
#' @importFrom stats  na.omit
#' @export testmcnemar
#' @examples
#'#
#'#Uso basico de la funcion introduciendo frecuencias observadas conforme a la tabla
#'#           post+ post-  |
#'# pre+      n11     n12  |
#'# pre-      n21     n22  |
#'#          -----------------
#'#                        |n
#'#
#'# Tabla de frecuencias. Son equivalentes las llamadas
#'testmcnemar(n=c(27,35,43,20))
#'testmcnemar(n11=27,n12=35,n21=43,n22=20)
#'
#'# Determinacion del tamano de muestra para declarar significativa una
#'# diferencia delta con potencia 1-beta y error alfa
#'testmcnemar(n11=27,n12=35,n21=43,n22=20,delta=0.05,alfa=0.05,beta=0.15)
#'
#'# Informacion en forma de vector
#'xpre<- c(1,1,2,2,1,2,1,2,2,2,1,1,2,2,1,1,2,2,1,1,1,1,1,1,1,NA,2)
#'xpost<-c(1,2,2,1,2,1,2,2,2,1,1,2,2,1,1,2,2,1,2,NA,1,1,1,2,2,2,1)
#'testmcnemar(xpre,xpost)
#'
#'# Determinacion del tamano muestral
#'testmcnemar(xpre,xpost,delta=0.05, beta=0.15)
#'

testmcnemar<-function(pre=NULL,post=NULL,n=NULL,n11=0, n12=0, n21=0, n22=0, fcat=c("+","-"), ccat=c("+","-"), alfa=0.05, conf=0.95, decs=4, delta=NULL, beta=0.20,potencia=1-beta,lbls=NULL)
{
  # funciones internas
  enic<-function(x=NULL,lic=NULL,uic=NULL){
    res<-FALSE
    if(x>=lic) {if(x<=uic) {res<-TRUE}}
    res
  }

  .IC_AgrestiMin<-function(n12=0,n21=0,alfa=0.05){
    za<-qnorm(1-(alfa/2))
    poam=(n12-n21)/(n+2)
    precisionam=za*sqrt( (n12+n21+1)-(((n12-n21)^2)/(n+2)) )/(n+2)
    icama<-poam-precisionam
    icamb<-poam+precisionam
    licam<-min(icama,icamb)
    uicam<-max(icama,icamb)
    result<-c(licam, uicam)
    return(result)
  }
  # - - - - - - - -

  if(!is.null(n)){
    if(length(n)==4){
      n11<-n[1]
      n12<-n[2]
      n21<-n[3]
      n22<-n[4]
    }
    n<-NULL
  }

  pre_name <-"pretest"
  post_name<-"posttest"

  # nombres de la variable
  if(!is.null(pre) && !is.null(post) && !is.null(lbls)){
    if(length(lbls)==2){
      pre_n <-lbls[[1]]
      post_n<-lbls[[2]]
    }else{
      pre_n <-deparse(substitute(pre))
      post_n<-deparse(substitute(post))
    }
    if(pre_n  != "x1") pre_name<-pre_n
    if(post_n != "x2") post_name<-post_n
  }

  indent<-2
  tab<-"  "
  tab2<-paste(tab,tab,sep="")
  napre<-0
  napost<-0
  pi<-"\u03c0"
  sub_11<-"\u2081\u2081"
  sub_12<-"\u2081\u2082"
  sub_21<-"\u2082\u2081"
  sub_22<-"\u2082\u2082"

  epairsget(p=alfa,q=conf, pmin=0.0001,pdefault=0.05)->par_alfa
  if(par_alfa[[1]][1]){
    alfa<-par_alfa[[2]][1]
    conf<-par_alfa[[2]][2]
  } else {stop("valor de alfa o de conf incongruente")}

  epairsget(p=beta,q=potencia, pmin=0.0001,pdefault=0.20)->par_beta
  if(par_beta[[1]][1]){
    beta<-par_beta[[2]][1]
    potencia<-par_beta[[2]][2]
  } else {stop("valor de beta o de potencia incongruente")}



  # preparar la tabla a partir de datos

  if(is.null(pre) || is.null(post))
  { if((n11+n12+n21+n22)==0) { stop("No hay datos v\u00e1lidos")}
  }
  else
  {
    lpre<-length(pre)
    lpost<-length(post)
    if(lpre!=lpost){
      stop("Los vectores de datos no tienen la misma longitud")
    } else {
      napre<-length(pre[is.na(pre)])
      napost<-length(post[is.na(post)])

      df<-data.frame(pre,post)
      df<-na.omit(df)
      tabla<-table(df$pre,df$post)
      n11<-tabla[[1,1]]
      n12<-tabla[[1,2]]
      n21<-tabla[[2,1]]
      n22<-tabla[[2,2]]
      if ((n11+n12+n21+n22)>0) {dataok<-TRUE}
      else { stop("No hay datos v\u00e1lidos")}
    }
  }

    n=n11+n12+n21+n22;
    ncondval<-n12+n21
    condval<-(ncondval>10)

  #test
  zexp=(abs(n12-n21)-0.5)/(sqrt(n12+n21))
  if (zexp<0){zexp<-0}
  pvalor<-pnorm(zexp,lower.tail = F)*2
  pvaltxt1<-ptxt(pvalor/2,decs=decs,eq=0)
  pvaltxt2<-ptxt(pvalor  ,decs=decs,eq=0)

  #IC
  confianza<-1-(alfa/2)
  za<-abs(qnorm(1-confianza))
  #Wald
  pow<-(n12-n21)/n
  precisionw<-(za*sqrt(   (n12+n21) - (((n12-n21)^2)/n ) +0.5   ))/n
  icwa<-pow-precisionw
  icwb<-pow+precisionw
  licw<-min(icwa,icwb)
  uicw<-max(icwa,icwb)

  # Agresti-Min
  icam<-.IC_AgrestiMin(n12=n12,n21=n21,alfa=alfa)
  poam=(n12-n21)/(n+2)
  licam<-icam[[1]]
  uicam<-icam[[2]]

  # IC individuales
  p12<-(n12+2)/(n+4)
  icp12<-icpwaldajustado(x=n12,n=n,conf=conf)
  licp12=min(icp12[[1]],icp12[[2]])
  uicp12=max(icp12[[1]],icp12[[2]])

  p21<-(n21+2)/(n+4)
  icp21<-icpwaldajustado(x=n21,n=n,conf=conf)
  licp21=min(icp21[[1]],icp21[[2]])
  uicp21=max(icp21[[1]],icp21[[2]])


  tab<-""
  for (i in 1:indent) tab<-paste(tab," ",sep="")
  cat("\n")
  showtitle("Inferencia con dos proporciones (muestras apareadas)",lev=1)
  showtitle(txt=paste("Frecuencias observadas ",pre_name," x ",post_name,sep=""),lev=2)
  if((napre+napost)>0){
    txt1=""; txt2<-""

    if(napre>0) {
      if(napre==1) {txt1<-paste("Detectado ",  napre," caso faltante en "  ,pre_name,sep="")}
      else         {txt1<-paste("Detectados ", napre," casos faltantes en ",pre_name,sep="")}
      }
    if(napost>0){
      if(napost==1) {txt2<-paste("Detectado ",  napost," caso faltante en "  ,post_name,sep="")}
      else          {txt2<-paste("Detectados ", napost," casos faltantes en ",post_name,sep="")}
    }
    if(txt1!="") cat(paste(tab,txt1,sep=""),"\n")
    if(txt2!="") cat(paste(tab,txt2,sep=""),"\n")
    cat(paste(tab,"Se eliminan las parejas con alg\u00fan valor faltante",sep=""),"\n")
    cat("\n")
  }
  trxcshow(x=matrix(c(n11,n12,n21,n22),nrow=2,byrow=TRUE),fcat=fcat,ccat=ccat,tipo="F",decs=0,eco=T,indent=indent+1)
  cat("\n")
  showtitle(paste("Proporciones observadas ",pre_name," x ",post_name,sep=""),lev=2)
  prop<-round(c(n11,n12,n21,n22)/(n11+n12+n21+n22),decs)
  trxcshow(x=matrix(prop,nrow=2,byrow=TRUE),fcat=fcat,ccat=ccat,tipo="F",decs=0,eco=T,indent=indent+1)


  showtitle(txt=paste("Test de McNemar: H\u2080:",pi,sub_12,"=",pi,sub_21,sep=""),lev=2)
  cat(paste(tab,"Validez: "))
    if (condval) {cat(paste("n",sub_12,"+n",sub_21," = ",ncondval," > 10 el test es v\u00e1lido",sep=""),"\n")}
    else {cat(paste("n",sub_11,"+n",sub_22," = ",ncondval," \u2a7d 10. El test NO es v\u00e1lido",sep=""),"\n")}
  cat(paste(tab,"Zexp = ",roundf(zexp,decs)) ,"\n")
  if(p12>p21) {desg<-">"} else {desg<-"<"}
  h1_2c<-paste(greek("H",1),":",greek("p",12),"\u2260",greek("p",21),sep="" )
  h1_1c<-paste(greek("H",1),":",greek("p",12),desg,greek("p",21),sep="" )
  pvals<-c(pvaltxt2,pvaltxt1)
  h1<-c(h1_2c,h1_1c)
  tabla<-data.frame(valor.p=pvals, Alternativa=h1)
  rownames(tabla)<-c(
    paste(tab2,"Bilateral",sep=""),
    paste(tab2,"Unilateral",sep=""))
  print(tabla)
  cat("\n")

  showtitle(txt="Test exacto de Fisher:",lev=2)
  cat(paste(tab,"H\u2080:",pi,sub_12,"=0.5 para n",sub_12," ~ B(n",sub_12,"+n",sub_21,", ",pi,sub_12,")",sep=""),"\n")
  test2ppar_exacto(n12=n12,n21=n21,alfa=alfa,decs=decs)
  wfoot(txt=paste("* Aqu\u00ed se alude a la probabilidad total de la discordancia, es decir que ",pi,sub_12,"+",pi,sub_21,"=1",sep="") )


  showtitle(paste("Estimaci\u00f3n de las proporciones individuales de discordancias \u03c0",sub_12," y \u03c0",sub_21," (m\u00e9todo de Wald ajustado)",sep=""),lev=2)
  cat(paste(tab," [1] p",sub_12," = ",roundf(p12,decs),", ",roundf(conf*100,0),"%-IC(\u03c0",sub_12,") = (",roundf(licp12,decs),", ", roundf(uicp12,decs),")",sep="" ),"\n")
  cat(paste(tab," [2] p",sub_21," = ",roundf(p21,decs),", ",roundf(conf*100,0),"%-IC(\u03c0",sub_21,") = (",roundf(licp21,decs),", ", roundf(uicp21,decs),")",sep="" ),"\n")


  cat("\n")

  showtitle("Intervalo de confianza para la diferencia de 2 proporciones apareadas",lev=2)

  cat(paste(tab,"[1] M\u00e9todo de Wald (cl\u00e1sico con cpc):\n" ))
  cat(paste(tab,"    Estimaci\u00f3n puntual de \u03c0",sub_12,"-\u03c0",sub_21," = ",roundf(pow,decs),sep=""),"\n")
  cat(paste(tab,"    Validez: ",sep=""))
  if ((n12+n21)>5) {cat(paste("n",sub_12,"+n",sub_21," = ",ncondval," > 5, el IC es v\u00e1lido",sep=""),"\n")}
  else {cat(paste("n",sub_11,"+n",sub_22," = ",ncondval," \u2a7d 5. El IC NO es v\u00e1lido",sep=""),"\n")}
  txtICw<-paste(tab,roundf(conf*100,0),"%-IC(\u03c0",sub_12,"-\u03c0",sub_21,") = (",roundf(licw,decs),", ",roundf(uicw,decs),")",sep="")
  cat(paste(tab,tab,txtICw,sep=""),"\n")

  cat("\n")
  cat(paste(tab,"[2] M\u00e9todo de Agresti-Min:\n" ))
  cat(paste(tab,"    Estimaci\u00f3n puntual de \u03c0",sub_12,"-\u03c0",sub_21," = ",roundf(poam,decs),sep=""),"\n")
  cat(paste(tab,"    Validez: siempre es v\u00e1lido",sep=""),"\n")
  txtICam<-paste(tab,roundf(conf*100,0),"%-IC(\u03c0",sub_12,"-\u03c0",sub_21,") = (",roundf(licam,decs),", ",roundf(uicam,decs),")",sep="")
  cat(paste(tab,tab,txtICam,sep=""),"\n")


  #Tamano muestral
  if(!is.null(delta)){ # estimacion del tamano de muestra

    #maximo a alcanzar
    ml<-0.5-delta/2
    mu<-0.5+delta/2
    status<- -1 #no encontrada la solucion


    #caso 1: la informacion muestral no aporta ventajas (los dos ic contienen a ml y mu)
          if( enic(ml,licp12,uicp12) & enic(mu,licp21,uicp21) ){p12<-ml;p21<-mu;status<-1}
    else {if( enic(mu,licp12,uicp12) & enic(ml,licp21,uicp21) ){p12<-mu;p21<-ml;status<-1} }

    #caso 2: los dos ic por debajo de ml
    if ((status<0) & ((uicp12<ml) & (uicp21<ml))){
       # obetener el mas proximo
         d12<-ml-uicp12
         d21<-ml-uicp21
         if(d12<d21){
           p12<-uicp12
           p21<-p12-delta
           if(enic(p21,licp21,uicp21)){status<-2}
           else{
             p21<-uicp21
             p12<-p21+delta
             if(enic(p12,licp12,uicp12)){status<-2}
           }
         }
         else {
           p21<-uicp21
           p12<-p21-delta
           if(enic(p12,licp12,uicp12)){status<-2}
           else{
             p12<-uicp12
             p21<-p12+delta
             if(enic(p21,licp21,uicp21)){status<-2}
           }
         }
    }
    #caso 3: un ic contiene a ml y el otro no
    if (status<0){
         if(enic(ml,licp12,uicp12)){
           p21<-uicp21
           if(enic(p21+delta,licp12,uicp12)){p12<-p21+delta;status<-3}
         }
         else{
           if(enic(ml,licp21,uicp21)){
             p12<-uicp12
             if(enic(p12+delta,licp21,uicp21)){p21<-p12+delta;status<-3}
         }
         }
    }

  if(delta>0){
    # Estudio de la potencia
    cat("\n")
    if(pvalor<=0.05){
      showtitle(txt="Estudio de la potencia",lev=2)
      cat(paste(tab,"El test es significativo, se omite el an\u00e1lisis de la potencia.",sep=""),"\n")
    }else{
      txt<-paste("Estudio de la potencia: \u03b4 = ",delta," -> \u00b1\u03b4=[",-delta,", ",delta,"], potencia \u03b8 = ",potencia,", ",greek("a")," = ",alfa,sep="")
      showtitle(txt=txt,lev=2)
      difp<-paste("\u03c0",sub_12,"-\u03c0",sub_21,sep="")
      icam2b<-.IC_AgrestiMin(n12=n12,n21=n21,alfa=2*beta)
      icam2btxt<-txtic(ic=c(icam2b[[1]],icam2b[[2]]),alfa=(2*beta),param=difp,decs=decs)
      cat(paste(tab,icam2btxt," (m\u00e9todo de Agresti-Min)",sep=""),"\n")
      get_diagram_ic(ic=icam2b,id=c(-delta,delta), potencia = potencia,m0=0,param="\u03c0",eco=TRUE)
    }


  #Tamanos de muestra
    z2b<-qnorm(1-beta)
    #n sin informacion
    nest1<-((za+z2b*sqrt(1-delta^2))/delta)^2
    nest1<-trunc(nest1+1)

    #n con informacion
    nest2<-((za*sqrt(p12+p21) + z2b*sqrt(p12+p21-delta^2) )/delta )^2
    nest2<-trunc(nest2+1)
    cat("\n")
    showtitle("Tama\u00f1o de muestra necesario para declarar significativa una diferencia \u03b4",lev=2)
    cat(paste(tab,"Diferencia: \u03b4 = ",delta,sep=""),"\n")
    cat(paste(tab,"Potencia: \u03b8 = ",roundf((1-beta),3),sep=""), "\n")
    cat(paste(tab,"Error de tipo I: \u03b1 = ",roundf(alfa,3),sep=""), "\n")
    cat("\n")
    cat(paste(tab,"[1] Estimaci\u00f3n para la varianza m\u00e1xima: "),"\n")
    if(n12<n21){
      cat(paste(tab,"    Asumiendo p",sub_12," = ",roundf(ml,decs),"   p",sub_21," = ",roundf(mu,decs),sep=""),"\n")
    }
    else{
      cat(paste(tab,"    Asumiendo p",sub_12," = ",roundf(mu,decs),"   p",sub_21," = ",roundf(ml,decs),sep=""),"\n")}
    cat(paste(tab,"    n \u2a7e ", nest1,sep="" ),"\n")
    cat("\n")
    if (status>0){
      cat(paste(tab,"[2] Basado en la informaci\u00f3n muestral: "),"\n")
      cat(paste(tab,"    Asumiendo p",sub_12," = ",roundf(p12,decs),"   p",sub_21," = ",roundf(p21,decs),sep=""),"\n")
      if(status==1) {ss<-" (la informaci\u00f3n muestral no aporta mejoras al caso anterior)"} else {ss<-""}
      cat(paste(tab,"    n \u2a7e ", nest2, ss,sep=""),"\n")

    } else {cat(paste(tab,"[2] Basado en la informaci\u00f3n muestral:",sep=""),"\n")
            cat(paste(tab,"   No se encuentran valores compatibles con las restricciones"),"\n")}
    alfa2<-alfa*2
    wfoot(paste("*Si se desea la estimaci\u00f3n de n para el test unilateral, repetir",sep=""))
    wfoot(paste(" el an\u00e1lisis indicando alfa=",roundf(alfa2,decs)," y asumir el n obtenido.",sep=""), line=FALSE)

   }
  }
}


#' @noRd
test2ppar_exacto<-function(n12=0,n21=0,alfa=0.05,decs=3){
  # Martin & Luna p266
  pi_12="\u03c0\u2081\u2082"
  conf=1-alfa
  tab="  "
  tab2=paste(tab,tab,sep="")

  p0<-0.5
  n<-(n12+n21)
  x<-n12
  p<-x/n

  if(x==0 || x==n) {
    p2c<-0
    p1c<-0
    fi<-(n)*p0/(1-p0)
    fd<-n*(1-p0)/(p0)
    pfi<-0
    pfd<-0
    pval<-0
    pfi2c<-0
    pfd2c<-0
    p2c<-0

  } else {
    fi<-(n-x)*p0/((x+1)*(1-p0))

    pfi<-pf(fi,2*(x+1),2*(n-x),lower.tail =FALSE)

    fd<-x*(1-p0)/((n-x+1)*p0)
    if (x==0) pfd<-0 else pfd<-pf(fd,2*(n-x+1),2*x,lower.tail =FALSE)


    #2 colas
    if (x>n*p0) {
      fd2c<-fd
      pfd2c<-pf(fd2c,2*(n-x+1),2*x,lower.tail =FALSE)

      x2<-n-x
      fi2c<-(n-x2)*p0/((x2+1)*(1-p0))
      pfi2c<-pf(fi2c,2*(x2+1),2*(n-x2),lower.tail =FALSE)
    } else{
      x2<-n-x
      fd2c<-x2*(1-p0)/((n-x2+1)*p0)
      pfd2c<-pf(fd2c,2*(n-x2+1),2*x,lower.tail =FALSE)

      fi2c<-fi
      pfi2c<-pf(fi2c,2*(x+1),2*(n-x),lower.tail =FALSE)
  }
  p2c<-pfi2c+pfd2c
  if (p2c>1) p2c<-1
  }

  if(p<p0){
    cola<-c(
      paste(greek("H",1),":",pi_12,"\u2260",p0,sep=""),
      paste(greek("H",1),":",pi_12,"<",p0,    sep="")
    )
    fexp<-c(roundf(fi,decs),"-")
    pval<-c(
      ptxt(p2c,decs,eq=FALSE),
      ptxt(pfi,decs,eq=FALSE)
    )
    #tabla<-data.frame(H1=cola,Fexp=fexp,Valor.p=pval)
     tabla<-data.frame(Valor.p=pval,Alternativa=cola)
     rownames(tabla)<-c(
       paste(tab2,"Bilateral",sep=""),
       paste(tab2,"Unilateral",sep="")
      )

  } else {
    cola<-c(
      paste(greek("H",1),":",pi_12,"\u2260",p0,sep=""),
      paste(greek("H",1),":",pi_12,">",p0,sep="")
    )
    fexp<-c(roundf(fd,decs),"-")
    pval<-c(
      ptxt(p2c,decs,eq=FALSE),
      ptxt(pfd,decs,eq=FALSE)
    )
    #tabla<-data.frame(H1=cola,Fexp=fexp,Valor.p=pval)
    tabla<-data.frame(Valor.p=pval,Alternativa=cola)
    rownames(tabla)<-c(
      paste(tab2,"Bilateral",sep=""),
      paste(tab2,"Unilateral",sep="")
      )
  }
  print(tabla)

}

