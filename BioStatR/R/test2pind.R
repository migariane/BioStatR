#' @noRd
#' @importFrom stats  na.omit
test2p<-function(x1=0,n1=0,x2=0,n2=0,alfa=0.05,delta=0,beta=0.20,fvar=NULL,cvar=NULL,decs=3){
  tab="  "
  conf<-1-alfa
  potencia<-1-beta
  byrow<-TRUE

  # Etiquetas
  p1_txt <-paste("p",greek("","1"),sep="")
  p2_txt <-paste("p",greek("","2"),sep="")
  q1_txt <-paste("q",greek("","1"),sep="")
  q2_txt <-paste("q",greek("","2"),sep="")
  pi1_txt<-greek("p","1")
  pi2_txt<-greek("p","2")

  okchk<-"\u2713"
  nochk<-"x"  #"\u0078"

  p1=x1/n1
  q1=(n1-x1)/n1
  ic1=icpwaldajustado(x1,n1,conf=conf)

  p2=x2/n2
  q2=(n2-x2)/n2
  ic2=icpwaldajustado(x2,n2,conf=conf)

  n=n1+n2
  p=(x1+x2)/n
  q=1-p
  a1=x1+x2
  a2=(n1-x1)+(n2-x2)

  # Lateralidad
  if(p1>=p2){
    difmodo=12
    difp<-p1-p2
    dsg<-">"
    diftxt=paste("\u03c0\u2081-\u03c0\u2082",sep="")
    difptxt=paste("p\u2081-p\u2082",sep="")
  } else {
    difmodo=21
    difp<-p2-p1
    dsg<-"<"
    diftxt=paste("\u03c0\u2082-\u03c0\u2081",sep="")
    difptxt=paste("p\u2082-p\u2081",sep="")
  }
  # criterios H1
  H1_1c<-paste(pi1_txt,dsg,pi2_txt,sep="")
  H1_2c<-paste(pi1_txt,"\u2260",pi2_txt,sep="")

  # minima frec esperada (para cdv)
  E<-min(a1,a2)*min(n1,n2)/n
  E_txt <-roundf(E,1)
  E_head<-paste("E=",E_txt,sep="")

  # TESTS CONDICIONADOS
  # 1) Test de Fisher - - -
  # bilateral
  fisher_2c<-fisher.test(matrix(c(x1,n1-x1,x2,n2-x2),nrow=2,byrow=TRUE))
  pfisher_2c    <-fisher_2c$p.value
  pfisher_2c_txt<-ptxt(pval=pfisher_2c,decs=decs,eq=FALSE)
  # unilateral
  if(difmodo==12) fisher_1c <-fisher.test(matrix(c(x1,n1-x1,x2,n2-x2),nrow=2,byrow=TRUE),alternative="g")
  else            fisher_1c <-fisher.test(matrix(c(x1,n1-x1,x2,n2-x2),nrow=2,byrow=TRUE),alternative="l")
  pfisher_1c    <-fisher_1c$p.value
  pfisher_1c_txt<-ptxt(pval=pfisher_1c,decs=decs,eq=FALSE)
  val_f1_txt <- okchk
  val_f2_txt <- okchk

  # 2) Aprox. a la normal
  cpcc<- (n/(n1*n2))/2
  numzexp<-(abs(p1-p2))-cpcc
  if(numzexp<0){numzexp=0}
  denzexp<-sqrt(p*q*n/(n1*n2))
  zexp_cond<-numzexp/denzexp
  pcond_2c    <-2*(1-pnorm(zexp_cond))
  pcond_2c_txt<-ptxt(pval=pcond_2c,decs=decs,eq=FALSE)
  pcond_1c    <-  (1-pnorm(zexp_cond))
  pcond_1c_txt<-ptxt(pval=pcond_1c,decs=decs,eq=FALSE)
  # cdv
  Ev_c=20.7
  if(n<=500){Ev_c=8.8}
  val_c2_txt<-okchk
  if(E >= Ev_c){
    val_c1_txt<-okchk
    val_c_txt=paste("E>",Ev_c,sep="")
  } else {
    val_c_txt=paste("E<",Ev_c,sep="")
    val_c1_txt<-nochk
  }


  # TEST INCONDICIONADO - aprox a la normal
  # Validez
  Ev_i=14.9
  if(n<=500){Ev_i=7.7}
  if(E >= Ev_i){
    val_i1_txt<-okchk
    val_i2_txt<-okchk
    val_i_txt=paste("E>",Ev_i,sep="")
  } else {
    val_i1_txt<-nochk
    val_i2_txt<-nochk
    val_i_txt=paste("E<",Ev_i,sep="")
  }

  cpci=1/(n1*n2)
  if(n1==n2){cpci<-2*cpci}
  numzexp<-(abs(p1-p2))-cpci
  if(numzexp<0){numzexp<-0}
  denzexp<-sqrt(p*q*n/(n1*n2))
  zexp_incond <- numzexp/denzexp
  pincond_2c    <-2*(1-pnorm(zexp_incond))
  pincond_2c_txt<-ptxt(pval=pincond_2c,decs=decs,eq=FALSE)
  pincond_1c    <-(1-pnorm(zexp_incond))
  pincond_1c_txt<-ptxt(pval=pincond_1c,decs=decs,eq=FALSE)


  # x x x x x x x x x x x

  fvarname  <-fvar[1]
  fvarlevel1<-fvar[2]
  fvarlevel2<-fvar[3]

  cvarname  <-cvar[1]
  cvarlevel1<-cvar[2]
  cvarlevel2<-cvar[3]


  showtitle(paste("Estimaci\u00f3n para ",cvarname," = '",cvarlevel1,"' \n",sep=""),lev=2)
  cat(paste(tab,fvarname," = ",fvarlevel1,sep="","\n"))
  txt1=paste(p1_txt," = ",roundf(p1,decs),"  (",q1_txt,"=1-",p1_txt," = ",roundf(1-p1,decs),")",sep=""," \n")
  cat(tab,txt1,sep="")
  txt<-txtic(ic=ic1,alfa=alfa, param=pi1_txt)
  cat(tab,txt,"\n",sep="")
  cat("\n")
  cat(paste(tab,fvarname," = ",fvarlevel2,sep="","\n"))
  txt2=paste(p2_txt," = ",roundf(p2,decs),"  (",q2_txt,"=1-",p2_txt," = ",roundf(1-p2,decs),")",sep="","\n")
  cat(tab,txt2,sep="")
  txt<-txtic(ic=ic2,alfa=alfa, param=pi2_txt)
  cat(tab,txt,"\n",sep="")
  wfoot("* Intervalos obtenidos por el m\u00e9todo de Wald ajustado (Agresti-Coull)")
  cat("\n")


  ifelse(difmodo==12,txtdif<-"\u03c0\u2081-\u03c0\u2082=0",txtdif<-"\u03c0\u2082-\u03c0\u2081=0")
  showtitle(txt=paste("Test de homogeneidad para contrastar Ho:\u03c0\u2081=\u03c0\u2082 (",txtdif,")",sep=""),lev=2)

  txt1=paste("p|",greek("H",0)," = ",roundf(p,decs),"  (q|",greek("H",0)," = ",roundf(1-p,decs),")",sep=""," \n")
  cat(tab,txt1,sep="")


  cat(tab,"M\u00e9todo: \n",sep="")
  metodo<-c(
    paste(tab,"Condicionado exacto (Fisher)",sep=""),
    paste(tab,"Condicionado adn (Yates) ",sep=""),
    paste(tab,"Incondicionado adn",sep=""))
  Zexp<-c(
    "-",
    roundf(zexp_cond,decs),
    roundf(zexp_incond,decs))

  cpc<-c(
    "-",
    roundf(cpcc,decs,lt=TRUE),
    roundf(cpci,decs,lt=TRUE))

  pval2<-c(
    pfisher_2c_txt,
    pcond_2c_txt,
    pincond_2c_txt)

  val2c<-c(
   val_f2_txt,
   val_c2_txt,
   val_i2_txt)

  pval1<-c(
    pfisher_1c_txt,
    pcond_1c_txt,
    pincond_1c_txt)

  val1c<-c(
    val_f1_txt,
    val_c1_txt,
    val_i1_txt)

  validez<-c(
    "-",
    val_c_txt,
    val_i_txt
  )

  tabla<-data.frame(
    Zexp,
    cpc,
    cdv=validez,
    p.bilat=pval2,
    b=val2c,
    p.unilat=pval1,
    u=val1c)
  row.names(tabla)<-metodo
  print(tabla)

  wfoot(paste("*   Alternativas: bilateral ",greek("H",1),":",H1_2c, "; unilateral ",greek("H",1),":",H1_1c,sep=""))
  wfoot(paste("**  E=",E_txt," es la frecuencia m\u00ednima esperada bajo ",greek("H",0),sep=""), line=FALSE)
  wfoot(paste("*** adn = aproximaci\u00f3n a la distribuci\u00f3n normal",sep=""), line=FALSE)
  cat("\n")


  showtitle(txt=paste("Estimaci\u00f3n de la diferencia ",difptxt,"=",roundf(difp,decs),sep=""),lev=2)
  #cat("\n")
  icn<-ic2p_normal(x1=x1,n1=n1,x2=x2,n2=n2,alfa=alfa)
  ica<-ic2p_agresti(x1=x1,n1=n1,x2=x2,n2=n2,alfa=alfa)
  if(x1>5 && x2>5 && (n1-x1>5) && (n2-x2>5) ){
    icvalido<-"(v\u00e1lido)"
  }else {
    icvalido<-"(NO es v\u00e1lido)"
  }
  cat(paste(tab,"Aproximaci\u00f3n a la distribuci\u00f3n normal ",icvalido,":",sep=""),"\n")
  cat(paste(tab,txtic(ic=icn, alfa=alfa, param=diftxt,decs=decs),sep=""),"\n")
  cat("\n")
  cat(paste(tab,"M\u00e9todo de Agresti-Caffo:",sep=""),"\n")
  cat(paste(tab,txtic(ic=ica, alfa=alfa, param=diftxt,decs=decs),sep=""),"\n")
  cat("\n")

  if(delta>0){
    # Estudio de la potencia
    #cat("\n")
    if(pfisher_2c<=0.05){
      showtitle(txt="Estudio de la potencia",lev=2)
      cat(paste(tab,"El test es significativo, se omite el an\u00e1lisis de la potencia.",sep=""),"\n")
    }else{
      txt<-paste("Estudio de la potencia: \u03b4 = ",delta," -> [",-delta,", ",delta,"], potencia \u03b8 = ",potencia*100,"%",sep="")
      showtitle(txt=txt,lev=2)
      ica2b<-ic2p_agresti(x1=x1,n1=n1,x2=x2,n2=n2,alfa=2*beta)
      icbtxt<-txtic(ic=ica2b,alfa=(2*beta),param=diftxt,decs=decs)
      cat(paste(tab,icbtxt," (m\u00e9todo de Agresti-Caffo)",sep=""),"\n")
      get_diagram_ic(ic=ica2b,id=c(-delta,delta), potencia = potencia,m0=0,param="\u03bc",eco=TRUE)
    }

    # Tamano muestral
     n2p(x1=x1,n1=n1,x2=x2,n2=n2,alfa=alfa,beta=beta,delta=delta,decs=decs,context=TRUE)
  }
cat("\n")

}



# # # # tamano de muestra # # #
#'@noRd
n2p<-function(x1=0,n1=0,x2=0,n2=0,alfa=0.05,beta=0.20,delta=0,decs=3,context=TRUE){
  za<-qnorm(1-(alfa/2))
  z2a<-qnorm(1-(alfa))
  z2b<-qnorm(1-beta)
  tab<-"  "
  ifelse(context,lev<-2, lev<-1)
  showtitle("Tama\u00f1o de muestra para detectar |\u03c0\u2081-\u03c0\u2082|=\u03b4",lev = lev)
  cat(paste(tab,"Diferencia a detectar: \u03b4 = ",roundf(delta,decs),sep=""),"\n" )
  cat(paste(tab,"Error de tipo I: \u03b1 = ",roundf(alfa,decs),sep=""),"\n")
  cat(paste(tab,"Potencia: \u03b8 = ",roundf(1-beta,decs),sep=""),"\n")
  cat(paste(tab,"Se considera n",greek("",1),"=n",greek("",2)," = n",sep=""),"\n")

  #sin informacion
  nsin<-trunc(0.5*(((za+z2b*sqrt(1-delta^2)))/delta)^2)+1
  nsincpc<-trunc(0.25*nsin*(1+sqrt(1+(4/(nsin*delta))))^2)+1
  cat("\n")
  cat(paste(tab,"Estimaci\u00f3n (n) para una varianza m\u00e1xima:",sep=""),"\n")
  cat(paste(tab,"- n (sin cpc) \u2a7e " ,nsin,sep=""),"\n")
  cat(paste(tab,"- n (con cpc) \u2a7e " ,nsincpc,sep=""),"\n")

  #con informacion
  if(x1>0 && x2>0 && n1>0 && n2>0){
    delta2<-delta/2
    ic1<-icpexact(x=x1,n=n1,alfa=alfa)
    ic2<-icpexact(x=x2,n=n2,alfa=alfa)
    lim<-ic2p_maxvar(ic1=ic1,ic2=ic2)
    if(lim==0) {
      ncon<-nsin
      nconcpc<-nsincpc
    } else {
      if(lim==1){ #tomar lim superior
        pp1<-ic1[[2]]
        pp2<-ic2[[2]]
      }else{ # tomar lim inferior
        pp1<-ic1[[1]]
        pp2<-ic2[[1]]
      }
      qq1<-1-pp1
      qq2<-1-pp2
      pp<-(pp1+pp2)/2
      qq<-1-pp

      ncon<-trunc(((za*sqrt(2*pp*qq) + z2b*sqrt((pp1*qq1)+(pp2*qq2)))/delta)^2)+1
      nconcpc<-trunc((ncon/4)*(1+sqrt(1+(4/(ncon*delta))))^2)+1
      cat("\n")
      cat(paste(tab,"Aproximaci\u00f3n (n') a partir de la informaci\u00f3n muestral:",sep=""),"\n")
      cat(paste(tab,"- n' (sin cpc) \u2a7e " ,ncon,sep=""),"\n")
      cat(paste(tab,"- n' (con cpc) \u2a7e " ,nconcpc,sep=""),"\n")

      wfoot(paste("*  La aproximaci\u00f3n n' puede variar en funci\u00f3n del criterio usado.",sep=""))
      wfoot(paste("** Si se desea la estimaci\u00f3n de n para el test unilateral, repetir",sep=""), line=FALSE)
      wfoot(paste("   el an\u00e1lisis indicando alfa=",roundf(alfa*2,decs)," y asumir los n obtenidos.",sep=""), line=FALSE)

    }
  }
}


# IC diferencia de proporciones # # # # # # # #

#' @noRd
ic2p_normal<-function(x1=0,n1=0,x2=0,n2=0,alfa=0.05){

 za<- qnorm((1-alfa/2))
 # IC aprox normal
 p1<-x1/n1
 q1<-1-p1

 p2<-x2/n2
 q2<-1-p2

 difp<-p1-p2

 se_p1p2<-sqrt((p1*q1/n1) +(p2*q2/n2))
 cpc_p1p2<-((n1+n2)/(2*n1*n2))
 d_p1p2<-za*se_p1p2+cpc_p1p2

 linf<-difp-d_p1p2
 if(linf< -1) linf <- -1
 lsup<-difp+d_p1p2
 if(lsup>  1) lsup <-  1

 ic_normal<-c(linf,lsup)
 return(ic_normal)

}
#' @noRd
ic2p_agresti<-function(x1=0,n1=0,x2=0,n2=0,alfa=0.05){
  #IC de Agresti & Caffo (2000)
  za<- qnorm((1-alfa/2))

  # IC aprox normal
  inc<-0.25*za^2
  x1<-x1+inc
  n1<-n1+2*inc
  x2<-x2+inc
  n2<-n2+2*inc

  p1<-x1/n1
  q1<-1-p1

  p2<-x2/n2
  q2<-1-p2

  difp<-p1-p2

  se_p1p2<-sqrt((p1*q1/n1) +(p2*q2/n2))
  d_p1p2<-za*se_p1p2

  linf<-difp-d_p1p2
  if(linf< -1) linf <- -1
  lsup<-difp+d_p1p2
  if(lsup>  1) lsup <-  1

  ic_agresti<-c(linf,lsup )
  return(ic_agresti)
  #ejemplo: ic2p_agresti(x1=5,n1=6,x2=4,n2=13)
}
#' @noRd
ic2p_martin<-function(x1=0,n1=0,x2=0,n2=0,alfa=0.05){
  #IC de Martin & Herranz (2003). pag 260 Bioestadistica
  za <-qnorm((1-alfa/2))
  za2<-za^2

  a1<-x1+x2
  a2<-(n1-x1) + (n2-x2)
  n<-n1+n2

  p1<-x1/n1
  q1<-1-p1

  p2<-x2/n2
  q2<-1-p2

  d<-p1-p2
  c<-n/(2*n1*n2)

  A<-  (za2 *(((n2-n1)^2 +n1*n2))) +n*n1*n2
  B1<-  za2 *(n2-n1)*(a2-a1) - 2*n *n1*n2*(d-c)
  B2<-  za2 *(n2-n1)*(a2-a1) - 2*n *n1*n2*(d+c)

  C1<- n*n1*n2*(d-c)^2 -za2*a1*a2
  C2<- n*n1*n2*(d+c)^2 -za2*a1*a2

  linf<- (-B1/(2*A)) - sqrt( (B1/(2*A))^2 - C1/A )
  lsup<- (-B2/(2*A)) + sqrt( (B2/(2*A))^2 - C2/A )

  if(linf< -1) linf <- -1
  if(lsup>  1) lsup <-  1

  ic_martin<-c(linf, lsup)
  return(ic_martin)
  # ejemplo ic2p_martin(x1=5,n1=6,x2=4,n2=13)
}
# ic2p_normal(x1=5,n1=6,x2=4,n2=13)
# ic2p_agresti(x1=5,n1=6,x2=4,n2=13)

