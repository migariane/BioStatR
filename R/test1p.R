#' @noRd
test1p<-function(x=NULL,n=0,x0=NA,p0=0.5,alfa=0.05,delta=0,beta=0.20,decs=3, msge=""){

  tab<-"  "
  tab2<-paste(tab,tab,sep="")
  conf<-1-alfa
  potencia<-1-beta
 # w_txt<-""

  if(length(x)>1){ #x es un vector
    if(n>0) warning("se ha indicado un vector de datos y un tama\u00f1o muestral n. Se ignora este \u00faltimo.")
    getxn(m=x,x0=x0,eco=FALSE)->xn
    w_txt<-xn[[2]][1]
    if(xn[[3]][1]==1) stop(w_txt)
    x<-xn[[1]][1]
    n<-xn[[1]][2]
  }

  if(n<=0) stop("Error: tama\u00f1o de muestra incorrecto")
  if(x>=n) stop("Error: se ha especificado x\u2a7en para el test con una muestra")
  if(x<=0) stop("Error: se ha especificado x\u2a7d0 para el test con una muestra")
  if(inrango(p0,rmin=0.01,rmax=0.99)==FALSE) stop("Error: se ha especificado un valor de prueba, p0, inadecuado")

  #test con una muestra
  showtitle("Test para contrastar una proporci\u00f3n binomial",lev=1)
  showtitle("Informaci\u00f3n muestral",lev=2)
  wmsge(msge,w=FALSE,sep=FALSE)
  cat(tab,"n = ",n,"\n",sep="");
  cat(tab,"x = ",x,"   n-x=",n-x,"\n",sep="");
  p<-x/n
  cat(tab,"p = ",roundf(p,decs),"; q = (1-p) = ",roundf(1-p,decs),"\n",sep="")
  cat("\n")
  cat("# Test Ho:\u03c0=",roundf(p0,decs),sep="","\n")

  cat(tab,"[1] M\u00e9todo exacto",sep="","\n")

  if(p<p0){
    fi<-(n-x)*p0/((x+1)*(1-p0))
    pfi<-pf(fi,2*(x+1),2*(n-x),lower.tail =FALSE)
    cola<--1 #izda <

    cola<-c(paste("\u03c0<",roundf(p0,decs),sep=""), paste("\u03c0\u2260",roundf(p0,decs),sep=""))
    fexp<-c(roundf(fi,decs),"-")
    pval<-c(ptxt(pfi,decs,eq=FALSE),ptxt(2*pfi,decs,eq=FALSE))
    tabla<-data.frame(H1=cola,Fexp=fexp,Valor.p=pval)
    rownames(tabla)<-c(paste(tab2,"Cola izquierda",sep=""),paste(tab2,"Bilateral",sep=""))

  } else {
    fd<-x*(1-p0)/((n-x+1)*p0)
    pfd<-pf(fd,2*(n-x+1),2*x,lower.tail =FALSE)
    cola<-1 #dcha >

    cola<-c(paste("\u03c0>",roundf(p0,decs),sep=""),paste("\u03c0\u2260",roundf(p0,decs),sep=""))
    fexp<-c(roundf(fd,decs),"-")
    pval<-c(ptxt(pfd,decs,eq=FALSE),ptxt(2*pfd,decs,eq=FALSE))
    tabla<-data.frame(H1=cola,Fexp=fexp,Valor.p=pval)
    rownames(tabla)<-c(paste(tab2,"Cola derecha",sep=""),paste(tab2,"Bilateral",sep=""))
  }
  print(tabla)
  cat("\n")
  ic_p<-icpexact(x,n,alfa=alfa); ic_metodo="Clooper-Pearson"
  ic_txt<-paste(conf*100,"%-IC(\u03c0) = (",roundf(ic_p[[1]][1],decs),", ",roundf(ic_p[[2]][1],decs),") ","(m\u00e9todo de ",ic_metodo,")",sep="")
  cat(tab2,ic_txt,sep="","\n")


  cat("\n")
  cat(tab,"[2] M\u00e9todo aproximado a la distribuci\u00f3n normal",sep="","\n")

  #test
  zexp<-(abs(x-n*p0)-0.5)/sqrt(n*p0*(1-p0))
  if(zexp<0) zexp<-0
  p_valor<-(1-pnorm(zexp))*2
  p_txt<-ptxt(p_valor,decs)
  cv_valor<-min(n*p0, n*(1-p0))
  ifelse(cv_valor>5, cv_txt<-paste("min(n\u03c0\u2080, n(1-\u03c0\u2080)) = ",roundf(cv_valor,1)," (>5, el m\u00e9todo es v\u00e1lido)",sep=""),
                     cv_txt<-paste("min(n\u03c0\u2080, n(1-\u03c0\u2080)) = ",roundf(cv_valor,1)," (\u2a7d5, ?el m\u00e9todo no es v\u00e1lido!)",sep=""))

  cat(tab2,"Validez: ",cv_txt,sep="","\n" )
  cat(tab2,"zexp = ",roundf(zexp,decs),",  p ",p_txt,sep="","\n")
  cat("\n")
  #cat(tab,"Estimaci\u00f3n \n",sep="")
  #cat(tab,"p = ",roundf(p,decs),"; ",sep="");
  if((x>=5) && ((n-x)>=5)) {ic_p<-icpwilson(x,n,alfa=alfa); ic_metodo="Wilson"} else {ic_p<-icpwaldajustado(x,n,alfa=alfa);ic_metodo="Wald ajustado"}
  ic_txt<-paste(conf*100,"%-IC(\u03c0) = (",roundf(ic_p[[1]][1],decs),", ",roundf(ic_p[[2]][1],decs),") ","(m\u00e9todo de ",ic_metodo,")",sep="")
  cat(tab2,ic_txt,sep="","\n")
  cat("\n")


  if(delta>0) n1p(p0=p0,alfa=alfa,delta=delta,beta=beta,decs=decs)

}

#test1p(x=2,n=10,p0=0.5)
#test1p(x=10,n=30,delta=0.01)
#test1p(x=c(1,2,3,4,5,6,5,4,5,4,3,2,3,4,5,5,1),x0=3,p0=0.5,delta=0.01)
#test1p(x=c(1,2,3,4,5,6,5,4,5,4,3,2,3,4,5,5,1),x0=3,p0=0.1,delta=0.01)

# # # # tama?o de muestra # # #
#'@noRd
n1p<-function(p0=0.5,alfa=0.05,delta=0,beta=0.20,decs=3,context=TRUE){
  tab="  "

  ifelse(context,lvl<-2,lvl<-1)
  showtitle("Tama\u00f1o de muestra para detectar |\u03c0-p\u2080|=\u03b4",lev=lvl)

  cat(tab,"p\u2080 = ",roundf(p0,decs),";  \u03b4 = ",delta,sep="","\n")
  cat(tab,"Casos (cpc = correcci\u00f3n por continuidad):",sep="","\n")
  za<-qnorm(1-(alfa/2))
  z2a<-qnorm(1-(alfa))
  z2b<-qnorm(1-beta)

  caso<-c(paste(tab,"1 Unilateral \u03c0<",roundf(p0,decs),sep=""),paste(tab,"2 Unilateral \u03c0>",roundf(p0,decs),sep=""),paste(tab,"3 Bilateral  \u03c0\u2260",roundf(p0,decs),sep=""))
  p1<-nearest(0.5,p0-delta,p0+delta,getlimit=TRUE)
  q1<-1-p1
  p1list<-c(paste(" ",roundf(p0-delta,decs)),paste(" ",roundf(p0+delta,decs)),paste(" ",roundf(p1,decs)))

  p1<-p0-delta
  nl<-  trunc(((z2a*sqrt(p0*(1-p0))+z2b*sqrt(p1*(1-p1)))/delta)^2)+1
  p1<-p0-delta
  nu<-  trunc(((z2a*sqrt(p0*(1-p0))+z2b*sqrt(p1*(1-p1)))/delta)^2)+1
  p1<-nearest(x=0.5,a=p0-delta,b=p0+delta,getlimit=TRUE)
  nb<-  trunc(((za*sqrt(p0*(1-p0))+z2b*sqrt(p1*(1-p1)))/delta)^2)+1
  nlist<-c(nl,nu,nb)

  ncpc<-c(
    trunc( (nl/4)*(1+sqrt(1+(2/(nl*delta))))^2)+1,
    trunc( (nu/4)*(1+sqrt(1+(2/(nu*delta))))^2)+1,
    trunc( (nb/4)*(1+sqrt(1+(2/(nb*delta))))^2)+1
  )

  #tablan<-data.frame(Alternativa=hipotesis,p1=p1list,n=nlist)
  tablan<-data.frame(p1=p1list,n=nlist, n_cpc=ncpc)
  row.names(tablan)<-caso
  print(tablan)
  cat("\n")

  # cat(tab,"(con 1 H\u2081:\u03c0\u2081<\u03c0\u2080; 2 H\u2081:\u03c0\u2081>\u03c0\u2080; 3 H\u2081:\u03c0\u2081\u2260\u03c0\u2080)",sep="")
}
