#' @noRd
#'
testt1<-function(m=NULL,n=0,s=0,m0=0, vac=TRUE,vname=NULL,alfa=.05, delta=0, beta=0.2, decs=3, grf=TRUE){
#  Test con una muestra
# # # # # # # # # # # # # # # #

# preparacion
  tab="  "
  tab2=paste(tab,tab,sep="")
  m_miss<-0
  potencia<-1-beta
  conf<-1-alfa

  if (!is.null(m)){
    if (length(m)>1) {caso<-11}       # CASO 11 = 1 muestra como vector
    else  {if (s>0 && n>0) {caso<-10}}
  }

  if(!is.null(vname)){ifelse(vname!="", varname<-vname,varname<-"")}

  if (caso==11){ # test con 1 Muestra como vector
    # test con una muestra pasada como vector

    m_len<-length(m)
    m<-m[!is.na(m)]
    m_n<-length(m)
    m_gl<-m_n-1
    m_miss<-m_len-m_n
    m_m<-mean(m)
    m_s<-sd(m)
    m_s2<-m_s^2
    # Test de normalidad
    sw <- shapiro.test(m)
    sw_w  <- roundf(sw[[1]],decs)
    sw_p  <- sw[[2]]
    sw_txt <-paste("W = ",sw_w,", ",ptxt(sw_p,decs=decs,eq=2),sep = "")
    sw <- shapiro.test(m)
    sw_w <- sw[[1]]
    sw_p <- sw[[2]]
    sw_txt<-paste("W = ",roundf(sw_w,decs),", gl = ",m_n,", ",ptxt(sw_p,decs,eq=2),sep="")
  } #end if caso = 11

  if (caso==10){ # test con 1 muestra como resumen
    err <- F
    m_m<-m
    m_n<-n
    m_gl<-m_n-1
    m_s<-s
    m_s2<-m_s^2
    # Test de normalidad
    sw<-NULL
  } #end if caso = 12


  # t-test - calculos
  cpc=0
  cpc_txt=""
  if(vac==FALSE){
    cpc=1/(2*m_n)
    cpc_txt<-ptxt(pval=cpc,decs=decs,eq=2,param="cpc")
  }

  t_num<-abs(abs(m_m-m0)-cpc)
  icm<-icm(m = m_m,n=m_n,s=m_s, vac=vac,alfa = alfa, eco=FALSE)
  t_den<-(m_s/sqrt(m_n))
  t_exp<-abs(t_num/t_den)
  t_gl<-m_n-1
  t_pval<-2*(1-pt(t_exp,t_gl))
  t_alfa<-qt((1-alfa/2),t_gl)
  t_lci<-(m_m-m0)-t_alfa*t_den
  t_uci<-(m_m-m0)+t_alfa*t_den
  t_2beta<-qt((1-beta),t_gl)
  t_lci2b<-m_m-m0-t_2beta*t_den
  t_uci2b<-m_m-m0+t_2beta*t_den
  if(delta>0){n_est<-m_s2*((t_alfa+t_2beta)/delta)^2}

  # Salida

  showtitle("t-Test con una muestra",lev=1)
  if (vac==FALSE){
    cat(paste(tab,"Se aplica correcci\u00f3n por continuidad (cpc) sobre el t-test y los intervalos",sep=""),"\n")

  }
  if(varname!="") showtitle(paste("Resumen de '",varname,"'",sep=""),lev=2)
  else            showtitle("Resumen de la muestra",lev=2)
  if(m_miss>0){
     cat(paste(tab2,"Datos faltantes:",m_miss,sep=""),"\n")  }
  cat(paste(tab2,"n = ",  roundf(m_n,decs),sep=""),"\n")
  cat(paste(tab2,"media = ",  roundf(m_m,decs),sep=""),"\n")
  cat(paste(tab2,"d.t. = ",  roundf(m_s,decs),sep=""),"\n")
  cat(paste(tab2,"sem = ",roundf(m_s/sqrt(m_n),decs),sep=""),"\n")
  if(vac==FALSE){
    cat(paste(tab2,cpc_txt,sep=""),"\n")}


  showtitle(paste("Estimaci\u00f3n de la media \u03bc:",sep=""),lev=2)
  txt<-paste((1-alfa)*100,"%-IC(\u03bc) = (",roundf(icm[[1]],decs),", ",roundf(icm[[2]],decs),")",sep="")
  cat(paste(tab,txt,sep=""),"\n")

  if(!is.null(sw))
  { showtitle("Test de normalidad de Shapiro-Wilk:",lev=2)
    cat(paste(tab,sw_txt, sep=""),"\n")
  }

  showtitle(paste("Test de Student para contrastar ",greek("H",0),":",greek("m"),"=",greek("m",0)," con ",greek("m",0),"=",roundf(m0,decs),sep=""),lev=2)
  #ptxt<-ifelse(t_pval<=p_tol, paste("p < ",p_tol,sep=""), paste("p = ",roundf(t_pval,decs),sep="") )
  ptxt<-ptxt(t_pval,decs=decs,eq=2)
  ptxt1cola<-ptxt(t_pval/2,decs=decs,eq=2)
  ifelse(m_m>=m0, s1cola<-">",s1cola<-"<")
  hbilateral<-paste (greek("H",1),":",greek("m"),"\u2260",greek("m",0),sep="")
  hunilateral<-paste(greek("H",1),":",greek("m"),s1cola,greek("m",0),sep="")
  cat(paste(tab,  "texp = ",roundf(t_exp,decs),", gl = ",t_gl,sep=""),"\n")
  cat(paste(tab2, ptxt," para la alternativa bilateral ",hbilateral,sep=""),"\n")
  cat(paste(tab2, ptxt1cola," para la alternativa unilateral ",hunilateral,sep=""),"\n")

  cat("\n")
  cat(paste(tab,"Estimaci\u00f3n del efecto bruto",sep=""),"\n")
  if(m0==0){ictxt<-paste(tab,roundf((1-alfa)*100,0),"%-IC(\u03bc) = (",roundf(t_lci,decs),", ",roundf(t_uci,decs),")",sep="")}
  else     {ictxt<-paste(tab,roundf((1-alfa)*100,0),"%-IC(\u03bc-",greek("m",0),") = (",roundf(t_lci,decs),", ",roundf(t_uci,decs),")",sep="")}
  cat(ictxt,"\n")

  #estudio de la potencia
  if (delta>0){

    if(t_pval<=0.05)
    { showtitle("Estudio de la potencia:",lev=2)
      cat(paste(tab,"El test es significativo, se omite el an\u00e1lisis de la potencia.",sep=""),"\n")}
    else
    {  txt<-paste("Estudio de la potencia: \u03b4 = ",delta," -> [",m0-delta,", ",m0+delta,"], potencia \u03b8 = ",potencia*100,"%",sep="")
    showtitle(txt,lev=2)
    if(m0==0){
      icbtxt<-paste("    ",roundf((1-2*beta)*100,0),"%-IC(\u03bc) = (",roundf(t_lci2b,decs),", ",roundf(t_uci2b,decs),")",sep="")
      cat(icbtxt,"\n")

      get_diagram_ic(ic=c(t_lci2b,t_uci2b),id=c(-delta,delta), potencia = potencia,m0=m0,param="\u03bc",eco=TRUE)

    } else{
      icbtxt<-paste("    ",roundf((1-2*beta)*100,0),"%-IC(\u03bc-",greek("m",0),") = (",roundf(t_lci2b,decs),", ",roundf(t_uci2b,decs),") con ",greek("m",0)," = ",roundf(m0,decs),sep="")
      cat(icbtxt,"\n")
      icbtxt<-paste("    ",roundf((1-2*beta)*100,0),"%-IC(\u03bc) = (",roundf(m0+t_lci2b,decs),", ",roundf(m0+t_uci2b,decs),")",sep="")
      cat(icbtxt,"\n")

      get_diagram_ic(c(t_lci2b+m0,t_uci2b+m0),c(m0-delta,m0+delta), potencia = potencia,m0=m0,param="\u03bc",eco=TRUE)
    }

    showtitle("Estimaci\u00f3n del tama\u00f1o muestral",lev=2)
    cat(paste(tab,"Efecto a detectar: \u03b4 = ",delta,sep=""),"\n")
    cat(paste(tab,"Potencia: \u03B8 = ",potencia,sep=""),"\n")
    cat(paste(tab,"Error de tipo I: ",greek("a")," = ",alfa,sep=""),"\n")

    cat(paste(tab,"n \u2a7e ",trunc(n_est+1,0)," casos",sep=""),"\n")
    cat("\n")
    }
  } #end estudio potencia

  if(grf){ # salida grafica
    if(caso==11){
      grpsggp(x=m, ggid=c(1),   lbls=c(paste("Variable (n=",m_n,")",sep=""),"casos"))
      grpsggp(x=m, ggid=c(5),   lbls=c("Media(sd)",paste("Muestra (n=",length(m),")",sep="") ) )
    }
    if(caso==12){
      grpsggp(x=m_m, se=m_s, ggid=c(9),   lbls=c("Media(sd)",paste("Muestra (n=",m_n,")",sep="") ) )
    }
  }
}

