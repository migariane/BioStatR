#' @noRd
#' @importFrom stats  na.omit
testt2p<-function(m1=NULL,m2=NULL,m0=0, vac=TRUE,vnames=NULL, alfa=.05, delta=0,beta=0.2,decs=3,grf=TRUE){
# # # # # # # # # # # # # # # # # # # # #
# Test con dos muestras apareadas
# # # # # # # # # # # # # # # # # # # # #

    tab<-"  "
    tab2<-paste(tab,tab,sep="")
    conf<-1-alfa
    potencia<-1-beta

   #nombres de las variables
    m1_name <-"pretest"
    m2_name <-"posttest"

    if(!is.null(vnames)){
      if(length(vnames)==2){
        m1_name <-vnames[[1]]
        m2_name<-vnames[[2]]
      }
    }
    names<-c(m1_name,m2_name)

   # muestras apareadas
    len_m1<-length(m1)
    len_m2<-length(m2)
    if(len_m1!=len_m2){stop("Las muestras no tienen la misma longitud")} #<-poner el nombre de las variables

    datos<-data.frame(m1,m2)
    datos<-na.omit(datos)

    mm1<-m1[!is.na(m1)]
    mm2<-m2[!is.na(m2)]

  # informacion muestral
    m1_n<-length(datos$m1)
    m1_miss<-len_m1-length(datos$m1)
    m1_m<-mean(datos$m1)
    m1_s<-sd(datos$m1)
    m1_sem<-m1_s/sqrt(m1_n)
    m1_s2<-m1_s^2
    if(vac==FALSE){
      m1_cpc<-1/(2*m1_n)
      m1_cpctxt<-ptxt(pval=m1_cpc,decs=decs,param="cpc")
      #cat(paste(tab,m1_cpctxt,sep=""),"\n")
    } else {m1_cpc<-0;m1_cpctxt<-""}


    m2_n<-length(datos$m2)
    m2_miss<-len_m2-length(datos$m2)
    m2_m<-mean(datos$m2)
    m2_s<-sd(datos$m2)
    m2_sem<-m2_s/sqrt(m2_n)
    m2_s2<-m2_s^2
    if(vac==FALSE){
      m2_cpc<-1/(2*m2_n)
      m2_cpctxt<-ptxt(pval=m2_cpc,decs=decs,param="cpc")
      #cat(paste(tab,m2_cpctxt,sep=""),"\n")
    } else {m2_cpc<-0;m2_cpctxt<-""}

    if(m1_m>=m2_m){difmodo<-12} else {difmodo<-21}
    if (difmodo==12){diftxt=paste("\u03bc\u2081-\u03bc\u2082",sep="") } else {diftxt=paste("\u03bc\u2082-\u03bc\u2081",sep="") }

    ifelse(difmodo==12,  m<-datos$m1-datos$m2,   m<-datos$m2-datos$m1)
    m_len<-length(m)
    m<-m[!is.na(m)]
    m_n<-length(m)
    m_miss<-m_len-m_n
    m_m<-mean(m)
    m_s<-sd(m)
    m_sem<-m_s/sqrt(m_n)
    m_s2<-m_s^2
    if(vac==FALSE){
      m_cpc<-1/(2*m_n)
      m_cpctxt<-ptxt(pval=m_cpc,decs=decs,eq=2,param="cpc")
      #cat(paste(tab,m_cpctxt,sep=""),"\n")
    } else {m_cpc<-0;m_cpctxt<-""}

    params_m1<-greek("m",1)
    params_m2<-greek("m",2)
    params_m <- diftxt

    # correlacion
    r<-cor(m1, m2, use ="pairwise")

    # test de normalidad de la diferencia
    sw <- shapiro.test(m)
    sw_w <- sw[[1]]
    sw_p <- sw[[2]]
    sw_txt<-paste("W = ",roundf(sw_w,decs),", gl = ",m_n,", ",ptxt(sw_p,decs,2),sep="")

    # test
    t_gl   <- (m_n-1)
    t_num  <- abs((m_m-m0))-m_cpc
    t_den  <- (m_s/sqrt(m_n))
    t_exp  <- abs(t_num)/t_den
    t_pval <- 2*(1-pt(t_exp,t_gl))
    t_alfa <- qt((1-alfa/2),t_gl)
    t_lci <- t_num-t_alfa*t_den
    t_uci <- t_num+t_alfa*t_den
    t_2beta <- qt((1-beta),t_gl)
    t_lci2b <- t_num-t_2beta*t_den
    t_uci2b <- t_num+t_2beta*t_den
    if(delta>0){n_est<-m_s2*((t_alfa+t_2beta)/delta)^2}



    # # # # # # # # # # # # # ## # # # # # #
    # Test para muestras apareadas
    # # # # # # # # # # # # # ## # # # # # #

    cat("\n")
    showtitle("t-test para dos muestras relacionadas",1)

    wmsgetxt<-""
    if (vac==FALSE){
      wmsgetxt<-c(wmsgetxt,("Se aplica una correcci\u00f3n por continuidad (cpc) sobre los IC y el t-test"))
    }
    if (m1_miss>0)             wmsgetxt<-c(wmsgetxt,(paste("Aparecen datos faltantes en ",m1_name,sep="")))
    if (m2_miss>0)             wmsgetxt<-c(wmsgetxt,(paste("Aparecen datos faltantes en ",m2_name,sep="")))
    if ((m1_miss+m2_miss)>0)   wmsgetxt<-c(wmsgetxt,(paste("Se omiten las parejas con alg\u00fan dato faltante",sep="")))
    wmsge(wtext=wmsgetxt,tab="",w=TRUE,sep=TRUE)

    showtitle("Informaci\u00f3n muestral y estimaci\u00f3n de las medias",2)
    cpc_vec<-round(c(m1_cpc,m2_cpc,m_cpc),decs)
    resumen_medias(
      n=c(m1_n,m2_n,m_n),
      nmiss=c(m1_miss,m2_miss,m_miss),
      m=c(m1_m,m2_m,m_m),
      s=c(m1_s,m2_s,m_s),
      sem=c(m1_sem,m2_sem,m_sem),
      cpc=c(m1_cpc,m2_cpc,m_cpc),
      ic=TRUE,
      alfa=alfa,
      vnames=c(m1_name,m2_name,"Diferencia"),
      params=c(params_m1,params_m2,params_m),
      footnote=TRUE,
      decs=decs
    )
    cat("\n")

    showtitle(paste("Correlaci\u00f3n de Pearson entre ",m1_name," y ", m2_name,":",sep=""),lev=2)
    cat(paste(tab,"r = ",roundf(r,decs),sep=""),"\n")
    cat("\n")

    if(!is.null(sw))
    {showtitle("Normalidad de la diferencia (Test de Shapiro-Wilk)",lev=2)
     cat(paste(tab,sw_txt,sep=""),"\n")}


    cat("\n")
    if(m0==0){
      showtitle(paste("t-test ",greek("H",0),":",greek("m",1),"=",greek("m",2)," (test de homogeneidad)", sep=""),lev=2)
    }else{
      showtitle(paste("t-test ",greek("H",0),":",greek("m",1),"-",greek("m",2),"=",greek("m",0),"  con ",greek("m",0),"=",m0,sep=""),lev=2)
    }

    ptxt_2c<-ptxt(t_pval,decs,eq=2)
    ptxt_1c<-ptxt(t_pval/2,decs,eq=2)

    if(vac==FALSE){
      cat(paste(tab,m_cpctxt,sep=""),"\n")
    }
    cat(paste(tab,"texp = ",roundf(t_exp,decs),", gl = ",t_gl,sep=""),"\n")
    if(m0==0){
      ifelse(m1_m>m2_m,desig<-">",desig<-"<")
      cat(paste(tab2,ptxt_2c," para la alternativa bilateral ", greek("H",1),":",greek("m",1),"\u2260",greek("m",2),sep=""),"\n")
      cat(paste(tab2,ptxt_1c," para la alternativa unilateral ",greek("H",1),":",greek("m",1),desig,greek("m",2),sep=""),"\n")
      ictxt<-paste(roundf((1-alfa)*100,0),"%-IC(",diftxt,") = (",roundf(t_lci,decs),", ",roundf(t_uci,decs),")",sep="")
      cat(paste(tab,ictxt,sep=""),"\n")
    }else{
      ifelse(m_m>m0,desig<-">",desig<-"<")
      cat(paste(tab2,ptxt_2c," para la alternativa bilateral ", greek("H",1),":",greek("m"),"\u2260",greek("m",0),sep=""),"\n")
      cat(paste(tab2,ptxt_1c," para la alternativa unilateral ",greek("H",1),":",greek("m"),desig,greek("m",0),sep=""),"\n")
      ictxt<-paste(roundf((1-alfa)*100,0),"%-IC(",greek("m"),"-",greek("m",0),") = (",roundf(t_lci,decs),", ",roundf(t_uci,decs),")",sep="")
      cat(paste(tab,ictxt,sep=""),"\n")
      cat(paste(tab,"con ", greek("m"),"=",diftxt," y ",greek("m",0),"=",m0,sep=""),"\n")
    }

    # estudio de la potencia
    # # # # # # # # # # # # # #
    if (delta>0){
      cat("\n")
      if(t_pval<=0.05)
      {showtitle("Estudio de la potencia:",lev=2)
        cat(paste(tab,"El test es significativo, se omite el an\u00e1lisis de la potencia.",sep=""),"\n")
      }else{
       txt<-paste("Estudio de la potencia: \u03b4 = ",delta," -> [",m0-delta,", ",m0+delta,"], potencia \u03b8= ",potencia*100,"%",sep="")
       showtitle(paste(txt,sep=""),lev=2)

       ifelse(m0==0,
             icbtxt<-paste(tab,roundf((1-2*beta)*100,0),"%-IC(",diftxt,") = (",roundf(t_lci2b,decs),", ",roundf(t_uci2b,decs),")",sep=""),
             icbtxt<-paste(tab,roundf((1-2*beta)*100,0),"%-IC((",diftxt,")-",m0,") = (",roundf(t_lci2b,decs),", ",roundf(t_uci2b,decs),")",sep=""))
       cat(icbtxt,"\n")

       get_diagram_ic(ic=c(t_lci2b,t_uci2b),id=c(m0-delta,m0+delta), potencia = potencia,m0=m0,param="\u03bc",eco=TRUE)

      }
      cat("\n")
      showtitle("Estimaci\u00f3n del tama\u00f1o muestral:",lev=2)

      cat(paste(tab,"Diferencia a detectar: \u03b4 =",delta,sep=""),"\n")
      cat(paste(tab,"Error de tipo I: ",greek("a"),"= ",roundf(alfa,decs),sep=""),"\n")
      cat(paste(tab,"Potencia: \u03b8 = ",roundf(potencia,decs), sep=""),"\n")
      cat(paste(tab,"n \u2a7e ",trunc(n_est+1,0)," casos",sep=""),"\n")
    }#end if delta>0

    if(grf){
      x<-c(m1,m2)
      f<-as.factor(c(rep(names[[1]],length(m1)),rep(names[[2]],length(m2))  ))
      grpsggp(x=x,f=f,     ggid=c(2),       lbls=c(paste("Variable (n=",m_n,")",sep=""),"Casos"))
      grpsggp(x=x,f=f,     ggid=c(9,7,5),   lbls=c("Media(sd)",paste("Muestra (n=",length(m1),")",sep="")))
    }
cat("\n")
}
