#' @noRd
#'
test2i<-function(m=NULL,m1=NULL,m2=NULL,n1=0,n2=0,s1=0,s2=0,m0=0, grupos=NULL, vac=TRUE,alfa=.05, delta=0,beta=0.2,vnames=NULL, decs=3,grf=TRUE,caso=0)
{
  #

  tab<-"  "
  tab2<-paste(tab,tab,sep="")
  conf<-1-alfa
  potencia<-1-beta

  if (caso==21) # vectores m y grupos (grupos debe ser un factor binario)
  { # Los datos son: m  y  grupos

    #nombre de las variables
    if(!is.null(vnames)){
      varnames<-vnames
    }else{
      varnames=c(deparse(substitute(m)),deparse(substitute(grupos)))}

    # analisis de la agrupacion
    grp<-levels(as.factor(grupos))
    grp_len<-nlevels(grupos) # numero de grupos
    if(grp_len==1) stop(paste("El factor de agrupaci\u00f3n ",varnames[[2]]," solo tiene un nivel ",sep=""))
    if(grp_len >2) stop(paste("El factor de agrupaci\u00f3n ",varnames[[2]]," presenta ",grp_len," niveles",sep=""))
    grpnames<-grp


    if (length(m)==length(grupos)) {
      datos<-data.frame(m,grupos)
      ntotal<-nrow(datos)
    }else{
      stop(paste("Las variables ",varnames[[1]]," y ",varnames[[2]]," no tienen la misma longitud"))}

    #muestra grupo==1
    m1_name<-paste(varnames[[1]]," [",grp[[1]],"]",sep="")
    m1<-datos$m[grupos==grp[[1]]]
    m1_len<-length(m1)
    m1 <-m1[!is.na(m1)]
    m1_n<-length(m1)
    m1_miss<-m1_len-m1_n
    m1_gl<-m1_n-1
    m1_m<-mean(m1)
    m1_s<-sd(m1)
    m1_sem<-m1_s/sqrt(m1_n)
    m1_s2<-m1_s^2
    #muestra grupo==2
    m2_name<-paste(varnames[[1]]," [",grp[[2]],"]",sep="")
    m2<-datos$m[grupos==grp[[2]]]
    m2_len<-length(m2)
    m2 <-m2[!is.na(m2)]
    m2_n<-length(m2)
    m2_miss<-m2_len-m2_n
    m2_gl<-m2_n-1
    m2_m<-mean(m2)
    m2_s<-sd(m2)
    m2_sem<-m2_s/sqrt(m2_n)
    m2_s2<-m2_s^2

  } #end if caso = 21

  if (caso==22) # m1 y m2 son 2 vectores independientes de datos (cada uno un grupo)
  {
    if(!is.null(vnames)){
      varnames<-vnames
    }else{
      varnames=c(deparse(substitute(m1)),deparse(substitute(m2)))}
    grp<-varnames
    grp_len<-2
    #muestra grupo==1
    m1_name<-varnames[[1]]
    m1_len<-length(m1)
    m1 <-m1[!is.na(m1)]
    m1_n<-length(m1)
    m1_miss<-m1_len-m1_n
    m1_gl<-m1_n-1
    m1_m<-mean(m1)
    m1_s<-sd(m1)
    m1_sem<-m1_s/sqrt(m1_n)
    m1_s2<-m1_s^2
    #muestra grupo==2
    m2_name<-varnames[[2]]
    m2_len<-length(m2)
    m2 <-m2[!is.na(m2)]
    m2_n<-length(m2)
    m2_miss<-m2_len-m2_n
    m2_gl<-m2_n-1
    m2_m<-mean(m2)
    m2_s<-sd(m2)
    m2_sem<-m2_s/sqrt(m2_n)
    m2_s2<-m2_s^2
  }

  if (caso==23)  # medidas resumen de 2 muestras independientes resumen
  { grp<-vector("list",length = 2)
    grp[[1]]<-1
    grp[[2]]<-2
    grp_len<-2
    if(!is.null(vnames)){
      varnames<-vnames
    }else{
      varnames=c("Grupo I","Grupo II")}

    grpnames<-varnames
    m1_name<-varnames[[1]]
    m1_m<-m1
    m1_n<-n1
    m1_miss<-0
    m1_gl<-m1_n-1
    m1_s<-s1
    m1_sem<-m1_s/sqrt(m1_n)
    m1_s2<-m1_s^2

    m2_name<-varnames[[2]]
    m2_m<-m2
    m2_n<-n2
    m2_miss<-0
    m2_gl<-m2_n-1
    m2_s<-s2
    m2_sem<-m2_s/sqrt(m2_n)
    m2_s2<-m2_s^2

  }# end if caso = 23



  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # CALCULOS
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # aqui llegan los datos en forma de m<i> si son vectores y con medidas m<i>_<medida>

    m1_params<-greek("m",1)
    m2_params<-greek("m",2)

    if(m1_m>=m2_m){
      difmodo=12
      diftxt="\u03bc\u2081-\u03bc\u2082"
      difsamp=paste(m1_name," - ",m2_name,sep="")

    } else {
      difmodo=21
      diftxt="\u03bc\u2082-\u03bc\u2081"
      difsamp=paste(m2_name," - ",m1_name,sep="")
    }
    m_params<-diftxt


  # cpc
    if(!vac){
      #cpc individual ICs
      m1_cpc<-1/(2*m1_n)
      m1_cpctxt<-ptxt(pval=m1_cpc,decs=decs,eq=2,param="cpc")
      m2_cpc<-1/(2*m2_n)
      m2_cpctxt<-ptxt(pval=m2_cpc,decs=decs,eq=2,param="cpc")
      #cpc para el test
      cpc=((m1_n+m2_n)/(2*m1_n*m2_n))
      cpc_txt<-ptxt(pval=cpc,decs=decs,eq=2,param="cpc")
    } else {
      cpc=0;
      cpc_txt=""
      m1_cpc<-0;
      m1_cpctxt<-""
      m2_cpc<-0;
      m2_cpctxt<-""
    }

  # IC
    icm1<-icm(m=m1_m,n=m1_n,s=m1_s,vac=vac,eco=FALSE)
    icm2<-icm(m=m2_m,n=m2_n,s=m2_s,vac=vac,eco=FALSE)


  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # test de normalidad
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    if(caso!=23){
      sw1   <- shapiro.test(m1)
      sw1_w <- sw1[[1]]
      sw1_p <- sw1[[2]]
      sw1_ptxt<-ptxt(sw1_p,decs=decs,eq=2)
      sw1_txt<-paste("W = ",roundf(sw1_w,decs),", gl = ",m1_n,", ",sw1_ptxt,sep="")

      sw2 <- shapiro.test(m2)
      sw2_w <- sw2[[1]]
      sw2_p <- sw2[[2]]
      sw2_ptxt<-ptxt(sw2_p,decs=decs,eq=2)
      sw2_txt<-paste("W = ",roundf(sw2_w,decs),", gl = ",m2_n,", ",sw2_ptxt,sep="")
    }

  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # test F
  # # # # # # # # # # # # # # # # # # # # # # # # # # #
    {
     ftest<-testf(m1_s,m1_n,m2_s,m2_n)
     f_exp<-ftest[[1]]
     f_gl1<-ftest[[2]]
     f_gl2<-ftest[[3]]
     f_pval<-ftest[[4]]
     f_txt<-paste("Fexp = ",roundf(f_exp,decs),", gl\u2081 = ",f_gl1,", gl\u2082 = ",f_gl2,", ",ptxt(f_pval,decs=decs,eq=2), sep="")
    }

  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # test de Student
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    { t_gl<-m1_gl+m2_gl
      s2p<-((m1_gl*m1_s2)+(m2_gl*m2_s2))/t_gl
      t_num<-abs(m1_m-m2_m-m0)-cpc
      t_den<-sqrt(s2p*((m1_n+m2_n)/(m1_n*m2_n)))
      t_exp<-abs(t_num)/t_den
      t_pval<-2*(1-pt(t_exp,t_gl))
      t_ptxt<-ptxt(t_pval,decs=decs,eq=2)
      t_pval1c<-t_pval/2
      t_ptxt1c<-ptxt(t_pval1c,decs=decs,eq=2)
      t_alfa<-qt((1-alfa/2),t_gl)
      t_lci<-t_num-t_alfa*t_den
      t_uci<-t_num+t_alfa*t_den
      t_2beta<-qt((1-beta),t_gl)
      t_lci2b<-t_num-t_2beta*t_den
      t_uci2b<-t_num+t_2beta*t_den
    }

  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  #test de Welch
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    {
      a<-m1_s2/m1_n
      b<-m2_s2/m2_n
      w_gl<-((a+b)^2)/(((a^2)/m1_gl)+((b^2)/m2_gl))
      w_num<-abs((m1_m-m2_m)-m0)-cpc
      w_den<-sqrt(a+b)
      w_exp<-abs(w_num)/w_den
      w_pval<-2*(1-pt(w_exp,w_gl))
      w_ptxt<-ptxt(w_pval,decs=decs,eq=2)
      w_pval1c<-w_pval/2
      w_ptxt1c<-ptxt(w_pval1c,decs=decs,eq=2)
      w_alfa<-qt((1-alfa/2),w_gl)
      w_lci<-w_num-w_alfa*w_den
      w_uci<-w_num+w_alfa*w_den
      w_2beta<-qt((1-beta),w_gl)
      w_lci2b<-w_num-w_2beta*w_den
      w_uci2b<-w_num+w_2beta*w_den
    }

  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  # tamano muestral
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    if(delta>0){
      # caso de varianzas iguales
      n_est<-2*s2p*((t_alfa+t_2beta)/delta)^2
      # caso de varianzas distintas
      if(m1_s<=m2_s)
      { wn_r<-m2_s/m1_s
        wn1<-m1_n
        wn2<-m2_n
        wv1<-m1_s2
        wv2<-m2_s2}
      else
      { wn_r<-m1_s/m2_s
        wn1<-m2_n
        wn2<-m1_n
        wv1<-m2_s2
        wv2<-m1_s2
      }
      wn_df<-((1+wn_r)^2) / ( (1/(wn1-1)) +((wn_r^2)/(wn2-1))  )
      wn_alfa<-qt((1-alfa/2),wn_df)
      wn_2beta<-qt((1-beta),wn_df)
      wn1_est <- (((wn_alfa+wn_2beta)/delta)^2)*(wn_r+1)*wv1
      wn2_est <- wn_r*wn1_est
      if(m1_s<=m2_s){
        n1_est<-wn1_est
        n2_est<-wn2_est
      }else{
        n1_est<-wn2_est
        n2_est<-wn1_est
      }
    }#end if delta>0


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# SALIDAS
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

    cat(" \n")
    showtitle("t-test para 2 Muestras Independientes",lev=1)

    # warnings
    wmsge<-""
    if (vac==FALSE){
      wmsge<-c(wmsge,("Se aplica una correcci\u00f3n por continuidad (cpc) sobre los IC y el t-test"))
    }
    if (m1_miss>0) wmsge<-c(wmsge,(paste("Aparecen datos faltantes en ",m1_name,sep="")))
    if (m2_miss>0) wmsge<-c(wmsge,(paste("Aparecen datos faltantes en ",m2_name,sep="")))
    wmsge(wtext=wmsge,tab="",w=TRUE,sep=TRUE)

    showtitle("Informaci\u00f3n muestral y estimaci\u00f3n de las medias",2)

    if(grp_len>2)wmsge(wtext=paste("La variable de agrupaci\u00f3n presenta ",grp_len," niveles. Se consideran solo los dos primeros.",sep=""))

    cat(paste(tab,"Niveles de agrupaci\u00f3n: ",grp[[1]],", ",grp[[2]],sep=""),"\n")
    cat("\n")

    #Resumen muestral
    resumen_medias(
      n=c(m1_n,m2_n),
      nmiss=c(m1_miss,m2_miss),
      m=c(m1_m,m2_m),
      s=c(m1_s,m2_s),
      sem=c(m1_sem,m2_sem),
      cpc=c(m1_cpc,m2_cpc),
      ic=TRUE,
      alfa=alfa,
      vnames=c(m1_name,m2_name),
      params=c(m1_params,m2_params),
      footnote=TRUE,
      decs=decs
    )


    #test de normalidad
    if(caso!=23){
      cat("\n")
      showtitle("Pruebas de normalidad (test de Shapiro-Wilk)",lev=2)
      cat("[1] Para grupo = ",grp[[1]],", ",sw1_txt,"\n",sep="")
      cat("[2] Para grupo = ",grp[[2]],", ",sw2_txt,"\n",sep="")
    }

    # test F
    ifelse(m1_s2>=m2_s2,txt<-"(Var\u2081/var\u2082)",txt<-"(var\u2082/var\u2081)")
    showtitle(paste("Test de homogeneidad de varianzas. Fexp = ",txt,sep=""),lev=2)
    cat(paste(tab,f_txt,sep=""),"\n")


    showtitle(paste("Diferencia de medias (",difsamp,")",sep=""),lev=2)



    if(m0==0){
      cat(paste(tab,"Hip\u00f3tesis a contrastar: ",greek("H",0),":",greek("m",1),"=",greek("m",2)," (",diftxt,"=0)",sep=""),"\n")
    }else{
      cat(paste(tab,"Hip\u00f3tesis a contrastar: ",greek("H",0),":",greek("m"),"=",greek("m",0),sep=""),"\n")
      cat(paste(tab,"con ",greek("m"),"=",diftxt," y ",greek("m",0),"=",round(m0,decs),sep=""),"\n")
    }

    if(!vac){cat(paste(tab,cpc_txt,sep=""),"\n")}

    cat("\n")
    cat(paste(tab,"a) Test de Student (varianzas homog\u00e9neas)",sep=""),"\n")
    cat(paste(tab,"texp = ",roundf(t_exp,decs),", gl = ",t_gl,sep=""),"\n")
    ifelse(m1_m>m2_m,desig<-">",desig<-"<")

    if(m0==0){
      cat(paste(tab2,t_ptxt," para la alternativa bilateral ", greek("H",1),":",greek("m",1),"\u2260",greek("m",2),sep=""),"\n")
      cat(paste(tab2,t_ptxt1c," para la alternativa unilateral ", greek("H",1),":",greek("m",1),desig,greek("m",2),sep=""),"\n")
      ictxt=paste(roundf((1-alfa)*100,0),"%-IC(",diftxt,") = (",sep="")
      ictxt<-paste(ictxt,roundf(t_lci,decs),", ",roundf(t_uci,decs),")",sep="")
      cat(paste(tab,ictxt,sep=""),"\n")
    }else{
      ifelse(abs(m1_m-m2_m)>m0 ,desig<-">",desig<-"<")
      diftxt_m0=paste(diftxt,"-",m0,sep="")
      cat(paste(tab2,t_ptxt,  " para la alternativa bilateral ", greek("H",1),":",greek("m"),"\u2260",greek("m",0),sep=""),"\n")
      cat(paste(tab2,t_ptxt1c," para la alternativa unilateral ",greek("H",1),":",greek("m"),desig,greek("m",0),sep=""),"\n")
      cat(paste(tab,"con ", greek("m"),"=",diftxt," y ",greek("m",0),"=",m0,sep=""),"\n")
      ictxt=paste(roundf((1-alfa)*100,0),"%-IC(",diftxt,") = (",sep="")
      ictxt<-paste(ictxt,roundf(t_lci-m0,decs),", ",roundf(t_uci-m0,decs),")",sep="")
      cat(paste(tab,ictxt,sep=""),"\n")

    }
    cat("\n")

    cat(paste(tab,"b) Test de Welch (varianzas no homog\u00e9neas)",sep=""), "\n")
    if(!vac){cat(paste(tab,"Por ser una v.a. discreta, este test es preferible al cl\u00e1sico de Student",sep=""),"\n")}
    cat(paste(tab,"texp = ",roundf(w_exp,decs),", gl = ",roundf(w_gl,2),sep=""),"\n")
    if(m0==0){
      cat(paste(tab2,w_ptxt," para la alternativa bilateral ", greek("H",1),":",greek("m",1),"\u2260",greek("m",2),sep=""),"\n")
      cat(paste(tab2,w_ptxt1c," para la alternativa unilateral ", greek("H",1),":",greek("m",1),desig,greek("m",2),sep=""),"\n")
      ictxt=paste(roundf((1-alfa)*100,0),"%-IC(",diftxt,") = (",sep="")
      ictxt<-paste(ictxt,roundf(w_lci,decs),   ", ",roundf(w_uci,decs),   ")",sep="")
      cat(paste(tab,ictxt,sep=""),"\n")
    }else{
      ifelse(abs(m1_m-m2_m)>m0 ,desig<-">",desig<-"<")
      diftxt_m0=paste(diftxt,"-",m0,sep="")
      cat(paste(tab2,w_ptxt,  " para la alternativa bilateral ", greek("H",1),":",greek("m"),"\u2260",greek("m",0),sep=""),"\n")
      cat(paste(tab2,w_ptxt1c," para la alternativa unilateral ",greek("H",1),":",greek("m"),desig,greek("m",0),sep=""),"\n")
      cat(paste(tab,"con ", greek("m"),"=",diftxt," y ",greek("m",0),"=",m0,sep=""),"\n")

      ictxt=paste(roundf((1-alfa)*100,0),"%-IC(",diftxt,") = (",sep="")
      ictxt<-paste(ictxt,roundf(w_lci-m0,decs),", ",roundf(w_uci-m0,decs),")",sep="")
      cat(paste(tab,ictxt,sep=""),"\n")
    }

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    # Estudio de la potencia# # # # # # # # # # # # # # # # # # # # # # # #
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    if (delta>0){
      if(t_pval<=0.05){
        showtitle("Estudio de la potencia:",lev=2)
        cat(paste(tab,"El test es significativo, se omite el an\u00e1lisis de la potencia.",sep=""),"\n")
      }else{
        txt<-paste("Estudio de la potencia: \u03b4 = ",delta," -> [",m0-delta,", ",m0+delta,"], potencia \u03B8 =",potencia*100,"%",sep="")
        showtitle(txt,lev=2)

        if(m0==0){
          icbtxt<-paste(tab,roundf((1-2*beta)*100,0),"%-IC(",diftxt,") = (",roundf(w_lci2b,decs),", ",roundf(w_uci2b,decs),")",sep="")
          cat(icbtxt,"\n")
        }else{
          icbtxt<-paste(tab,roundf((1-2*beta)*100,0),"%-IC((",diftxt,")-",m0,") = (",roundf(w_lci2b,decs),", ",roundf(w_uci2b,decs),")",sep="")
          cat(icbtxt,"\n")
          icbtxt<-paste(tab,roundf((1-2*beta)*100,0),"%-IC((\u03bc\u2081-\u03bc\u2082)+(",m0,")) = (",roundf(w_lci2b+m0,decs),", ",roundf(w_uci2b+m0,decs),")",sep="")
          cat(icbtxt,"\n")
        }
        get_diagram_ic(ic=c(w_lci2b,w_uci2b),id=c(m0-delta,m0+delta), potencia = potencia,m0=m0,param="\u03bc",eco=TRUE)
      }

      # Tamano de muestra
      txt<- paste("Estimaci\u00f3n del tama\u00f1o muestral para detectar una diferencia \u03b4=",delta," con potencia  \u03B8=",potencia*100,"%",sep="")
      showtitle(txt,lev=2)
      cat("(1) Considerando las varianzas homog\u00e9neas: \n")
      cat(paste(tab,"(n1 = n2)  \u2a7e ",trunc(n_est+1,0)," casos en cada grupo",sep=""),"\n")
      cat("\n")
      txt<-ifelse(m1_s2>m2_s2,"s\u2081/s\u2082","s\u2082/s\u2081")
      cat(paste("(2) Considerando las varianzas heterog\u00e9neas: k=",txt,"=",roundf(wn_r,decs),", (gl'=",roundf(wn_df,2),")",sep=""),"\n")
      cat(paste(tab,"n\u2081 \u2a7e ",trunc(n1_est+1)," casos en el grupo [1]",sep=""),"\n")
      cat(paste(tab,"n\u2082 \u2a7e ",trunc(n2_est+1)," casos en el grupo [2]",sep=""),"\n")
      cat("\n")
    } # end if delta>0


    # salidas graficas
    if(grf){
      if(caso==21){

        f<-as.factor(c(rep(paste(grpnames[[1]]," (n=",length(m1),")",sep=""),length(m1)),rep(paste(grpnames[[2]]," (n=",length(m2),")",sep=""),length(m2))  ))
        grpsggp(x=m,f=f,     ggid=c(9,7,5,2),    lbls=grpnames)
      }
      if(caso==22){
        if(caso==22) grpnames<-varnames
        x<-x<-c(m1,m2)
        f<-c(c(rep(grpnames[[1]],length(m1))),
             c(rep(grpnames[[2]],length(m2))))

       # f<-as.factor(c(rep(paste(grpnames[[1]]," (n=",length(m1),")",sep=""),length(m1)),rep(paste(grpnames[[2]]," (n=",length(m2),")",sep=""),length(m2))  ))

        f=as.factor(f)
        grpsggp(x=x,f=f,     ggid=c(9,7,5,2),    lbls=grpnames)
      }
      if(caso==23){
        grpsggp(x=c(m1_m,m2_m),f=as.factor(c("1","2")),se=c(m1_s,m2_s), ggid=c(9),  lbls=c("Media(sd)","Muestra"))
      }


    }
    # fin generacion de graficos

}
