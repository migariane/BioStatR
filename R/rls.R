#' @noRd
#' @title rl
#' @description utilidad para el ajuste del modelo de RLS de acuerdo a la especificacion y~x
#' @param f formula
#' @param data dataframe
#' @return data.frame del modelo
#' @noRd
rl<-function(f,data=NULL){
  #procesamiento de la formula
  cfm<-as.character(f)
  # cat("Modelo: ",cfm[[2]],cfm[[1]],cfm[[3]],"\n")
  vexplic<-cfm[[3]]
  mexp<-strsplit(vexplic," ")
  mexp<-mexp[[1]]
  npred<-length(mexp) # numero de preds debe ser 1
  # si npred != 1 generar error
  if(npred>1) return(npred)

  # variable explicativa
  attr(terms(f),which="term.labels")
  vexp<-attr(terms(f),which="term.labels")

  # Respuesta
  vars<-attr(terms(f),which="variables")

  if(is.null(data)){
    # ejemplo de llamada
    # rl(osteo$imc~osteo$peso)

    mf<-model.frame(f) # obtener el dataframe
    lbls<-colnames(mf)
    # variable explicativa
    xlbl<-strsplit(lbls[[2]],"\\$")
    lblx<-xlbl[[1]][2]
    # Respuesta
    ylbl<-strsplit(lbls[[1]],"\\$")
    lbly<-ylbl[[1]][2]

    # generar output
    colnames(mf)<-c(lbly,lblx)
    return(mf)
  } else {    # !is.null(data)
    # ejemplo de llamada
    # rl(imc~peso, data=osteo)

    # Variable explicativa
    varx<-get(vexp,data)
    varxlbl<-cfm[[3]]
    #cat("Media de ",varxlbl,": ",mean(varx),"\n")

    # Respuesta
    vary<-get(vars[[2]],data)
    varylbl<-cfm[[2]]
    #cat("Media de ",varylbl,": ",mean(vary),"\n")

    df<-data.frame(vary,varx)
    colnames(df)[1] <- varylbl
    colnames(df)[2] <- varxlbl

    return(df)
  }
}



#' @title Regresion lineal simple
#' @description Ajuste del modelo de regresion lineal simple de acuerdo a la especificacion y~x
#' @param y vector: variable explicada o formula del modelo lineal
#' @param x vector: variable explicativa
#' @param data data.frame: tabla de datos (necesaria si se indica y como formula)
#' @param pred vector: valores del regresor para realizar pronosticos a partir del modelo
#' @param grf valor logico: si grf=FALSE se omite la salida grafica
#' @param dfout valor logico: si dfout=TRUE el procedimiento devuelve la matriz de datos con valores residuales y pronosticos
#' @param conf valor real < 1: nivel de confianza para la elaboracion del IC para la estimacion del efecto
#' @param alfa valor real < 1: error alfa (parametro alternativamente al nivel de confianza, en tanto por uno). Por defecto =.05.
#' @param decs valor entero: precision decimal para la salida de resultados. Por defecto = 4.
#' @return Informe con medidas descriptivas, correlaciones, estimacion de los parametros de regr. lineal, descriptiva residual y graficos de dispersion y de la distribucion residual
#' @importFrom methods show
#' @importFrom stats cor cor.test cov lm na.exclude rstandard sd var shapiro.test
#' @importFrom ggplot2 ggplot aes geom_dotplot geom_histogram geom_point geom_smooth geom_ribbon
#' @importFrom ggplot2 ggplot geom_linerange labs
#' @export rls
#' @examples
#' pre  <-c(200.1, 190.9, 192.7, 213, 241.4, 196.9, 172.2, 185.5, 205.2, 193.7)
#' post <-c(392.9, 393.2, 345.1, 393, 434, 427.9, 422, 383.9, 392.3, 352.2)
#' # Ejemplo 1 - entrada como vectores individuales
#' rls(post, pre) #ajusta el modelo post~pre
#' # Ejemplo 2 - entrada como formula
#' datos<-data.frame(pre,post)
#' rls(post~pre, data=datos)
#' # Ejemplo 3 - Pronosticos
#' rls(post~pre, data=datos,pred=c(197,205))
#' # Ejemplo 4 - Obtencion de la matriz con residuos y pronosticos
#' tabla<-rls(post~pre, data=datos,dfout=TRUE)
#' head(tabla)

rls<-function(y=NULL,x=NULL,data=NULL,pred=NULL,grf=TRUE,dfout=F ,alfa=0.05,conf=1-alfa, decs=3){
  # ejemplos de entrada de datos:
    # como formula:
    #  rls(imc~peso, data=osteo,grf=F)
    #  rls(osteo$imc~osteo$peso,grf=F)
    # como vectores individuales:
    #  rls(osteo$imc,osteo$peso,grf=F)

  isErr<-FALSE
  # determinación del tipo de entrada
  if(class(y)=="formula") {
    # obtener el data.frame del modelo
    dataf<-rl(y,data)
    if (class(dataf)=="integer") {
      isERR<-TRUE
      stop(get_msg("rls_stop_multi_var"))}
    ylbl<-names(dataf)[1]
    xlbl<-names(dataf)[2]
  }else{
    # los datos se dan como vectores (y) (x)
    if(is.null(y) || is.null(x)) {
      isErr<-TRUE
      stop(get_msg("rls_stop_invalid_input"))}

    ylbl<-deparse(substitute(y))
    yname<-strsplit(ylbl,"\\$")
    ylbl<-yname[[1]][2]

    xlbl<-deparse(substitute(x))
    xname<-strsplit(xlbl,"\\$")
    xlbl<-xname[[1]][2]

    dataf<-data.frame(y,x)
    colnames(dataf)<-c(ylbl,xlbl)
  }
  if(isErr) return()

  tab=" "
  n<-nrow(dataf)
  y<-dataf[,1]
  x<-dataf[,2]

  # determinacion de valores faltantes + descriptiva de cada variable
  xmiss<-length(x[is.na(x)])
  ymiss<-length(y[is.na(y)])

  n<-length(x)
  sxx<-(n-1)*var(x)
  syy<-(n-1)*var(y)
  sxy<-(n-1)*cov(x,y)

  my<-mean(y)
  ymin<-min(y)
  ymax<-max(y)
  yrank<-ymax-ymin

  mx<-mean(x)
  xmin<-min(x)
  xmax<-max(x)
  xrank<-xmax-xmin

  vars<-c(ylbl,xlbl)
  n_total<-c(n+ymiss,n+xmiss)
  n_miss<-c(ymiss,xmiss)
  n_val<-c(n,n)
  sx<-round(sd(x),decs)
  sy<-round(sd(y),decs)
  medias<-c(round(my,decs),round(mx,decs))
  mins<-c(round(ymin,decs),round(xmin,decs))
  maxs<-c(round(ymax,decs),round(xmax,decs))
  rangos<-c(round(yrank,decs),round(xrank,decs))
  dt<-c(sy,sx)

  if(ymiss+xmiss>0){
       tabla_desc<-data.frame(variable=vars,n=n_total, faltante=n_miss, n_valido=n_val, media=medias, dt=dt,Min=mins,Max=maxs,Rango=rangos)
  }else{
       tabla_desc<-data.frame(variable=vars,n=n_val, media=medias, dt=dt,Min=mins,Max=maxs,Rango=rangos)
  }

  modelo<-lm(y~x) #

  # coeficientes del modelo (calculo explicito)

  b1<- sxy/sxx
  b0<- my-b1*mx

  s2r<-(syy-(sxy^2)/sxx)/(n-2)
  t_gl<-n-2
  se_b1<-sqrt(s2r/sxx)
  se_b0<-sqrt(s2r* ((1/n)+(mx^2/sxx)))

  #intervalo
  #if(conf!=.95){alfa<-1-conf} else {if (alfa!=0.05){conf<-1-alfa}}
  epairsget(p=alfa,q=conf, pmin=0.0001,pdefault=0.05)->par_alfa
  if(par_alfa[[1]][1]){
    alfa<-par_alfa[[2]][1]
    conf<-par_alfa[[2]][2]
  } else {stop(get_msg("rls_stop_invalid_conf"))}


  talfa<- qt(1 -(alfa / 2), t_gl)
  d_b1<-talfa*se_b1
  icl_b1<-b1-d_b1
  icu_b1<-b1+d_b1
  d_b0<-talfa*se_b0
  icl_b0<-b0-d_b0
  icu_b0<-b0+d_b0

  #test
  p_tol<-0.001
  t_exp_b1=abs(b1)/se_b1
  t_pval_b1<- 2*(1-pt(t_exp_b1,t_gl))
  ptxt_b1<-ifelse(t_pval_b1<=p_tol,paste("<",p_tol,sep=""),round(t_pval_b1,decs) )

  t_exp_b0=abs(b0)/se_b0
  t_pval_b0<- 2*(1-pt(t_exp_b0,t_gl))
  ptxt_b0<-ifelse(t_pval_b0<=p_tol, paste("<",p_tol,sep=""), round(t_pval_b0,decs) )

  termino<-c("(Constante)",xlbl)
  estimacion<-c(round(b0,decs),round(b1,decs))
  se<-c(round(se_b0,decs),round(se_b1,decs))
  ic_inf<-c(round(icl_b0,decs),round(icl_b1,decs))
  ic_sup<-c(round(icu_b0,decs),round(icu_b1,decs))
  texp<-c(round(t_exp_b0,decs),round(t_exp_b1,decs))
  sig<-c(ptxt_b0,ptxt_b1)

  # correlacion
  rpearson<-round(cor(x=x,y=y),decs)
  #rspearman<-cor((x=x,y=y, method="spearman")
  r2<-round(as.numeric(rpearson)^2,decs)

  rtest<-cor.test(x,y,method="pearson")
  t_exp_r<-round(rtest$statistic,decs)
  t_pval_r<-round(rtest$p.value,decs)
  ptxt_r<-ifelse(t_pval_r<=p_tol,paste("< ",p_tol,sep=""),paste("= ",round(t_pval_r,decs),sep="") )
  icl_r<-round(rtest$conf.int[[1]],decs)
  icu_r<-round(rtest$conf.int[[2]],decs)

  regm<-lm(y~x)

  res<-regm$residuals
  zres<-rstandard(regm)
  pre<-regm$fitted.values

  sw_test<-shapiro.test(res)
  ptxt_sw<-ifelse(sw_test$p.value<=p_tol,paste("< ",p_tol,sep=""),paste("= ",round(sw_test$p.value,decs),sep="") )

  mc_min<-vector(mode="numeric",n)
  mc_max<-vector(mode="numeric",n)
  for(i in 1:n){
    mc_min[i]<-min(pre[i],y[i])
    mc_max[i]<-max(pre[i],y[i])
  }


  dat<-data.frame(x,y,res,zres,pre,mc_min,mc_max)
  bmeds <-  predict(modelo,dat,interval = 'confidence')
  bmeds <-  bmeds[,-1]
  bpreds <- predict(modelo,dat,interval = 'prediction')
  bpreds <- bpreds[,-1]
  dat<-data.frame(dat,bmeds,bpreds)


  # Pronosticos
  tpronosticos<-data.frame()
  extrapol<-FALSE
  if(!is.null(pred)){
    #puntual<-predict(modelo,newdata=data.frame(x=pred))
    pronmin<-min(pred)
    pronmax<-max(pred)

    if(pronmin<xmin || pronmax>xmax){
      extrapol<-TRUE
      prmsge<-paste (get_msg("rls_extrapol_msg"),roundf(xmin,decs)," - ",roundf(xmax,decs),sep="")
    }
    bconf<-predict(modelo,newdata=data.frame(x=pred),level=conf,interval="confidence")
    bpreds <- predict(modelo,newdata=data.frame(x=pred),level=conf,interval = 'prediction')

    tpronosticos<-data.frame(predictor=pred,bconf,bpreds)

    tpronosticos<-tpronosticos[,-5]
    names(tpronosticos)<-c("Predictor","Puntual","IC(m)_inf","IC(m)_sup","IC(obs)_inf","IC(obs)_sup")
  }

  # resultados
  tabla_coefs<-data.frame(Coef=termino, estim=estimacion, se, ic_inf, ic_sup, texp, sig)

  tabla_r<-data.frame(r=rpearson, IC_inf=icl_r, IC_sup=icu_r, gl=t_gl, texp=t_exp_r, sig=ptxt_r)
  row.names(tabla_r) <-" "

  tabla_resids<-round(data.frame(res=quantile(res,na.rm=T),zres=quantile(zres,na.rm=T)),decs)
  row.names(tabla_resids) <-c("min","Q1","Q2","Q3","max")

  cat("\n")
  cat(get_msg("rls_title"), "\n")
  cat("----------------------------------------------------------------\n")
  cat(get_msg("info_muestral"),"\n")
  cat("\n")
  print(tabla_desc)
  cat("\n")
  cat(tab," Cov(",ylbl,",",xlbl,") = ", round(cov(x,y),decs),sep="")
  cat("\n")
  cat("\n")
  cat(get_msg("rls_cor_pearson"), "\n")
  cat("\n")
  print(tabla_r)
  cat("\n")
  cat(get_msg("modelo_lineal"), "\n")
  cat("\n")
  cat(tab,"Modelo: ",ylbl,"~",xlbl,"\n")
  cat(tab,"R\u00b2 = ", r2,"\n")
  cat(tab,"S\u00b2residual = ",round(s2r,decs),"\n")
  cat(tab, get_msg("rls_coeficientes"), ":\n")
  cat("\n")
  print(tabla_coefs)
  cat("\n")

  if(!is.null(pred)) {
    cat(get_msg("rls_pronosticos"), "\n")
    cat(tab, get_msg("rls_pron_bandas"), conf*100, get_msg("rls_confianza"), "\n")
    cat(tab, get_msg("rls_promedios"), "  \n")
    cat("\n")
    print(tpronosticos)
    cat("\n")
    if(extrapol) wmsge(prmsge)
  }
  cat(get_msg("rls_dist_residual"), "\n")
  su<-summary(modelo)
  cat(tab, get_msg("rls_error_est_res"), round(su$sigma,decs),"\n")
  print(tabla_resids)
  cat("\n")
  cat(tab, get_msg("rls_test_normalidad"), "\n ")
  cat(tab,"w =",round(sw_test$statistic,decs),", p",ptxt_sw,sep="", "\n")
  cat("\n")



if(grf){
  #plot(modelo)
  ggbase_xy<-ggplot(data=dat, mapping=aes(x=x,y=y))

 #modelo
  sctr0<-ggbase_xy+
    geom_point()+
    labs(title=get_msg("rls_plot_scatter"),x=xlbl, y=ylbl)

  sctr1<-ggbase_xy+
    geom_point()+
    geom_smooth(method="lm",se=FALSE)+
    labs(title=get_msg("rls_plot_scatter_lm"),x=xlbl, y=ylbl)

  sctr2<-ggbase_xy+
    geom_point()+
    geom_smooth(method="lm",se=TRUE)+
    ggplot2::geom_ribbon(aes(ymin=lwr.1,ymax=upr.1),alpha=0.1,color="lightgrey")+
    labs(title=get_msg("rls_plot_pron"),subtitle=paste(get_msg("rls_plot_sub_conf"),conf*100,"% de confianza",sep=""),x=xlbl, y=ylbl)


  sctr3<-ggbase_xy +
   geom_smooth(method="lm",se=FALSE,color="blue")+
   geom_smooth(se=FALSE,color="brown",linetype = "dashed")+
   geom_point()+
   labs(title=get_msg("rls_plot_eval_lin"), subtitle=get_msg("rls_plot_sub_lin"),x=xlbl, y=ylbl)


  sctr4<-ggbase_xy +
    geom_linerange(aes(ymin = mc_min, ymax = mc_max),color="brown",linetype = "dashed") +
    geom_smooth(method="lm",se=FALSE,color="blue")+
    geom_point()+
   labs(title=get_msg("rls_plot_dist_res"),x=xlbl, y=ylbl)


 # residuos
  sctr5<-ggplot2::ggplot(data=dat, mapping=aes(x=x,y=res))+
   geom_point()+
   geom_hline(yintercept=0)+
   geom_smooth(method="lm",se=FALSE)+
   geom_smooth(method="loess",se=FALSE,color="brown",linetype="dashed")+
   labs(title=get_msg("rls_plot_dist_res_brutos"),x=xlbl, y="Residuos")


  sctr6<-ggplot2::ggplot(data=dat, mapping=aes(x=pre,y=zres))+
   geom_point()+
   geom_smooth(method="loess",se=FALSE,color="brown",linetype="dashed")+
   geom_hline(yintercept=0)+
   geom_hline(yintercept=-2,color="red",linetype = "dashed")+
   geom_hline(yintercept=2, color="red",linetype = "dashed")+
   labs(title=get_msg("rls_plot_res_est"),x=get_msg("rls_plot_x_pron"), y=get_msg("rls_plot_y_res_est"))


 if(length(zres)>30){
   sctr7<-ggplot2::ggplot(data=dat, mapping=aes(zres))+
      geom_histogram()+
      labs(title=get_msg("rls_plot_dist_res_2"),x=get_msg("rls_plot_y_res_est"),y=get_msg("rls_plot_y_frec"))
  } else {
   sctr7<-ggplot2::ggplot(data=dat, mapping=aes(x=zres))+
      geom_dotplot()+
      labs(title=get_msg("rls_plot_dist_res_2"),x=get_msg("rls_plot_y_res_est"),y=get_msg("rls_plot_y_frec"))
  }

  par(mfrow=c(2,2))
  plot(modelo)
  par(mfrow=c(1,1))

  suppressMessages(show(sctr7))
  suppressMessages(show(sctr6))
  suppressMessages(show(sctr5))
  suppressMessages(show(sctr4))
  suppressMessages(show(sctr3))
  suppressMessages(show(sctr2))
  suppressMessages(show(sctr1))
  suppressMessages(show(sctr0))
}
  invisible(regm)
  if (dfout) return(dat[,c(-6,-7)])
}

