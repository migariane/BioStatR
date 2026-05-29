#' @title Test no parametricos con dos muestras independientes (Wilcoxon/Mann-Whitney) y apareadas (Wilcoxon)
#' @description Test de homogeneidad no parametricos para dos muestras independientes (test de Wilcoxont/Mann-Whitney con aprox. a la normal) y con dos muestras apareadas (Wilcoxon). Se dan algunas medidas de tamano del efecto. (Omision deliberada de tildes por compatibilidad)
#' @param m1 vector: vector de datos de la primera muestra cuando se indican dos muestras apareadas y tambien valido para muestras independientes.
#' @param m2 vector: vector de datos de la segunda muestra cuando se indican dos muestras apareadas y tambien valido para muestras independientes.
#' @param m  vector: vector de datos a contrastar en formato longitudinal. Es preciso especificar el vector grupos para segmentar a este vector
#' @param par valor logico: si los tamanos de m1 y m2 son iguales se asumen muestras apareadas, pero si par=FALSE se asumen independientes
#' @param grupos vector factor con dos niveles: variable de agrupacion en la comparacion de dos muestras independientes con valores dados en m
#' @param grf valor logico: si grf=FALSE se omite la salida grafica
#' @param conf valor real < 1: nivel de confianza para la elaboracion del IC para la estimacion del efecto
#' @param alfa valor real < 1: error alfa (parametro alternativamente al nivel de confianza, en tanto por uno). Por defecto =.05.
#' @param decs valor entero: precision decimal para la salida de resultados. Por defecto = 4.
#' @param mess valor entero: -1 desactiva mensajes de aviso (ver documentacion de Rs para options(warn = valor))
#' @return Informe con estadisticos de orden, rangos, resultado del test y estimacion del tamano del efecto
#' @importFrom methods show
#' @importFrom stats wilcox.test median IQR
#' @importFrom ggplot2 aes ggplot geom_boxplot geom_dotplot stat_summary labs
#' @export testwx
#' @examples
#' #[A] Muestras independientes
#' #[A.1] Como vectores independientes (distinto tamano)
#' y1<-c(78,64,75,45,82,55,48)
#' y2<-c(110,70,53,51,63,87)
#' testwx(m1=y1,m2=y2)
#'
#' #[A.2] Como vectores independientes (con el mismo tamano)
#' y1<-c(78,64,75,45,82,55,48)
#' y2<-c(110,70,53,51,63,87,99)
#' testwx(m1=y1,m2=y2,par=FALSE)
#'
#' #[A.3] En formato longitudinal (vector de valores y vector de agrupacion)
#' y<-c(78,64,75,45,82,55,48,  110,70,53,51,63,87)
#' g<-c(1,1,1,1,1,1,1,2,2,2,2,2,2)
#' testwx(m=y,grupos=g)
#'
#' #[B] Muestras apareadas (y1 e y2 deben tener el mismo tamano)
#' y1<-c(78,64,75,45,82,55,48)
#' y2<-c(110,70,53,51,63,87,99)
#' testwx(m1=y1,m2=y2)
#'
testwx<-function(m1=NULL,m2=NULL,par=FALSE,m=NULL,grupos=NA,grf=TRUE,alfa=0.05,conf=1-alfa, decs=3,mess=-1){

  defaultW <- getOption("warn")
  options(warn = mess)

  if(!is.null(m1)&&!is.null(m2)){
    m1_lbl<-deparse(substitute(m1))
    m2_lbl<-deparse(substitute(m2))}

  p_tol<-0.001
  tab<-"  "
  caso<-0

  #identificacion del problema
  #muestras indeps como vector + grupo
  if(!is.null(m) && !is.null(grupos)){
    if(length(m)!=length(grupos)) stop("Las dimensiones de 'm' y 'grupos' no son compatibles")
    df<-data.frame(x=m,g=as.factor(grupos))
    dfg<-df #data frame para graficos
    ng<-length(levels(df$g))
    if(ng<=1) stop("El n\u00famero de niveles de agrupaci\u00f3n es <= 1")
    lev1<-levels(df$g)[1]
    lev2<-levels(df$g)[2]
    if(ng>2)  warning("El n\u00famero de niveles de agrupaci\u00f3n es > 2. Se asumen los dos primeros: ",lev1," y ",lev2)
    x1<-df$x[df$g==lev1]
    x2<-df$x[df$g==lev2]
    xlbl<-deparse(substitute(m))
    glbl<-deparse(substitute(grupos))
    x1lbl<-paste(xlbl,"[",glbl,"=",lev1,"]",sep="")
    x2lbl<-paste(xlbl,"[",glbl,"=",lev2,"]",sep="")

    caso<-1 #muestras independientes
  }

  #muestras como vectores
  if(!is.null(m1) && !is.null(m2)){
     if((length(m1)==length(m2)) && par==TRUE){
      #muestras apareadas
      x1miss<-length(m1[is.na(m1)])
      x2miss<-length(m2[is.na(m1)])
      df<-na.exclude(data.frame(m1,m2))
      m1<-df$m1
      m2<-df$m2
      n<-length(m1)

      g<-as.factor(c(rep(m1_lbl,n),rep(m2_lbl,n)))
      datos<-c(m1,m2)
      dfg<-data.frame(x=datos,g)

      criterio_txt<-paste(m1_lbl," - ",m2_lbl,sep="")
      x<-m1-m2
      npar_ini<-length(x)
      x<-x[!is.na(x)]
      x<-x[x!=0]
      npar_valid<-length(x)

      x1<-x[x<0]
      x2<-x[x>0]

      x1lbl<-"dif.negativas"
      x2lbl<-"dif.positivas"
      caso<-2 #muestras apareadas

    } else {  #muestras independientes

      x1lbl<-deparse(substitute(m1))
      x2lbl<-deparse(substitute(m2))

      g<-as.factor(c(rep(m1_lbl,length(m1)),rep(m2_lbl,length(m2))))
      datos<-c(m1,m2)
      dfg<-data.frame(x=datos,g)

      x1<-m1
      x2<-m2
      caso<-1 #muestras independientes}
    }
  }

  #prodedimiento de calculo
    if(caso==0) stop("No se han indicado correctamente los par\u00e1metros")
    ifelse(caso==1,
         wilcox.test(x1,x2, paired=FALSE, conf.int = TRUE)->wt,
         wilcox.test(m1,m2, paired=TRUE , conf.int = TRUE)->wt)

    if(caso==1){
      x1miss<-length(x1[is.na(x1)])
      x2miss<-length(x2[is.na(x2)])
    } else{
      #medianas
      me1p<-median(m1)
      me2p<-median(m2)
      #Q1 y Q3
      q11p<-quantile(m1,0.25)
      q12p<-quantile(m2,0.25)
      q31p<-quantile(m1,0.75)
      q32p<-quantile(m2,0.75)
      #RIQ
      riq1p<-q31p-q11p
      riq2p<-q32p-q12p

    }


    n1<-length(x1)
    n2<-length(x2)
    g1<-rep(1,n1)
    g2<-rep(2,n2)
    x<-c(x1,x2)
    grupo<-c(g1,g2)
    x1x2f<-data.frame(x,grupo)
    rangos<-rank(x1x2f$x)
    x1x2f<-cbind(x1x2f,rangos)
    #medianas
    me1<-median(x1)
    me2<-median(x2)
    #Q1 y Q3
    q11<-quantile(x1,0.25)
    q12<-quantile(x2,0.25)
    q31<-quantile(x1,0.75)
    q32<-quantile(x2,0.75)
    #RIQ
    riq1<-q31-q11
    riq2<-q32-q12

    #sumas de rangos
    sr1<-sum(x1x2f$rangos[x1x2f$grupo==1])
    sr2<-sum(x1x2f$rangos[x1x2f$grupo==2])
    #rangos medios
    mr1<-sr1/n1
    mr2<-sr2/n2
    #u muestras independientes
    u1<-n1*n2+((n1*(n1+1))/2)-sr1
    u2<-n1*n2+((n2*(n2+1))/2)-sr2
    u<-min(c(u1,u2))

    if(caso==1){
      z<-abs((u-((n1*n2)/2))/sqrt((n1*n2*(n1+n2+1))/12))
    }else{
      z<-abs(((wt$statistic)-((n*(n+1))/4))/sqrt(n*(n+1)*(2*n+1)/24))
      # cat("z=",z,"\n")
      p_z<-2*(1-pnorm(z,0,1))
      p_z_txt<-ifelse(p_z<=p_tol,paste("< ",p_tol,sep=""),paste("= ",roundf(p_z,decs),sep="") )
    }

    #tama?o efecto
    if(caso==1){
      r<-abs(z/sqrt(n1+n2)) #Fritz et al
      r_lbl<-"(criterio: 0.1 peque\u00f1o; 0.3 mediano; >0.5 grande)"
      ps<-u/(n1*n2)         # Grissom & Kim () Effect Sizes for Research. A Broad Practical Approach. pp 98..
      ifelse(ps>=0.5,ip<-ps,ip<-(1-ps))
      ifelse(me1>=me2,ip_lbl<-">",ip_lbl<-"<")
    }

  # tablas de salida
  # Muestras
    if(caso==1){#muestras independientes
      if(x1miss+x2miss==0){
        samples_df<-data.frame(
        Muestra=c(x1lbl,x2lbl),
          n =c(n1,n2),
          min=roundf(c(min(x1),min(x2)),decs),
          Q1=roundf(c(q11,q12),decs),
          Q2= roundf(c(me1,me2)   ,decs),
          Q3=roundf(c(q31,q32),decs),
          max=roundf(c(max(x1),max(x2)),decs),
          RIQ=roundf(c(riq1,riq2),decs)
        )
      }else{
        samples_df<-data.frame(
          Muestra=c(x1lbl,x2lbl),
          n = c(n1+x1miss,n2+x2miss),
          faltante= c(x1miss,x2miss),
          n_valido= c(n1,n2),
          min=roundf(c(min(x1),min(x2)),decs),
          Q1=roundf(c(q11,q12),decs),
          Q2= roundf(c(me1,me2)   ,decs),
          Q3=roundf(c(q31,q32),decs),
          max=roundf(c(max(x1),max(x2)),decs),
          RIQ=roundf(c(riq1,riq2),decs)
        )
      }
    } else {#muestras apareadas
      samples_df<-data.frame(
        Muestra=c(m1_lbl,m2_lbl),
        n = c(n,n),
        min=roundf(c(min(m1),min(m2)),decs),
        Q1=roundf(c(q11p,q12p),decs),
        Q2= roundf(c(me1p,me2p)   ,decs),
        Q3=roundf(c(q31p,q32p),decs),
        max=roundf(c(max(m1),max(m2)),decs),
        RIQ=roundf(c(riq1p,riq2p),decs)
      )
    }

    rangos_df<-data.frame(
      Muestra=c(x1lbl,x2lbl),
      n = c(n1,n2),
      Suma_rangos=c(sr1,sr2),
      Rango_medio=roundf(c(mr1,mr2),decs)
    )
    if(caso==1)  rangos_df<-cbind(rangos_df,U=roundf(c(u1,u2),decs))


    ptxt_wx<-ifelse(wt$p.value<=p_tol,paste("< ",p_tol,sep=""),paste("= ",roundf(wt$p.value,decs),sep="") )

    cat("\n")
    if(caso==1){
      cat(paste("Test de Wilcoxon/Mann-Whithney para dos muestras independientes \n"))
      cat("----------------------------------------------------------------\n")
    } else {
      cat(paste("Test de Wilcoxon  para dos muestras apareadas \n"))
      cat("----------------------------------------------\n")
    }
    cat("# Informaci\u00f3n muestral ---\n")
    cat("\n")
    print(samples_df)
    cat("\n")
    cat("# Rangos ---\n")
    cat("\n")
    if(caso==2){
      cat(tab,"Se obtienen las diferencias como ",criterio_txt,"\n",sep="")
      cat(tab,"Pares de datos efectivos para los rangos: ",npar_valid," de ", npar_ini, sep="","\n")
      cat("\n")
    }
    print(rangos_df)
    cat("\n")
    cat("# Test ---\n")
    cat("\n")
    if(caso==1){
      cat(tab,"U = ",roundf(u,decs),"; Z = ",roundf(z,decs),"; W = ",roundf(wt$statistic,decs),"; p ",ptxt_wx,"\n",sep="")
    } else {
      cat(tab,"V = ",roundf(wt$statistic,decs),"; p ",ptxt_wx,"\n",sep="")
      cat(tab,"z = ",roundf(z,decs),"; p ",p_z_txt,"\n",sep="")
    }
    if(caso==2){
      cat("\n")
      cat("# Correlaci\u00f3n de Spearman ---\n")
      cat("\n")
      cor.test(m1,m2,method="spearman")->rs
      ptxt_rho<-ifelse(rs$p.value<=p_tol,paste("< ",p_tol,sep=""),paste("= ",roundf(rs$p.value,decs),sep="") )
      cat(tab,"rho-Spearman = ",roundf(rs$estimate,decs),"; p ",ptxt_rho,"\n",sep="")
    }
    cat("\n")
    cat("# Tama\u00f1o del efecto ---\n")
    cat("\n")
    if(caso==1){#muestras independientes
      cat(tab,"Diferencia de localizaci\u00f3n: ",roundf(wt$estimate,decs),sep="")
      cat(tab,"  95%-IC = (",roundf(wt$conf.int[1],decs),", ",roundf(wt$conf.int[2],decs),") \n", sep="" )
      cat(tab,"r = ",roundf(r,decs),"  ",r_lbl, "\n",sep="")
      cat(tab,"Probabilidad de superioridad PS = ",roundf(ip,decs), "\n",sep="")
      cat(tab,"(probabilidad de que un valor al azar de M1 sea ",ip_lbl," a un valor al azar de M2) \n",sep="")
    } else { #caso==2 muestras apareadas
      cat(tab,"Diferencia de localizaci\u00f3n:  (pseudo)mediana = ",roundf(wt$estimate,decs),sep="")
      cat(tab,"  95%-IC = (",roundf(wt$conf.int[1],decs),", ",roundf(wt$conf.int[2],decs),") \n", sep="" )
      cat(tab,"r = ",roundf(z/sqrt(2*n),decs),"; p ",ptxt_wx,"\n",sep="")

    }

  if(grf){

      ggboxplot<-ggplot(data=dfg, mapping=aes(x=g,y=x))+
        geom_boxplot()+
        labs(y="Puntuaciones",x="Grupo")
      ggdotplot<-ggplot(data=dfg, mapping=aes(x=g,y=x))+
        geom_dotplot(binaxis="y",stackdir="center")+
        stat_summary(fun.y=median, geom="point", shape=18,
                     size=3, color="red")+
        labs(y="Puntuaciones",x="Grupo")


      suppressMessages(show(ggboxplot))
      suppressMessages(show(ggdotplot))
  }
  options(warn = defaultW)
}

