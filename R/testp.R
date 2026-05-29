#' @title Test con proporciones
#' @description Test para una proporcion o estudio de la homogeneidad de dos proporciones binomiales
#'              independientes o dos proporciones apareadas.
#'              (Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres)
#' @param x valor entero de casos favorables cuando se trata de una unica muestra o vector binario de observaciones .
#' @param n valor entero = tamano de muestra cuando esta es unica o vector con la tabla para test de McNemar
#' @param x1 vector: variable por filas en la tabla de contingencia 2x2 a elaborar.
#' @param x2 vector: variable por columnas en la tabla de contingencia 2x2 a elaborar.
#' @param grupos vector: variable de agrupacion o indicacion a la variable x1 o x2 que haga esta funcion
#' @param x1 valor entero o vector: casos favorables en la muestra 1 o vector de observaciones.
#' @param n1 valor entero: tamano de la muestra 1.
#' @param x2 valor entero o vector: casos favorables en la muestra 2 o vector de observaciones
#' @param n2 valor entero: tamano de la muestra 2.
#' @param x0 indicador de la categoria a analizar
#' @param p0 valor de la proporcion para estimar n exclusivamente
#' @param par valor logico: indicador de muestras apareadas
#' @param alfa real en (0,1): Nivel de error de los intervalos (alternativa a conf)
#' @param conf real en (0,1): Nivel de confianza de los intervalos (alternativa a alfa)
#' @param beta real en (0,1): Nivel de error de tipo II (alternativa a indicar la potencia)
#' @param potencia real en (0,1): Nivel de potencia (alternativa a indicar el error de tipo II)
#' @param delta real en (0,1): Diferencia a detectar (tamano del efecto)
#' @param decs entero: Numero de decimales en las salidas
#' @importFrom stats  na.omit
#' @return Informe con test e IC, estudio de la fiabilidad de la no significacion y tamano de muestra
#' @export testp
#' @examples
#'
#' # test con una muestra
#' testp(x=30,n=70,p0=0.4)
#' testp(x=30,n=70,p0=0.4,delta=0.05,potencia=0.9)
#'
#' modos<-c("A","A","A","A","A","A","A","A","A","A","B","B","B","B")
#' edad <-c(5,4,8,6,7,4,5,4,5,6,9,6,5,4,5,6,4)
#'
#' testp(x=modos, p0=0.4,delta=0.05,potencia=0.9)
#' testp(x=edad, x0=6,p0=0.5, delta=0.05,potencia=0.9)
#' testp(x=as.factor(edad), x0=6,p0=0.5, delta=0.05,potencia=0.9)
#'
#' testp(p0=0.65,delta=0.05,potencia=0.9)  #solo tamano de muestra
#' testp(p0=0.5,delta=0.05,alfa=0.05,potencia=0.9)
#'
#'# 2 muestras independientes ---
#' fumaH=c("S","N","N","N","N","N","S","N","S","N","S","N","S","N","S","N","N")
#' fumaM=c("N","N","N","N","N","N","S","S","S","S","S","N","N","S","N","N","S")
#' testp(x1=fumaH,x2=fumaM)

#' testp(x1=fumaH,x2=fumaM,x0="S")
#'
#' sexo=c("H","H","H","M","M","H","M","M","H","H","M","M","M","M","H","M",
#'        "M","M","M","M","H","M","H","H","H","H")
#' fuma=c("S","N","S","S","N","N","S","N","S","S","N","N","N","N","S","N",
#'        "N","N","S","N","S","N","S","N","N","N")
#' testp(x=fuma,grupos=sexo, x0="S")
#' testp(x=fuma,grupos=sexo, x0="N")
#'
#' a<-rbinom(250,1,0.4)
#' b<-rbinom(250,1,0.6)
#' testp(x1=a,x2=b,x0=1)
#'
#'
#'
#'#'# 2 muestras apareadas ----
#' testp(n=c(12,35,43,20), par=TRUE, delta=0.05)
#' testp(x1=150,n1=450,x2=34,n2=49, par=TRUE)
#'
#' fuma1<-c("S","S","S","N","S","N","S","S","S","N","S","N","S","N","S","N","N",
#'          "N","N","S","N","N","N","S","S","S","S","S","N","N","S","N","N","S")
#' fuma2<-c("N","N","S","N","N","S","S","N","S","N","S","N","S","N","N","N","N",
#'          "N","N","N","N","N","S","N","N","N","N","N","N","N","N","N","N","S")
#' testp(x1=fuma1,x2=fuma2,par=TRUE)
#'

testp=function(x=NULL,n=0,x1=NULL,n1=0,x2=NULL,n2=0,grupos=NULL, x0=NA, p0=0.5,par=FALSE, alfa=0.05,conf=1-alfa,delta=0,beta=0.20,potencia=1-beta, decs=3)
{
  #cabecera
  { tab="  "
    w_txt<-""
    msge<-""
    done<-FALSE

    #flags
    null_x<-is.null(x)
    null_x1<-is.null(x1)
    null_x2<-is.null(x2)
    null_grupos<-is.null(grupos)

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

    if(delta>0) {
      if(inrango(delta,0.001,1)==FALSE) stop("El valor de delta debe estar comprendido entre 0 y 1")}
  } # end cabecera


  # Determinacion del numero de muestras

  if(null_grupos && null_x1 && null_x2 && null_x && length(n)==1) {
    if(delta>0 && p0>0) n1p(p0=p0,alfa=alfa,delta=delta,beta=beta,decs=decs,context=FALSE)
    done<-TRUE
  }

  # 1 muestra
  # /////////////////////////
  if(!done && !null_x && null_grupos) {
    if(length(x)>1) {
      x_name<-deparse(substitute(x))
      m<-getxn(m=x,x0=x0,varname=x_name)
      w_txt<-m[[2]][1]
      if(m[[3]][1]) stop(w_txt)
      x<-m[[1]][1]
      n<-m[[1]][2]
    }else{
      if(x<1 || n<=x) stop("Test con una muestra. Valores de x y/o n inadecuados.")
    }
    test1p(x=x,n=n,x0=x0,p0=p0,alfa=alfa,delta=delta,beta=beta,decs=decs,msge=w_txt)
    test_2pind<-FALSE
    done<-TRUE
  }


  # 2 Muestras
  # /////////////////////////
  if(!done){
    # [1] Entrada de datos resumidos
    if(length(n)==4){
      if(par){
        testmcnemar(n=n,alfa=alfa, decs=decs, delta=delta, beta=beta)
        test_2pind<-FALSE
      } else {
        g_name <-"Muestra"
        g1_name<-"I"
        g2_name<-"II"

        r_name <-"Respuesta"
        x0  <-"R+"
        x0_ <-"R-"
        x1<-n[[1]]
        n1<-n[[1]]+n[[2]]
        x2<-n[[3]]
        n2<-n[[3]]+n[[4]]
        test_2pind<-TRUE
      }
      done<-TRUE
    }

    if(!done && null_grupos && !null_x1 && !null_x2 && (n1>0 && n2>0)){

      if(par){#pareadas
        testmcnemar(n11=x1, n12=n1-x1, n21=x2, n22=n2-x2,alfa=alfa, decs=decs, delta=delta, beta=beta)
        test_2pind<-FALSE
        done<-TRUE

      }else{# independientes

        if(length(x1)!=1  || length(x2)!=1 || length(n1)!=1 || length(n2)!=1) stop("x1, n1 y x2, n2 deben ser valores enteros.")
        if(x1<=0 || x2<=0 || n1 <=0 || n2<=0 || x1>=n1 || x2>=n2) stop("x1, n1 y x2, n2 deben ser enteros positivos con x1<n1 y x2<n2.")
        g_name <-"Muestra"
        g1_name<-"I"
        g2_name<-"II"

        r_name <-"Respuesta"
        x0  <-"R+"
        x0_ <-"R-"
        test_2pind<-TRUE
        done<-TRUE}

    }   # end [1]

    # [2] Entrada de dos vectores independientes de variable respuesta
    # '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    if(!done && null_grupos && (!null_x1 && !null_x2)) {
     if(par){
       x1_name<-deparse(substitute(x1))
       x2_name<-deparse(substitute(x2))
       if(length(x1)==length(x2)){
         testmcnemar(pre=x1,post=x2,alfa=alfa, decs=decs, delta=delta, beta=beta,lbls=c(x1_name,x2_name))
       }else{ stop("Los vectores ",x1_name," y ",x2_name," deben de tener la misma longitud")}
       test_2pind<-FALSE
       done<-TRUE

     } else { #2 muestras ind

       cat(paste("\n[!] Las muestras ",deparse(substitute(m1))," y ",deparse(substitute(m2))," tienen el mismo tama\u00f1o, pero no se ha indicado",sep=""),"\n")
       cat(paste("    par=TRUE. Se asume que las muestras son independientes."),"\n")

       g_name <-"Muestra"
       g1_name<-deparse(substitute(x1))
       g2_name<-deparse(substitute(x2))

       r_name <-"Respuesta"

       x1_f<-as.factor(x1[!is.na(x1)])
       x2_f<-as.factor(x2[!is.na(x2)])
       x1_levels<-levels(x1_f); x1_nl<-length(x1_levels)
       x2_levels<-levels(x2_f); x2_nl<-length(x2_levels)

       # verificar que x0 esta en los dos vectores
       set_i<-intersect(x1_levels, x2_levels)
       set_u<-union(x1_levels, x2_levels)

       if(!is.na(x0)){
         if(length(x0)!=1) stop("Referencia no v\u00e1lida en x0.")
         x0<-levels(as.factor(x0))
         if (length(intersect(set_i, x0))==0) stop(paste("No se encuentra la categor\u00eda ",x0," en alguno de los vectores de datos ",g1_name," o ",g2_name,".", sep=""))
         # aqui, x0 es categor?a licita y se puede seguir
       }else{ # no se ha especificado x0, se elige el primer nivel
         if (length(set_i)==0) stop(paste("Los vectores de datos ",g1_name," y ",g2_name," no tienen ninguna categor\u00eda en com\u00fan"))
         if (length(set_i)>0){
           x0<-set_i[1]
           if (length(set_i)==1) w_txt<-addline(w_txt,paste("Los vectores ",g1_name," y ",g2_name," solo tienen la categor\u00eda ",x0,"en com\u00fan. Es analiza ",x0,".",sep=""))
           if (length(set_i)==2) w_txt<-addline(w_txt,paste("No se ha especificado la categor\u00eda a estudiar. Se considera '",x0,"'",sep=""))
         }
       }
       if (length(set_u) >2) {
         if(nlevels(x1_f)>2) w_txt<-addline(w_txt,paste("La variable ",g1_name," no es binaria",sep=""))
         if(nlevels(x2_f)>2) w_txt<-addline(w_txt,paste("La variable ",g2_name," no es binaria",sep=""))
       }

       #x0 es categor?a licita. Obencion de la tabla
       if (length(set_u[set_u!=x0])==1) x0_<-set_u[set_u!=x0]
       if (length(set_u[set_u!=x0]) >1) x0_<-paste(x0,"_",sep="")

       n1<-length(x1_f)
       n2<-length(x2_f)
       x1<-length(x1_f[x1_f==x0])
       x2<-length(x2_f[x2_f==x0])
       test_2pind<-TRUE
       done<-TRUE
     }
    } # end [2]
    # '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

    # [3] Entrada de un vector respuesta y un vector de agrupacion
    # '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    if(!done && !null_grupos){
      if(!null_x1 && !null_x2){
        r_name <-"Respuesta"
        x1_name<-deparse(substitute(x1))
        x2_name<-deparse(substitute(x2))
        g_name <-deparse(substitute(grupos))

        if(length(x1) != length(x2)) stop(paste("Las variables ",x1_name," y ",x2_name," no tienen la misma longitud.",sep=""))

        if (g_name==x1_name){
          datos<-data.frame(grupos<-as.factor(x1), x<-as.factor(x2))
        }else{
          datos<-data.frame(grupos<-as.factor(x2), x<-as.factor(x1))
        }
      } else { #x1 y x2 son null, se usa x

        if(null_x) stop("Especificaci\u00f3n incorrecta, al declarar 'grupos' se deben indicar los datos en 'x'")
        x_name<-deparse(substitute(x))
        r_name<-deparse(substitute(x))
        g_name <-deparse(substitute(grupos))
        if(length(grupos) != length(x)) stop(paste("Las variables ",g_name," y ",x_name," no tienen la misma longitud.",sep=""))
        datos<-data.frame(grupos<-as.factor(grupos), x<-as.factor(x))
      }

      datos<-na.omit(datos)
      #aqui ya hay un dataframe datos con las columnas (grupos,x)
      grupos_levels<-levels(datos$grupos)
      if(length(grupos_levels)==1) stop("La variable de agrupaci\u00f3n solo presenta un grupo.")
      if(length(grupos_levels) >2) stop(paste("La variable de agrupaci\u00f3n presenta ",length(grupos_levels)," grupos.",sep=""))
      g1_name<-grupos_levels[1]
      g2_name<-grupos_levels[2]


      x_levels<-levels(datos$x)
      if(!is.na(x0)){
        if(length(x0)!=1) stop("Referencia no v\u00e1lida en x0.")
        if (length(intersect(x_levels, x0))==0) stop(paste("No se encuentra la categor\u00eda ",x0," en los niveles de ",x_name,".", sep=""))
      } else { x0<-x_levels[1] }
      #aqui ya es licito x0

      if(length(x_levels) <2) stop(paste("La variable ",x_name," presenta una sola categor\u00eda.",sep=""))
      if(length(x_levels)==2) {x0_<-x_levels[x_levels!=x0]}
      if(length(x_levels) >2){
        w_txt<-paste("La variable ",x_name," presenta m\u00e1s de dos categor\u00edas. Se analiza ",x0,".",sep="")
        x0_<-"x_"
      }

      x_grupo1<-datos$x[datos$grupos==grupos_levels[1]]
      x_grupo2<-datos$x[datos$grupos==grupos_levels[2]]

      n1<-length(x_grupo1)
      n2<-length(x_grupo2)

      x1<-length(x_grupo1[x_grupo1==x0])
      x2<-length(x_grupo2[x_grupo2==x0])
      test_2pind<-TRUE
      done<-TRUE

    }#done [3]
    # '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

    # Aqui, la entrada ya debe tener formato x1, n1, x2, n2 con x1,x2 el recuento
    # de casos que son x0 en cada grupo
    if(test_2pind){
      tabla=as.table(matrix(c(x1,n1-x1,x2,n2-x2),nrow=2,byrow=TRUE))
      row.names(tabla)<-c(g1_name,g2_name)
      colnames(tabla)<-c(x0,x0_)

      fvar<-c(g_name, g1_name, g2_name); fvarinfo<-paste(g_name,"['",g1_name,"','",g2_name,"']", sep="")
      cvar<-c(r_name, x0, x0_);          cvarinfo<-paste(r_name,"['",     x0,"','",    x0_,"']", sep="")

      t<-trxcshow(x=tabla,fcat=row.names(tabla),ccat=colnames(tabla),tipo="F",eco=FALSE)

      showtitle("Test para contrastar la diferencia de dos proporciones independientes",lev=1)
      showtitle("Informaci\u00f3n muestral", lev=2)
      wmsge(w_txt)
      cat("\n")
      cat(paste(tab,"Tabla de contingencia de ",cvarinfo," x ",fvarinfo,sep=""),"\n")
      nsep<-8-nchar(g_name)
      if(nsep<=0) nsep<-1
      sep<-setstr(" ",nsep)
      cat(tab,g_name,sep,r_name,sep="","\n")
      print(t)
      cat("\n")
      test2p(x1=x1,n1=n1,x2=x2,n2=n2,alfa=alfa,delta=delta,beta=beta,fvar=fvar,cvar=cvar,decs=decs)
    }
  }#end 2 muestras

}









