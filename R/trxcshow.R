#' @noRd
#' @title trxcshow
#' @description Procedimiento de obtencion y presentacion de una tabla de contingencia (Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres)
#' @param x tabla: frecuencias del interior de la tabla
#' @param fcat vector: nombres de las categorias por filas
#' @param ccat  vector: nombres de las categorias por columnas
#' @param decs valor entero (= 6): decimales a mostrar
#' @param tipo caracter: "F" = frecuencias; Porcentajes: "R" =filas, "C" =columnas, "T"=totales; Residuos: "S"=estandarizados, "P" de Pearson; "X" Contribucion X2. El parametro soporta caracteres en minuscula.
#' @param eco valor logico (= TRUE): muestra por pantalla la tabla
#' @param indent valor entero: espacios de indentacion de la tabla
#' @return Data.frame con el formato indicado
#' @examples
#'# Obtencion de la tabla de frecuencias observadas
#'x=matrix(c(11,12,13,21,22,23),nrow=2,byrow=TRUE)
#'
#'trxcshow(x,fcat=c("I","II"),ccat=c("A","B","C"),tipo="F")
#'
#'# Porcentajes por filas
#'trxcshow(x,fcat=c("I","II"),ccat=c("A","B","C"),tipo="R")
#'
#'# Porcentajes por columnas
#'trxcshow(x,fcat=c("I","II"),ccat=c("A","B","C"),tipo="C")
#'
#'# Porcentajes totales
#'trxcshow(x,fcat=c("I","II"),ccat=c("A","B","C"),tipo="T")


trxcshow <- function(x=NULL,fcat=NULL, ccat=NULL,decs=3,tipo="F",eco=FALSE,indent=2)
{
  nr <-nrow(x)
  nc <-ncol(x)
  dr <-min(nr,length(fcat))
  dc <-min(nc,length(ccat))
  tipo<-toupper(tipo)

  t_x<-as.table(matrix(0,nrow=nr+1,ncol=nc+1))
  for(i in 1:nr){
      for(j in 1:nc){
        t_x[[i,j]] <- x[[i,j]]
      }
    }
  t_t<-sum(t_x)
  for(i in 1:nr){t_x[[i,nc+1]]<-sum(x[i,])}
  for(j in 1:nc){t_x[[nr+1,j]]<-sum(x[,j])}
  t_x[[nr+1,nc+1]]<-t_t


  t2_x<-as.table(matrix(0,nrow=nr+1,ncol=nc+1))

  if(tipo=="F"){ t2_x<-t_x}

  if(tipo=="E" || tipo=="S"|| tipo=="P"||tipo=="X"){ #frecuencias esperadas / Residuos estandarizados / res. de Pearson / contribucion X2
    for(i in 1:nr){
      for(j in 1:nc){
        t2_x[[i,j]]<-round(t_x[[i,nc+1]]*t_x[[nr+1,j]]/t_x[[nr+1,nc+1]],decs)
      }
    }
    for(i in 1:nr){t2_x[[i,nc+1]]<-sum(x[i,])}
    for(j in 1:nc){t2_x[[nr+1,j]]<-sum(x[,j])}
    t2_x[[nr+1,nc+1]]<-t_x[[nr+1,nc+1]]<-t_t

    if(tipo=="S") {# residuos estandarizados
      r_x<-as.table(matrix(0,nrow=nr,ncol=nc))
      for(i in 1:nr){
        for(j in 1:nc){
          r_x[[i,j]] <- round((t_x[[i,j]]-t2_x[[i,j]])/sqrt(t2_x[[i,j]]*(1-(t_x[[i,nc+1]]/t_x[[nr+1,nc+1]]))*(1-(t_x[[nr+1,j]]/t_x[[nr+1,nc+1]]))),decs)
        }
      }
    } # end tipo==S

    if(tipo=="P"){# residuos de Pearson
      r_x<-as.table(matrix(0,nrow=nr,ncol=nc))
      for(i in 1:nr){
        for(j in 1:nc){
          r_x[[i,j]] <- round((t_x[[i,j]]-t2_x[[i,j]])/sqrt(t2_x[[i,j]]),decs)
        }
      }
    } # end tipo==P


    if(tipo=="X"){# contribucion X2 en %
      r_x<-as.table(matrix(0,nrow=nr,ncol=nc))
      x2<-0
      for(i in 1:nr){
        for(j in 1:nc){
          x2 <- x2 + ((t_x[[i,j]]-t2_x[[i,j]])^2)/t2_x[[i,j]]
        }
      }
      for(i in 1:nr){
        for(j in 1:nc){
          r_x[[i,j]] <- round((((t_x[[i,j]]-t2_x[[i,j]])^2)/t2_x[[i,j]])/x2,decs)
        }
      }
    } # end tipo==X

  }

  if(tipo=="R"){ #porcentajes por filas
    for(i in 1:nr){
      for(j in 1:nc){
        t2_x[[i,j]]<-round(t_x[[i,j]]/t_x[[i,nc+1]],decs)
      }
    }
    for(i in 1:nr){t2_x[[i,nc+1]]<-round(t_x[[i,nc+1]]/t_x[[i,nc+1]],decs)}
    for(j in 1:nc){t2_x[[nr+1,j]]<-round(t_x[[nr+1,j]]/t_x[[nr+1,nc+1]],decs)}
    t2_x[[nr+1,nc+1]]<-round(t_x[[nr+1,nc+1]]/t_x[[nr+1,nc+1]],decs)
  }

  if(tipo=="C"){ #porcentajes por columnas
    for(j in 1:nc){
      for(i in 1:nr){
        t2_x[[i,j]]<-round(t_x[[i,j]]/t_x[[nr+1,j]],decs)
      }
    }
    for(i in 1:nr){t2_x[[i,nc+1]]<-round(t_x[[i,nc+1]]/t_x[[nr+1,nc+1]],decs)}
    for(j in 1:nc){t2_x[[nr+1,j]]<-round(t_x[[nr+1,j]]/t_x[[nr+1,j]],decs)}
    t2_x[[nr+1,nc+1]]<-round(t_x[[nr+1,nc+1]]/t_x[[nr+1,nc+1]],decs)
  }

  if(tipo=="T"){ #porcentajes totales
    for(j in 1:nc){
      for(i in 1:nr){
        t2_x[[i,j]]<-round(t_x[[i,j]]/t_x[[nr+1,nc+1]],decs)
      }
    }
    for(i in 1:nr){t2_x[[i,nc+1]]<-round(t_x[[i,nc+1]]/t_x[[nr+1,nc+1]],decs)}
    for(j in 1:nc){t2_x[[nr+1,j]]<-round(t_x[[nr+1,j]]/t_x[[nr+1,nc+1]],decs)}
    t2_x[[nr+1,nc+1]]<-round(t_x[[nr+1,nc+1]]/t_x[[nr+1,nc+1]],decs)
  }


  ##Ajuste final
  tab<-""
  for (i in 1:indent) tab<-paste(tab," ",sep="")

  if(tipo=="S" || tipo=="P"|| tipo=="X"){t2_x<-r_x}
  if (dr>0) for(i in 1:dr) {rownames(t2_x)[i]<-paste(tab,fcat[i],sep="")}
  if (dc>0) for(j in 1:dc) {colnames(t2_x)[j]<-paste(tab,ccat[j],sep="")}
  if(tipo !="S" && tipo !="P" && tipo!="X"){
    colnames(t2_x)[nc+1]<-"Total"
    rownames(t2_x)[nr+1]<-paste(tab,"Total",sep="")
  }
  if(eco) {print.table(t2_x)}
  t2_x

}
