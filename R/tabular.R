#' @noRd
#' @title tabular
#' @description Procedimiento auxiliar para generar tablas a partir de vectores o data.frames (Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres)
#' @param frecs vector o data.frame: frecuencias del interior de la tabla
#' @param fnum  entero positivo: numero de filas que ha de tener la tabla
#' @param filas valor logico (por defecto = TRUE): la lectura se hace por filas (TRUE) o por columnas (FALSE)
#' @return Data.frame con el formato indicado
#' @examples
#' # Obtencion de una tabla a partir de un vector
#' tabular(frecs=c(9,8,27,8,47,236,23,39,88,49,179,706,28,48,89,19,104,293),
#' fnum=3) -> tabla
#' tabla
#' # Obtencion de una tabla a partir de un data.frame con
#' #    col1 categorias por filas
#' #    col2 categorias por columnas
#' #    col3 frecuencias

tabular<-function(frecs=NULL, fnum=0, filas=TRUE){
  tt<-NULL
  if (!is.null(frecs)){

    if(is.numeric(frecs) && fnum>0){
      tt<-as.table(matrix(data=frecs,nrow=fnum, byrow=filas))
    }

    if(is.data.frame(frecs)){
      if(ncol(frecs)==1 && fnum>0){
        nc<-nrow(frecs)%/%fnum
        tt<-as.table(matrix(data=frecs,nrow=fnum,ncol=nc, byrow=filas))
      }
      else {
      if(ncol(frecs)==3){
        if(is.numeric(frecs[,3])){
          nr=length(table(frecs[,1]))
          nc=length(table(frecs[,2]))
          if(length(frecs[,3])%%nr==0){

            tt<-as.table(matrix(data=frecs[,3],nrow=nr,byrow=TRUE))
            rownames(tt)<-names(table(frecs[,1]))
            colnames(tt)<-names(table(frecs[,2]))
          }
        }
      }
      else {
        stop("Se espera un data.frame con tres columnas: [1] variable por filas, [2] variable por columnas y [3] frecuencia")
      }
        }
    }
  }
  as.table(tt)
}
