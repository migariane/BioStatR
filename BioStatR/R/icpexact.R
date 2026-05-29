#' @noRd
#' @title IC(p) - metodo exacto de Clooper-Pearson
#' @description Obtencion del intervalo de confianza para una proporcion binomial a partir del metodo clasico de Wald ajustado (Texto intencionadamente sin tildes u otros caracteres especiales por la incompatibilidad de los mapas de caracteres)
#' @param x valor entero: numero de casos favorables (numerador de la proporcion binomial)
#' @param n valor entero: numero de casos posibles (denominador de la proporcion binomial)
#' @param conf valor real (0,1): nivel de confianza (parametro alternativo al error alfa, en tanto por uno). Por defecto =.95.
#' @param alfa valor real (0,1): error alfa (par?metro alternativo al nivel de confianza, en tanto por uno). Por defecto =.05.
#' @return Intervalo de confianza de Wald ajustado para estimar la proporcion binomial
#' @examples
#' icpexact(x=25, n=210)
#' icpexact(x=25, n=210, conf=.99)
#' icpexact(x=25, n=210, alfa=.01)
#'

icpexact<-function(x=0,n=0, conf=.95,alfa=.05, decs=3,eco=FALSE)
{
#  if(conf!=.95){alfa<- 1-conf} else {if (alfa!=0.05){conf<- 1-alfa}}
  epairsget(p=alfa,q=conf, pmin=0.0001,pdefault=0.05)->par_alfa
  if(par_alfa[[1]][1]){
    alfa<-par_alfa[[2]][1]
    conf<-par_alfa[[2]][2]
  } else {stop("valor de alfa o de conf incongruente")}


  zalfa<- qnorm(1 - (alfa / 2))
  res<-vector("list",length = 2)
  res[[1]]<-NA
  res[[2]]<-NA

  if((n>1 && n>x) && x>0){
    f1<-qf(alfa/2, (2*n-x+1),2*x,lower.tail = F)
    f2<-qf(alfa/2, 2*(x+1),2*(n-x),lower.tail = F)

    res[[1]]<-x/((x+(n-x+1)*f1))
    res[[2]]<-((x+1)*f2)/( (n-x) + (x+1)*f2 )

    if(res[[1]]<0){res[[1]]<-0}
    if(res[[2]]>1){res[[2]]<-1}

  }
  else {

  if(x==0 || x==n){
    if (x==0){
      res[[1]]<-0
      res[[2]]<-1-(alfa^(1/n))

    }
    else{
      res[[1]]<-alfa^(1/n)
      res[[2]]<-1
    }

  }

  else {cat("\n ERROR: No valid data \n")}
  }
  if(eco){
    tab<-"  "
    showtitle("Intervalo de confianza para una proporci\u00f3n binomial")
    cat(paste(tab,"M\u00e9todo: exacto",sep=""),"\n")
    cat(paste(tab,"Informaci\u00f3n muestral:",sep=""),"\n")
    cat(paste(tab,"Casos: x = ",x,",  Tama\u00f1o muestral: n = ",n,sep=""),"\n")
    ictxt<-txtic(ic=res, alfa=alfa, param=greek("p"),decs=decs,full=TRUE)
    cat(paste(tab,ictxt,sep=""),"\n")
    cat(paste(tab,"Radio del intervalo: ",roundf((res[[2]]-res[[1]])/2,decs),sep=""),"\n")
  }else{
    return(res)
  }
}
