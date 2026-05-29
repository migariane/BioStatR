
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCIONES NUMERICAS # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#' @noRd
#' @param x valor a comprobar
#' @param rmin minimo del rango en donde debe estar x
#' @param rmas minimo del rango en donde debe estar x
#' @return valor booleano
inrango<-function(x=0,rmin=0,rmax=0){
  result<-((rmin<=rmax)&&(x<=rmax)&&(x>=rmin))
  return(result)
}

#' @noRd
#' @title .epairschk
#' @description permite verificar la coherencia de parametros de entrada que deben sumar 1
#' @param p probabilidad menor (alfa o beta)
#' @param q probabilidad mayor (conf. o potencia)
#' #' @return valor booleano indicando la coherencia de p y de q
epairschk<-function(p=0.05,q=1-p,pmin=0.0001,pmax=1-pmin) {
  inrangop<-inrango(p,pmin,pmax)
  inrangoq<-inrango(q,pmin,pmax)
  sumchk<-(p+q)==(pmin+pmax)
  result<-(inrangop && inrangoq && sumchk )
  return(result)

}
#' @noRd
#' @description devuelve una pareja coherente de probs p y q tq p+q=1
#' @param p probabilidad menor (alfa o beta)
#' @param q probabilidad mayor (conf. o potencia)
#' @param pmin tolerancia para evitar probs nulas
#' @param pdefault valor de p que sirve de base para discriminar cual se ha cambiado
#' @return lista con valor booleano, valor de p y valor de q
#' @examples
#' .epairsget(0.05,0.99,pdefault=0.05)
epairsget<-function(p=0.05,q=1-p,pmin=0.0001,pmax=1-pmin, pdefault=0.05){
  if(p!=pdefault) {q<-1-p} else {if(q!=(1-pdefault)){p<-1-q}}
  chk<-epairschk(p=p,q,pmin=pmin,pmax=pmax)
  result<-list()
  rp<-p
  rq<-1-rp
  rres<-chk
  result<-list(rres, c(rp,rq))
  return(result)
}
#' @noRd
roundf = function(x=0, digits=0,lt=FALSE){
  result<-format(round(as.numeric(x), digits), nsmall = digits)
  if(lt){
    tol<-10^(-digits)
    if (round(x,digits)<tol) result<-paste("<",tol,sep="")
  }
  return(result)
}

#' @noRd
#' @examples
#' ptxt(pval=0.0001,decs=3,eq=2)
#' ptxt(pval=0.01,decs=3,eq=2)
#'
ptxt<-function(pval=0,decs=3,eq=1,param="p"){
  #devuelve el valor de p en formato texto
  # eq se hace numerico, originalmente era T/F
  # eq= 0 o F no muestra el signo igual
  # eq= 1 o T muestra el signo igual
  # eq=2  muestra el formato completo, con p, por ej: p = 0.002
  pmin<-10^-decs
  if(pval>1) pval=1.0
  if(pval<pmin){result<-paste(" < ",roundf(pmin,decs),sep="")}
  else {
      if(eq==0) result<-paste(      roundf(pval,decs),sep="")
      if(eq >0) result<-paste(" = ",roundf(pval,decs),sep="")
  }
  if(eq>=2) result<-paste(param,result,sep="")
  return(result)
}

#' @noRd
nearest<-function(x=0,a=0,b=0,getlimit=FALSE){
  #Si getlimit==F devuelve el valor de (a,b) mas proximo a x
  #Si getlimit==T devuelve el limite de (a,b) mas proximo a x
  if (inrango(x,a,b) && (getlimit==FALSE)) {
    result<-x
  }else{
    aa<-abs(x-a)
    ab<-abs(x-b)
    ifelse (aa<=ab, result<-a, result<-b)
  }
  return(result)
}

#' @noRd
ic2p_maxvar<-function(ic1,ic2){
  # se devuelve el limite que hay que elegir. 1 el superior, -1 el inferior
  # si se devuelve 0, representa el caso sin info (ej, si x esta en algun intervalos)
  # delta no se maneja de momento
  x<-0.5
  if(inrango(x,ic1[[1]],ic1[[2]])){
    pos1<-0
  }else{
    ifelse(ic1[[1]]>x, pos1<- 1,pos1<- -1)
  }
  if(inrango(x,ic2[[1]],ic2[[2]])){
    pos2<-0
  }else{
    ifelse(ic2[[1]]>x, pos2<- 1,pos2<- -1)
  }
  if((pos1*pos2)<= 0) { #estan uno a cada lado de 0.5 o conteniendo a 0.5
    result<-0
  }else{ #los dos ic al mismo lado y sin contener a 0.5
    ifelse((pos1==1), result<- -1, result<- 1)
    #se devuelve el limite que hay que elegir. 1 el superior, -1 el inferior
  }
  return(result)
  #ej: .ic2p_maxvar(ic1=c(0.3,0.4),ic2=c(0.6,0.7))
}
#' @noRd
#' @description transpone un data.frame o matriz
tr_df<-function(m=NULL){
  nr<-nrow(m)
  nc<-ncol(m)
  x<-matrix(rep(0,times=nc*nr),nrow=nc)
  for(i in 1:nr){
    for(j in 1:nc){
      x[[j,i]]<-m[[i,j]]
    }
  }
  return(as.data.frame(x))
}

#' @noRd
#' @description dicotomiza al vector m usando como criterio x0 o el primer valor que aparezca cuando solo hay dos distintos.
#' @examples
#' .getxn(m=as.factor(c(1,2,3,4,5,6,7)),x0=4,eco=TRUE)->kk
#' .getxn(m=c(1,2,3,4,5,6,7),x0=4,eco=TRUE)->kk
#' .getxn(m=1,x0=1,eco=TRUE)->kk
#'
getxn<-function(m=NULL,x0=NA,varname=NA,eco=FALSE){
 #dicotomiza al vector m usando como criterio x0 o el primer valor que aparezca cuando solo hay dos distintos
 #si m es factor, se dicotomiza con valores =x0
 #si m es numerico, se dicotomiza con valores <=x0
 #devuelve {x, n, texto de mensaje, indicador binario de error}

 warning_txt=""
 iserr<-FALSE
 # ////////////
 if(length(m)>1){
   #m_name<-deparse(substitute(m))
   m_name<-varname
   n<-length(m)
   if(is.character(m)) m<-as.factor(m)

   if(!is.na(x0)){ #con valor de x0
     if(is.factor(m)){
       x<-length(m[m==x0])# si x es factor se busca la coincidencia
       warning_txt<-paste("Inferencia sobre la proporci\u00f3n \u03c0 de valores ",m_name," = '",x0,"'",sep="")

     }else{ # se espera m numerico
       if(is.numeric(m)) {
         x<-length(m[m<=x0])
         warning_txt<-(paste("Inferencia sobre la proporci\u00f3n \u03c0 de valores ",m_name," \u2a7d ",x0,sep=""))
       } else {
         x<-NA
         warning_txt<-paste("El vector ",m_name," debe ser de tipo num\u00e9rico o de tipo factor.",sep="")
         iserr<-TRUE
       }
     }

   }else{ # sin x0
     if(length(unique(m))==2){
       x_lbl<-m[1]
       warning_txt<-paste("No se ha especificado un criterio en x0. Se considera ",m_name," = '",x_lbl,"'",sep="")
       n<-length(m)
       x<-length(m[m==m[1]])
     } else{
       x<-NA
       warning_txt<-paste("El vector ",m_name," presenta m\u00e1s de dos valores distintos. Se debe especificar un criterio x0.",sep="")
       iserr<-TRUE
     }
   }
 }else{ #length(m)=1
  n<-1
  ifelse(m==x0,x<-1,x<-0)
  warning_txt<-"Solo hay una observaci\u00f3n."
  iserr<-TRUE
 }

 # - - - - - - - - - - -
 xn<-c(x,n)

 result<-list(xn,warning_txt,iserr)
 if(eco){
    cat(result[[2]][1],"\n")
    cat(" x = ",result[[1]][1],"\n")
    cat(" n = ",result[[1]][2],"\n")
    cat("genera error: ",result[[3]][1],"\n")
 }
 return(result)
}


#' @noRd
resumen_medias<-function( n=NULL,nmiss=NULL,m=NULL,s=NULL,sem=NULL,cpc=NULL,ic=FALSE,alfa=0.95,tab="  ",vnames=NULL,params=NULL,footnote=TRUE, decs=3){
  filas=length(n)
  for(i in 1:length(vnames)){vnames[[i]]<-paste(tab,vnames[[i]],sep="")}
  tabla<-data.frame(n=n)
  haymiss<-FALSE
  if(!is.null(nmiss)){
    if (sum(nmiss)>0) {
      tabla<-data.frame(tabla,nmiss=nmiss)
      haymiss<-TRUE}
  }
  tabla<-data.frame(tabla,media=round(m,decs), dt=roundf(s,decs))
  if(!is.null(sem)) tabla<-data.frame(tabla,sem=round(sem,decs))

  isvac<-TRUE
  if(!is.null(cpc)){
    if(sum(as.numeric(cpc))>0) {
      tabla<-data.frame(tabla,cpc=round(cpc,decs))
      isvac<-FALSE
    }
  }
  if(ic){
    iclist=" "
    for(i in 2:filas) iclist<-c(iclist," ")
    for(i in 1:filas){
        ici<-icm(m=m[[i]],n=n[[i]],s=s[[i]],alfa=alfa,decs=decs,vac=isvac, eco=FALSE)
        if(!is.null(params)) param<-params[[i]] else param=""
        icitxt<-txtic(ici,alfa=alfa,param=param,decs=decs,full=FALSE)
        iclist[[i]]<-icitxt
    }
    tabla<-data.frame(tabla,IC=iclist)
  }
  paramslist<-""
  if(!is.null(params)){
    # tabla<-data.frame(tabla,parametro=params)
    for(i in 1:(filas-1)) {
      ifelse(i==(filas -1),coma<-"",coma<-", ")
      paramslist<-paste(paramslist,params[[i]],coma,sep="")
    }
    paramslist<-paste(paramslist," y ", params[[filas]],sep="")
  }

  row.names(tabla)<-vnames
  print(tabla)
  #cat("\n")
  if(footnote){
    cat(paste(tab,"____",sep=""),"\n")
    cat(paste(tab,"* IC elaborados al ",(1-alfa)*100,"% de confianza para estimar ",paramslist," respectivamente",sep=""),"\n")
    if(haymiss){
    cat(paste(tab,"**En el recuento n ya se han excluido los valores faltantes",sep=""),"\n")
    }
  }
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# FUNCIONES DE CADENA # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#' @noRd
setstr<-function(car="",long=0){
  # devuelve un string de car repetido long veces
  s<-""
  for(i in 1:long) s<-paste(s,car,sep="")
  return(s)
}
#' @noRd
showtitle<-function(txt="",lev=1,tab=""){
  prefijo<-"# "
  uline<-"-"
  if(lev==1){
    cat("\n")
    cat(tab,prefijo,txt,sep="","\n")
    cat(tab,prefijo,setstr(uline,nchar(txt)),sep="","\n")
  }
  if(lev==2){
    cat("\n")
    cat(tab,prefijo,txt,sep="","\n")
  }
}
#' @noRd
istxt<-function(txt=""){
  return(nchar(txt[1])>1)
}
#' @noRd
addline<-function(txt="",newline=""){
  # verifica si txt contiene texto y lo crea o a?ade una linea en funcion de ello
  if(length(txt)>1){
    result<-c(txt,newline)
  }else{
    if(nchar(txt)==0) result<-newline else result<-c(txt,newline)
  }
  return(result)
}

#' @noRd
#' @examples
#' wmsge("esto es un aviso")
#' m<-c("esta es la primera linea","esta es la segunda")
#' wmsge(m)
#' pp<-""
#' addline(pp,"hola")->pp
#' addline(pp,"adios")->pp
#' pp
#' wmsge(pp)
wmsge<-function(wtext=NULL ,tab="  ",w=TRUE,sep=TRUE){
  #muestra avisos, wtext puede ser un texto o un vector de textos

  if(w){
    avisotxt<-"[!] "
    ncharavisotxt<-nchar(avisotxt)
    tab2<-setstr(" ",ncharavisotxt)
  } else{
    avisotxt<-""
    tab2<-""
  }

  if(length(wtext)>1){
    if(nchar(wtext[[1]])==0) {wtext<-wtext[-1]}

    if(sep) cat("\n")
    cat(tab,avisotxt,wtext[[1]],sep="","\n")
    if(length(wtext)>=2){
      for(i in 2:length(wtext)){
        cat(tab,tab2,wtext[[i]],sep="","\n")
      }
    }
    if(sep) cat("\n")
  } else {
    if(length(wtext)>0)
    {if(nchar(wtext)>1){
      if(sep) cat("\n")
      cat(paste(tab,avisotxt,wtext,"\n",sep=""))
      if(sep) cat("\n")
    }}
  }

}

#' @noRd
txtic<-function(ic=NULL, alfa=0.05, param="",decs=3,full=TRUE){

  conf<-paste(round((1-alfa)*100),"%",sep="")
  if(nchar(param)>=1){
    ictxt<-paste("IC(",param,")",sep="")
  } else {
    ictxt<-"IC"
  }
  head<-paste(conf,"-",ictxt," = ",sep="")
  tail<-paste("(",round(ic[[1]],decs),", ",round(ic[[2]],decs),")",sep="")
  ifelse(full, result<-paste(head,tail,sep=""),
               result<-tail)
  return(result)
}

#' @noRd
get_diagram_ic<-function(ic=NULL,id=NULL,m0=0,potencia=NA,param="",eco=TRUE)
{
#antiguo .getictxt
  ic.inf=ic[[1]]
  ic.sup=ic[[2]]
  id.inf=id[[1]]
  id.sup=id[[2]]


  ley1<-" --(---)--    --[---|---]-- "
  if(m0==0){ley2<-"  IC- IC+      -\u03b4   0  +\u03b4"}
  else     {ley2<-paste("  IC- IC+     ",param,"\u2080-\u03b4  ",param,"\u2080  ",param,"\u2080+\u03b4",sep="")}

  ifelse(!is.null(potencia), p<-paste(potencia*100,"%",sep=""), p<-"")
  txt0a<-paste("---[-(-|-)-]----","    potencia > ",p,sep="")
  txt0b<-paste("---[--|(-)-]----","    potencia > ",p,sep="")
  txt0c<-paste("---[-(-)|--]----","    potencia > ",p,sep="")

  txt1a<-paste("---[---|-(-]-)--","    potencia < ",p,sep="")
  txt1b<-paste("---[--(|---]-)--","    potencia < ",p,sep="")

  txt2a<-paste("-(--[-)-|---]---","    potencia < ",p,sep="")
  txt2b<-paste("-(--[--|)---]---","    potencia < ",p,sep="")

  txt4<-paste("-(-[---|---]-)--","    potencia < ",p,sep="")
  txt5<-paste("--[---|---]-(-)-","    potencia < ",p,sep="")
  txt6<-paste("-(-)-[--|--]---","     potencia < ",p,sep="")

  txt<-""
  if((ic.inf<=id.inf) && (ic.sup>=id.sup)){ txt<-txt4  }
  else{
    if((ic.inf>=id.inf) && (ic.sup<=id.sup)){
      txt<-txt0a
      if(ic.inf>m0){txt<-txt0b}
      if(ic.sup<m0){txt<-txt0c}
    }
    else{
      if((ic.inf>=id.sup) || (ic.sup<=id.inf)){
        if(ic.inf>=id.sup) {txt<-txt5}
        if(ic.sup<=id.inf) {txt<-txt6}
      }
      else {
        if(ic.inf<=id.inf){
          if(ic.sup<=m0){txt<-txt2a}
          else          {txt<-txt2b}}

        else{
          if(ic.inf>=m0){txt<-txt1a}
          else          {txt<-txt1b}}
      }
    }
  }
  diagrama<-c(txt,ley1,ley2)
  if(eco){
     cat("\n")
     cat("    ",diagrama[[1]],"\n")
     cat("\n")
     cat("    Leyenda: ",diagrama[[2]],"\n")
     cat("             ",diagrama[[3]],"\n")
  }

  return(diagrama)
  # getictxt(ic=c(-1,10),id=c(-2,2),m0=0,potencia=0.85,param="\u03bc")->pp
  # pp[[2]]
  # pp[[3]]
}

#' @noRd
#' @examples
#' tt<-"hola"
#' qq<-"y otro"
#' cat(wline(c(tt,"un texto",qq),tab=5))
#'
wline<-function(txt="",tab=2,show=TRUE){
  indent<-setstr(" ",tab)
  result=indent
  for(i in 1:length(txt)){
    result<-paste(result," ",txt[[i]],sep="")
  }
  result<-paste(result,"\n")
  if(show) cat(result)
  else return(result)
}
#' @noRd
wfoot<-function(txt="",line=TRUE,len=4,tab=2,show=TRUE){
  linea <-setstr("_",len)
  indent<-setstr(" ",tab)
  if(line) cat(paste(indent,linea,sep="","\n"))
  if(nchar(txt>0)) cat(paste(indent,txt,sep="","\n"))
}


#' @noRd
#'
greek<-function(s=NA,sub=NA){
  c_done<-FALSE
  n_done<-FALSE
  car<-""
  n<-""

  if(!is.na(s)){
    if(s=="a"){ car<-"\u03b1"; c_done<-TRUE}
    if(s=="b"){ car<-"\u03b2"; c_done<-TRUE}
    if(s=="d"){ car<-"\u03b4"; c_done<-TRUE}
    if(s=="l"){ car<-"\u03bb"; c_done<-TRUE}
    if(s=="m"){ car<-"\u03bc"; c_done<-TRUE}
    if(s=="p"){ car<-"\u03c0"; c_done<-TRUE}
    if(s=="t"){ car<-"\u03b8"; c_done<-TRUE}
    if(s=="H"){ car<-"H";      c_done<-TRUE}
    if (!c_done) car<-s}

  if(!is.na(sub)){
    if(sub==0){ n<-"\u2080"; n_done<-TRUE}
    if(sub==1){ n<-"\u2081"; n_done<-TRUE}
    if(sub==2){ n<-"\u2082"; n_done<-TRUE}
    if(sub==11){ n<-"\u2081\u2081"; n_done<-TRUE}
    if(sub==12){ n<-"\u2081\u2082"; n_done<-TRUE}
    if(sub==21){ n<-"\u2082\u2081"; n_done<-TRUE}
    if(sub==22){ n<-"\u2082\u2082"; n_done<-TRUE}
    if (!n_done) n<-sub }
  result<-paste(car,n,sep="")
  return(result)
}

