## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup--------------------------------------------------------------------
library(BioStatR)
data(osteo)


## ----rls----------------------------------------------------------------------
# Ejemplo de regresión lineal simple
rls(imc ~ peso, data = osteo)


## ----rlogits------------------------------------------------------------------
# Ejemplo de regresión logística simple
rlogits(osteo_cue ~ imc, data = osteo)

