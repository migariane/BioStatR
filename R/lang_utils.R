#' @title Get multilingual message
#' @description Returns a message in the language specified by option 'BioStatR.lang' (default 'es').
#' @param key Message key
#' @return Message string
get_msg <- function(key) {
  lang <- getOption("BioStatR.lang", default = "es")
  
  messages <- list(
    es = list(
      rlm_title = "Regresi\u00f3n lineal m\u00faltiple",
      rlog_title = "Regresi\u00f3n log\u00edstica",
      info_muestral = "# Informaci\u00f3n muestral ---",
      modelo_lineal = "# Modelo lineal ---",
      modelo_log = "# Modelo log\u00edstico ---",
      modelo = "Modelo",
      ajustado = "ajustado",
      coeficientes = "Coeficientes del modelo",
      dist_res = "# Distribuci\u00f3n de la variable respuesta",
      hl_test = "Test de bondad de ajuste de Hosmer-Lemeshow",
      disc_cap = "Capacidad discriminante",
      auc = "AUC (Area bajo la curva ROC)",
      roc_main = "Curva ROC",
      roc_xlab = "1 - Especificidad",
      roc_ylab = "Sensibilidad (Tasa de Verdaderos Positivos)",
      n_inicial = "Tama\u00f1o muestral (N inicial)",
      n_completos = "Tama\u00f1o muestral tras eliminar valores perdidos (Casos completos)",
      n_efectivo = "M\u00ednima frecuencia de eventos (n efectivo)",
      # RLS keys
      rls_title = "Regresi\u00f3n lineal simple",
      rls_cor_pearson = "# Correlaci\u00f3n de Pearson ---",
      rls_modelo = "# Modelo lineal ---",
      rls_pronosticos = "# Pron\u00f3sticos con el modelo ---",
      rls_pron_bandas = "Pronosticos puntuales y bandas al",
      rls_confianza = "% de confianza para",
      rls_promedios = "promedios IC(m), y para una nueva observaci\u00f3n: IC(obs)",
      rls_dist_residual = "# Distribuci\u00f3n residual ---",
      rls_error_est_res = "Error est\u00e1ndar residual:",
      rls_test_normalidad = "Test de normalidad residual (Shapiro-Wilk):"
    ),
    en = list(
      rlm_title = "Multiple Linear Regression",
      rlog_title = "Logistic Regression",
      info_muestral = "# Sample information ---",
      modelo_lineal = "# Linear model ---",
      modelo_log = "# Logistic model ---",
      modelo = "Model",
      ajustado = "adjusted",
      coeficientes = "Model coefficients",
      dist_res = "# Distribution of the response variable",
      hl_test = "Hosmer-Lemeshow goodness-of-fit test",
      disc_cap = "Discriminant capacity",
      auc = "AUC (Area under the ROC curve)",
      roc_main = "ROC Curve",
      roc_xlab = "1 - Specificity",
      roc_ylab = "Sensitivity (True Positive Rate)",
      n_inicial = "Sample size (Initial N)",
      n_completos = "Sample size after removing missing values (Complete cases)",
      n_efectivo = "Minimum frequency of events (effective n)",
      # RLS keys
      rls_title = "Simple Linear Regression",
      rls_cor_pearson = "# Pearson Correlation ---",
      rls_modelo = "# Linear model ---",
      rls_pronosticos = "# Predictions with the model ---",
      rls_pron_bandas = "Point predictions and bands at",
      rls_confianza = "% confidence for",
      rls_promedios = "averages IC(m), and for a new observation: IC(obs)",
      rls_dist_residual = "# Residual distribution ---",
      rls_error_est_res = "Residual standard error:",
      rls_test_normalidad = "Residual normality test (Shapiro-Wilk):"
    )
  )
  
  return(messages[[lang]][[key]])
}
