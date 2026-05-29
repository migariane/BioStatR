#' @title Regresion logistica multivariable
#' @description Ajuste del modelo de regresion logistica multivariable de acuerdo a la especificacion y ~ x1 + x2 + ...
#' @param f formula: especificacion del modelo (ej. y ~ x1 + x2)
#' @param data data.frame: tabla de datos
#' @param pred data.frame: valores de los regresores para realizar pronosticos a partir del modelo
#' @param grf valor logico: si grf=TRUE se incluye el grafico de la curva ROC. Por defecto = FALSE.
#' @param alfa valor real < 1: error alfa (parametro alternativamente al nivel de confianza, en tanto por uno). Por defecto =.05.
#' @param conf valor real < 1: nivel de confianza para la elaboracion del IC para la estimacion del efecto. Por defecto = 1-alfa.
#' @param decs valor entero: precision decimal para la salida de resultados. Por defecto = 3.
#' @return Informe con medidas de asociacion (OR), estimacion de los parametros de regr. logistica multivariable, bondad de ajuste (Hosmer-Lemeshow) y curva ROC.
#' @importFrom stats glm binomial confint predict formula model.frame na.exclude
#' @importFrom ResourceSelection hoslem.test
#' @importFrom pROC roc auc coords
#' @importFrom ggplot2 labs theme_minimal scale_x_continuous scale_y_continuous coord_cartesian
#' @export rlogitm
#' @examples
#' # Ejemplo 1 - Uso basico
#' data(osteo)
#' rlogitm(osteo_cue ~ imc + edad, data = osteo)
#' 
#' # Ejemplo 2 - Con predicciones
#' data(osteo)
#' nuevos_datos <- data.frame(imc = c(22, 25), edad = c(30, 50))
#' rlogitm(osteo_cue ~ imc + edad, data = osteo, pred = nuevos_datos)

rlogitm <- function(f, data = NULL, pred = NULL, grf = FALSE, alfa = 0.05, conf = 1 - alfa, decs = 3) {
  
  if (missing(f)) stop("Debe especificar una formula para el modelo.")
  if (!inherits(f, "formula")) stop("El primer argumento debe ser una formula (ej. y ~ x1 + x2).")

  # Manejo de alfa y conf
  epairsget(p = alfa, q = conf, pmin = 0.0001, pdefault = 0.05) -> par_alfa
  if (par_alfa[[1]][1]) {
    alfa <- par_alfa[[2]][1]
    conf <- par_alfa[[2]][2]
  } else {
    stop("valor de alfa o de conf incongruente")
  }

  # Obtener el modelo
  mf <- model.frame(f, data = data)
  mf <- na.omit(mf)
  modelo <- glm(f, data = mf, family = binomial)
  
  # Extraer datos
  dataf <- model.frame(modelo)
  y_var <- dataf[[1]]
  ylbl <- colnames(dataf)[1]
  
  # Tamaños muestrales
  n_analizado <- nrow(dataf)
  n_inicial <- if(!is.null(data)) nrow(na.omit(model.frame(f, data=data))) else n_analizado
  n_miss <- n_inicial - n_analizado
  
  n_eventos <- sum(modelo$y == 1)
  n_no_eventos <- sum(modelo$y == 0)
  # Tamaño muestral efectivo (regla del pulgar para logística: min de eventos y no eventos)
  n_efectivo <- min(n_eventos, n_no_eventos)

  # Frecuencias de la respuesta
  tab_y <- table(y_var)
  prop_y <- prop.table(tab_y)
  resumen_y <- data.frame(
    Categoria = names(tab_y),
    n = as.numeric(tab_y),
    Porcentaje = round(as.numeric(prop_y) * 100, decs)
  )

  # Coeficientes y OR
  su <- summary(modelo)
  coefs <- su$coefficients
  ci <- confint(modelo, level = conf)
  
  # Odds Ratios
  or <- exp(coefs[, 1])
  ci_or <- exp(ci)
  
  tabla_coefs <- data.frame(
    Termino = rownames(coefs),
    Estimacion = round(coefs[, 1], decs),
    Error_Std = round(coefs[, 2], decs),
    z_exp = round(coefs[, 3], decs),
    sig = sapply(coefs[, 4], function(p) ptxt(p, decs = decs, eq = 1)),
    OR = round(or, decs),
    OR_inf = round(ci_or[, 1], decs),
    OR_sup = round(ci_or[, 2], decs)
  )
  rownames(tabla_coefs) <- NULL

  # Bondad de ajuste
  null_dev <- su$null.deviance
  res_dev <- su$deviance
  aic <- su$aic
  
  # R2 de Nagelkerke
  n <- nrow(dataf)
  r2_cuadrado <- 1 - exp((res_dev - null_dev) / n)
  r2_max <- 1 - exp(-null_dev / n)
  r2_nagelkerke <- round(r2_cuadrado / r2_max, decs)

  # Test de Hosmer-Lemeshow
  hl <- ResourceSelection::hoslem.test(modelo$y, fitted(modelo))
  hl_pval <- hl$p.value
  hl_txt <- ptxt(hl_pval, decs = decs, eq = 1)

  # Capacidad discriminante (AUC)
  prob_pred <- predict(modelo, type = "response")
  roc_obj <- pROC::roc(modelo$y, prob_pred, quiet = TRUE)
  auc_val <- pROC::auc(roc_obj)
  
  # Pronosticos
  tpronosticos <- NULL
  if (!is.null(pred)) {
    if (!is.data.frame(pred)) pred <- as.data.frame(pred)
    pred_prob <- predict(modelo, newdata = pred, type = "response")
    tpronosticos <- data.frame(pred, Probabilidad = round(pred_prob, decs))
  }

  # Salida por consola
  cat("
")
  cat(get_msg("rlog_title"), " multivariable
")
  cat("----------------------------------------------------------------
")
  cat(get_msg("info_muestral"), "

")
  cat("  ", get_msg("n_inicial"), ": ", n_inicial, "
")
  cat("  ", get_msg("n_completos"), ": ", n_analizado, "
")
  cat("  ", get_msg("n_efectivo"), ": ", n_efectivo, "

")
  cat(get_msg("dist_res"), " (", ylbl, ") ---

", sep = "")
  print(resumen_y)
  cat("
")
  
  cat(get_msg("modelo_log"), " --- 

")
  cat("  ", get_msg("modelo"), ": ", deparse(f), "
")
  cat("  Devianza residual: ", round(res_dev, decs), " (Nula: ", round(null_dev, decs), ")
")
  cat("  AIC: ", round(aic, decs), "
")
  cat("  R\u00b2 de Nagelkerke: ", r2_nagelkerke, "

")
  cat("  ", get_msg("hl_test"), ":
")
  cat("  X\u00b2 = ", round(hl$statistic, decs), ", gl = ", hl$parameter, ", ", hl_txt, "

")
  cat("  ", get_msg("disc_cap"), ":
")
  cat("  ", get_msg("auc"), " = ", round(auc_val, decs), "

")
  
  if (!is.null(tpronosticos)) {
    cat("# Pron\u00f3sticos con el modelo ---

")
    print(tpronosticos)
    cat("
")
  }
  
  cat("  ", get_msg("coeficientes"), ":

")
  print(tabla_coefs)
  cat("
")

  # Grafico ROC
  if (grf) {
    library(ggplot2)
    # Calcular AUC e IC
    auc_val <- pROC::auc(roc_obj)
    ci_auc <- pROC::ci.auc(roc_obj)
    auc_label <- sprintf("AUC = %.3f (95%% CI: %.3f - %.3f)", 
                         as.numeric(auc_val), as.numeric(ci_auc[1]), as.numeric(ci_auc[3]))
    
    p <- pROC::ggroc(roc_obj, aes = "color", size = 1) +
      theme_classic() +
      labs(title = paste(get_msg("roc_main"), "\n", auc_label), 
           x = get_msg("roc_xlab"), 
           y = get_msg("roc_ylab")) +
      geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "gray")
    
    print(p)
  }

  invisible(list(modelo = modelo, roc = roc_obj, hl = hl))
}
