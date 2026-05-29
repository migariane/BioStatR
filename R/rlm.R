#' @title Multiple Linear Regression
#' @description Adjusts a multiple linear regression model according to the specification y ~ x1 + x2 + ...
#' @param f formula: model specification (e.g., y ~ x1 + x2)
#' @param data data.frame: data table
#' @param pred data.frame: regressors for model prediction
#' @param grf logical: if grf=FALSE, graphical output is omitted
#' @param dfout logical: if dfout=TRUE, the procedure returns the data matrix with residuals and predictions
#' @param alfa real < 1: alpha error (parameter alternatively to confidence level). Default = 0.05.
#' @param conf real < 1: confidence level for effect estimation IC. Default = 1-alfa.
#' @param decs integer: decimal precision for results. Default = 3.
#' @return Report with descriptive measures, parameter estimation for multiple linear regression, residual description, and diagnostic plots
#' @importFrom stats lm na.exclude rstandard shapiro.test predict formula model.frame confint sd
#' @importFrom ggplot2 ggplot aes geom_point geom_hline geom_smooth labs geom_histogram geom_dotplot
#' @importFrom graphics par plot
#' @export rlm

#' @examples
#' # Example 1 - Basic usage
#' data(osteo)
#' rlm(imc ~ peso + talla, data = osteo)
#' 
#' # Example 2 - With predictions
#' data(osteo)
#' new_data <- data.frame(peso = c(70, 80), talla = c(170, 180))
#' rlm(imc ~ peso + talla, data = osteo, pred = new_data)
#' 
#' # Example 3 - English output
#' data(osteo)
#' options(BioStatR.lang = "en")
#' rlm(imc ~ peso + talla, data = osteo)
#' options(BioStatR.lang = "es")

rlm <- function(f, data = NULL, pred = NULL, grf = TRUE, dfout = FALSE, alfa = 0.05, conf = 1 - alfa, decs = 3) {
  
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
  modelo <- lm(f, data = mf)
  
  # Extraer datos para descriptiva
  dataf <- model.frame(modelo)
  vars <- colnames(dataf)
  n_val <- nrow(dataf)
  
  # Descriptiva basica
  resumen_vars <- data.frame(
    Variable = vars,
    n = rep(n_val, length(vars)),
    Media = round(colMeans(dataf, na.rm = TRUE), decs),
    DT = round(apply(dataf, 2, sd, na.rm = TRUE), decs),
    Min = round(apply(dataf, 2, min, na.rm = TRUE), decs),
    Max = round(apply(dataf, 2, max, na.rm = TRUE), decs)
  )

  # Coeficientes
  su <- summary(modelo)
  coefs <- su$coefficients
  ci <- confint(modelo, level = conf)
  
  tabla_coefs <- data.frame(
    Termino = rownames(coefs),
    Estimacion = round(coefs[, 1], decs),
    Error_Std = round(coefs[, 2], decs),
    IC_inf = round(ci[, 1], decs),
    IC_sup = round(ci[, 2], decs),
    t_exp = round(coefs[, 3], decs),
    sig = sapply(coefs[, 4], function(p) ptxt(p, decs = decs, eq = 1))
  )
  rownames(tabla_coefs) <- NULL

  # Bondad de ajuste
  r2 <- round(su$r.squared, decs)
  r2adj <- round(su$adj.r.squared, decs)
  s2r <- round(su$sigma^2, decs)
  
  # Residuos
  res <- modelo$residuals
  zres <- rstandard(modelo)
  pre <- modelo$fitted.values
  sw_test <- shapiro.test(res)
  ptxt_sw <- ptxt(sw_test$p.value, decs = decs, eq = 1)
  
  tabla_resids <- round(data.frame(
    Residuos = as.numeric(quantile(res, na.rm = TRUE)),
    Res_Est = as.numeric(quantile(zres, na.rm = TRUE))
  ), decs)
  row.names(tabla_resids) <- c("min", "Q1", "Q2", "Q3", "max")

  # Pronosticos
  tpronosticos <- NULL
  if (!is.null(pred)) {
    if (!is.data.frame(pred)) {
      # Intentar convertir a dataframe si es posible
      pred <- as.data.frame(pred)
    }
    bconf <- predict(modelo, newdata = pred, level = conf, interval = "confidence")
    bpreds <- predict(modelo, newdata = pred, level = conf, interval = "prediction")
    tpronosticos <- data.frame(pred, Puntual = bconf[, 1], IC_m_inf = bconf[, 2], IC_m_sup = bconf[, 3], IC_obs_inf = bpreds[, 2], IC_obs_sup = bpreds[, 3])
  }

  # Salida por consola
  cat("\n")
  cat(get_msg("rlm_title"), "\n")
  cat("----------------------------------------------------------------\n")
  cat(get_msg("info_muestral"), "\n\n")
  print(resumen_vars)
  cat("\n")
  
  cat(get_msg("modelo_lineal"), "\n\n")
  cat("  ", get_msg("modelo"), ": ", deparse(f), "\n")
  cat("  R\u00b2 = ", r2, " (R\u00b2 ", get_msg("ajustado"), " = ", r2adj, ")\n")
  cat("  S\u00b2residual = ", s2r, "\n\n")
  cat("  ", get_msg("coeficientes"), ":\n\n")
  print(tabla_coefs)
  cat("\n")

  if (!is.null(tpronosticos)) {
    cat("# Pron\u00f3sticos con el modelo --- \n")
    cat("  Pronosticos puntuales y bandas al", conf * 100, "% de confianza para \n")
    cat("  promedios IC(m), y para una nueva observaci\u00f3n: IC(obs)  \n\n")
    print(tpronosticos)
    cat("\n")
  }

  cat("# Distribuci\u00f3n residual --- \n")
  cat("  Error est\u00e1ndar residual: ", round(su$sigma, decs), "\n")
  print(tabla_resids)
  cat("\n")
  cat("  Test de normalidad residual (Shapiro-Wilk): \n ")
  cat("  w =", round(sw_test$statistic, decs), ", ", ptxt_sw, "\n")
  cat("\n")

  # Graficos
  if (grf) {
    dat_plot <- data.frame(pre = pre, res = res, zres = zres)
    
    # Residuos vs Ajustados
    p1 <- ggplot(dat_plot, aes(x = pre, y = res)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "blue") +
      labs(title = "Residuos frente a Valores Ajustados", x = "Valores Ajustados", y = "Residuos")

    # Q-Q Plot
    p2 <- ggplot(dat_plot, aes(sample = zres)) +
      ggplot2::stat_qq() +
      ggplot2::stat_qq_line() +
      labs(title = "Q-Q Plot de Residuos Estandarizados", x = "Cuantiles Te\u00f3ricos", y = "Cuantiles de los Residuos")

    # Histograma Residuos
    p3 <- ggplot(dat_plot, aes(x = zres)) +
      geom_histogram(bins = 30, fill = "lightblue", color = "black") +
      labs(title = "Distribuci\u00f3n de Residuos Estandarizados", x = "Residuos Estandarizados", y = "Frecuencia")

    # Mostrar graficos (como hace rls, pero simplificado)
    par(mfrow = c(2, 2))
    plot(modelo)
    par(mfrow = c(1, 1))
    
    suppressMessages(print(p1))
    suppressMessages(print(p2))
    suppressMessages(print(p3))
  }

  invisible(modelo)
  if (dfout) {
    res_df <- data.frame(dataf, pre = pre, res = res, zres = zres)
    return(res_df)
  }
}
