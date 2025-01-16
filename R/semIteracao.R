#' Create a Formula with Random Effects
#'
#' Esta función crea una fórmula para un modelo lineal mixto, especificando un efecto aleatorio.
#'
#' @param input Una lista que debe contener los siguientes elementos:
#' \describe{
#'   \item{dataset}{Un data frame con los datos.}
#'   \item{varY}{Nombre de la variable dependiente en el modelo.}
#'   \item{REML}{Un valor lógico que indica si se debe usar REML (TRUE) o máxima verosimilitud (FALSE).}
#'   \item{covariavel}{Nombre de la covariable para el efecto aleatorio.}
#' }
#' @return Una fórmula de modelo lineal mixto con el efecto aleatorio especificado.
#' @examples
#' input <- list(
#'   dataset = mi_dataset,
#'   varY = "mi_variable_dependiente",
#'   REML = TRUE,
#'   covariavel = "mi_covariable"
#' )
#' formula <- create_formulaComUma(input)
#' @author Juanprimate
#' @export
create_formulaComUma <- function(input) {
  # Validación de entradas
  if (!is.list(input) || !all(c("dataset", "varY", "REML") %in% names(input))) {
    stop("La entrada debe ser una lista con 'dataset', 'varY' y 'REML'.")
  } 
  
  # Extraer datos y variables
  dataset <- input$dataset
  varY <- input$varY
  
  # Construir la parte de efectos aleatorios
  temporal <- paste0("(1|", input$covariavel, ")")
  
  # Crear la fórmula completa
  formula <- stats::formula(paste(varY, '~', temporal))
  
  return(formula)
}

#' Run Mixed Model Analysis with Iterated Random Effects Formula
#'
#' Esta función crea una fórmula para un modelo lineal mixto con efectos aleatorios para todas las variables predictores,
#' y luego ejecuta un análisis de modelo mixto utilizando la fórmula generada.
#'
#' @param input Una lista que debe contener los siguientes elementos:
#' \describe{
#'   \item{dataset}{Un data frame con los datos.}
#'   \item{varY}{Nombre de la variable dependiente en el modelo.}
#'   \item{REML}{Un valor lógico que indica si se debe usar REML (TRUE) o máxima verosimilitud (FALSE).}
#' }
#' @return El resultado del análisis del modelo mixto realizado por `run_mixed_model_analysis`, que debe ser una lista con 
#' varios resultados del modelo.
#' @details La función utiliza `create_formulaSemItera` para generar la fórmula del modelo lineal mixto y luego pasa esa 
#' fórmula a `run_mixed_model_analysis` para ejecutar el análisis.
#' @examples
#' input <- list(
#'   dataset = mi_dataset,
#'   varY = "mi_variable_dependiente",
#'   REML = TRUE
#' )
#' results <- lmerRandomListSemItera(input)
#' @author Juanprimate
#' @export
lmerRandomListSemItera <- function(input) {
  # Validar la entrada
  if (!is.list(input) || !all(c("dataset", "varY", "REML") %in% names(input))) {
    stop("La entrada debe ser una lista con 'dataset', 'varY', 'REML'")
  }
  
  covariavel <- setdiff(names(input$dataset), input$varY)
  # Construir la parte de efectos aleatorios
  temporal <- paste0("(1|", covariavel, ")", collapse = '+')
  
  # Crear la fórmula completa
  formula <- stats::formula(paste(input$varY, '~', temporal))  
  
  # Ajustar el modelo mixto lineal
  fit2 <- lme4::lmer(formula = formula, data = input$dataset, REML = input$REML, na.action = na.omit)
  fit <- lmerTest::as_lmerModLmerTest(fit2)
  # Calcular y almacenar los resultados
  return(listaFuncion(input$dataset, input$varY, formula, input$REML,fit))
}

#' Extract p-value for a Specific Term from ANOVA Results
#'
#' Esta función extrae el p-valor asociado a un término específico de un marco de datos que contiene resultados de ANOVA.
#'
#' @param df Un marco de datos que contiene resultados de ANOVA, con términos en las filas y p-valores en una columna llamada "Pr(>Chisq)".
#' @param term El término para el cual se desea extraer el p-valor.
#' @return El p-valor asociado al término especificado. Si el término no se encuentra en el marco de datos, se devuelve NA.
#' @examples
#' # Supongamos que `output$randFitAnovaFit` es el marco de datos con resultados de ANOVA
#' p_value <- extract_p_value(output$randFitAnovaFit, "employee:batch")
#' @author Juanprimate
#' @export
extract_p_value <- function(df, term) {
  # Filtrar la fila correspondiente al término especificado
 
  # Extraer el p-valor usando subset sin corchetes

  p_value <- subset(df, rownames(df) == term, select = "Pr(>Chisq)")

  return(p_value)
}

#' Run Linear Mixed Model Analysis with Specific Random Effects
#'
#' This function creates a formula for a linear mixed model with a specified random effect,
#' fits the model using the `lmer` function from the `lme4` package, and performs analysis
#' on the model using various functions. The results are returned as a list.
#'
#' @description
#' The function takes a list containing a dataset, the name of the dependent variable,
#' a logical value indicating whether to use REML or maximum likelihood estimation, and the
#' name of a covariable to include as a random effect in the model. It constructs the formula,
#' fits the linear mixed model, and then computes and returns various analysis results.
#'
#' @param input A list containing the following elements:
#' \describe{
#'   \item{dataset}{A data frame with the data.}
#'   \item{varY}{A character string representing the name of the dependent variable in the model.}
#'   \item{REML}{A logical value indicating whether to use REML (TRUE) or maximum likelihood (FALSE) for fitting the model.}
#'   \item{covariavel}{A character string representing the name of the covariable to include as a random effect.}
#' }
#'
#' @return A list containing various results from the linear mixed model analysis:
#' \describe{
#'   \item{covarianceMatrixFit}{The covariance matrix of the fixed effects.}
#'   \item{randFitAnovaFit}{The results of ANOVA on the random effects.}
#'   \item{contestFitResp}{Results from the `contestFit` function.}
#'   \item{contest1DFitResp}{Results from the `contest1DFit` function.}
#'   \item{contestMDFitResp}{Results from the `contestMDFit` function.}
#'   \item{confintFitIntervalResp}{Confidence intervals of the fixed effects.}
#'   \item{vcovLmerResp}{Variance-covariance matrix of the fixed effects.}
#'   \item{VarCorrLmerResp}{Variance components of the random effects.}
#' }
#'
#' @examples
#' # Example usage
#' # Assuming `my_data` is a data frame, "response" is the dependent variable,
#' # and "group" is the covariable for the random effect.
#' input <- list(
#'   dataset = my_data,
#'   varY = "response",
#'   REML = TRUE,
#'   covariavel = "group"
#' )
#' results <- lmerRandomListComUma(input)
#'
#' @author juanprimate
#'
#' @export
lmerRandomListComUma <- function(input) {
  # Validar la entrada
  if (!is.list(input) || !all(c("dataset", "varY", "REML","covariavel") %in% names(input))) {
    stop("La entrada debe ser una lista con 'dataset', 'varY', 'REML','covariavel'")
  }
  # Construir la parte de efectos aleatorios
  temporal <- paste0("(1|", input$covariavel, ")")
  # Crear la fórmula completa
  formula <- stats::formula(paste(input$varY, '~', temporal))
  # Ajustar el modelo mixto lineal
  fit2 <- lme4::lmer(formula = formula, data = input$dataset, REML = input$REML, na.action = na.omit)
  fit <- lmerTest::as_lmerModLmerTest(fit2)
  # Calcular y almacenar los resultados
        covarianceMatrixFit = covarianceMatrix(fit)
  output <- as.matrix(lmerTest::rand(fit))

  return(listaFuncionParte(input$dataset, input$varY, formula, input$REML,fit,output=output))
}
#' Generate Analysis Results for Linear Mixed Model
#'
#' This function processes the results from a linear mixed model and returns a list
#' containing various analysis outputs such as the covariance matrix, ANOVA results,
#' confidence intervals, and variance-covariance matrix.
#'
#' @description
#' The function takes a dataset, the name of the dependent variable, the model formula,
#' a logical value indicating whether REML was used, the fitted model object, and optional
#' output. It then calculates and returns several results related to the linear mixed model analysis.
#'
#' @param dataset A data frame containing the data used for fitting the model.
#' @param varY A character string representing the name of the dependent variable in the model.
#' @param formula A formula object specifying the model to be fitted.
#' @param REML A logical value indicating whether REML (TRUE) or maximum likelihood (FALSE) was used for fitting the model.
#' @param fit An object of class `lmerModLmerTest` or similar, which is the result of fitting the model.
#' @param output (Optional) A matrix representing the random effects output, used in the calculation of ANOVA results.
#' 
#' @return A list with the following components:
#' \describe{
#'   \item{covarianceMatrixFit}{The covariance matrix of the fixed effects.}
#'   \item{randFitAnovaFit}{Results of the ANOVA on the random effects, processed by `randFitAnovaParte`.}
#'   \item{contestFitResp}{Results from the `contestFit` function.}
#'   \item{contest1DFitResp}{Results from the `contest1DFit` function.}
#'   \item{contestMDFitResp}{Results from the `contestMDFit` function.}
#'   \item{confintFitIntervalResp}{Confidence intervals of the fixed effects.}
#'   \item{vcovLmerResp}{Variance-covariance matrix of the fixed effects.}
#'   \item{VarCorrLmerResp}{Variance components of the random effects.}
#' }
#'
#' @examples
#' # Example usage
#' # Assuming `my_data` is a data frame, "response" is the dependent variable,
#' # and `fit` is the fitted model object.
#' results <- listaFuncionParte(
#'   dataset = my_data,
#'   varY = "response",
#'   formula = formula(response ~ (1|group)),
#'   REML = TRUE,
#'   fit = fitted_model
#' )
#'
#' @author juanprimate
#'
#' @export
listaFuncionParte <- function(dataset, varY, formula, REML,fit,output=NULL) {
  results <- list(
    covarianceMatrixFit = covarianceMatrix(fit),
    randFitAnovaFit = randFitAnovaParte(output),
    contestFitResp = contestFit(dataset, varY, formula, REML),
    contest1DFitResp = contest1DFit(fit),
    contestMDFitResp = contestMDFit(fit),
    confintFitIntervalResp = confintFitInterval(fit),
    vcovLmerResp = vcovLmer(fit),
    VarCorrLmerResp = VarCorrLmer(fit)
  )
  
  return(results)
}
#' Generate Analysis Results for Linear Mixed Model with Random Effects Output
#'
#' This function processes the results from a linear mixed model, including the random effects output,
#' and returns a list of various analysis outputs such as the covariance matrix, ANOVA results, confidence
#' intervals, and variance-covariance matrix. It is a wrapper around `listaFuncionParte`, including the
#' random effects output as input.
#'
#' @description
#' The function takes a dataset, the name of the dependent variable, the model formula, a logical value indicating
#' whether REML was used, and the fitted model object. It computes the random effects matrix and then passes
#' it to `listaFuncionParte` for further analysis.
#'
#' @param dataset A data frame containing the data used for fitting the model.
#' @param varY A character string representing the name of the dependent variable in the model.
#' @param formula A formula object specifying the model to be fitted.
#' @param REML A logical value indicating whether REML (TRUE) or maximum likelihood (FALSE) was used for fitting the model.
#' @param fit An object of class `lmerModLmerTest` or similar, which is the result of fitting the model.
#' 
#' @return A list with the following components, as returned by `listaFuncionParte`:
#' \describe{
#'   \item{covarianceMatrixFit}{The covariance matrix of the fixed effects.}
#'   \item{randFitAnovaFit}{Results of the ANOVA on the random effects, processed by `randFitAnovaParte`.}
#'   \item{contestFitResp}{Results from the `contestFit` function.}
#'   \item{contest1DFitResp}{Results from the `contest1DFit` function.}
#'   \item{contestMDFitResp}{Results from the `contestMDFit` function.}
#'   \item{confintFitIntervalResp}{Confidence intervals of the fixed effects.}
#'   \item{vcovLmerResp}{Variance-covariance matrix of the fixed effects.}
#'   \item{VarCorrLmerResp}{Variance components of the random effects.}
#' }
#'
#' @examples
#' # Example usage
#' # Assuming `my_data` is a data frame, "response" is the dependent variable,
#' # and `fit` is the fitted model object.
#' results <- listaFuncion(
#'   dataset = my_data,
#'   varY = "response",
#'   formula = formula(response ~ (1|group)),
#'   REML = TRUE,
#'   fit = fitted_model
#' )
#'
#' @author juanprimate
#'
#' @export
listaFuncion <- function(dataset, varY, formula, REML,fit) {
  output <- as.matrix(lmerTest::rand(fit))
  return(listaFuncionParte(dataset, varY, formula, REML,fit,output))
}
