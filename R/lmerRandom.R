#' Creating a formula from a string to mixed model, formulaRandomList
#'
#' @description
#' Creating a formula from a string to random effect
#'
#' @author juanprimate
#'
#' @param data A data set of class "list".
#' @param varY An object of class "character".
#'
#' @return An object of class `"formula"` which contains a symbolic model
#' formula.
#'
#' @importFrom combinat combn
#' @importFrom stats formula
#'
#' @export
#'
formulaRandomList <- function(data,varY) {
	covariables <- names(data)[names(data)!=varY]
	k <- length(covariables)
	temporal <- NULL
	for (i in seq(k)) {
		combina <- as.matrix(combinat::combn(covariables, i))
		acumula <- NULL
		for(j in seq(ncol(combina))){
			tmp <- combina[,j]
			formula2 <- paste(tmp,collapse=':')
			acumula <- c(acumula,formula2)
		}
		temporal <- c(temporal,paste0("(1|",acumula,")", collapse='+'))
	}
	return(stats::formula(paste(varY,'~',paste0(temporal,collapse="+"))))
}

#' Creating a formula from a string to mixed model White test
#'
#' @description
#' Creating a formula from a string to random effect
#'
#' @author juanprimate
#'
#' @param dataset A data set of class "data.frame".
#' @param varY An object of class "character".
#'
#' @return An object of class `"formula"` which contains a symbolic model
#' formula.
#'
#' @importFrom combinat combn
#' @importFrom stats formula
#'
#' @export
#'
formulaRandom <- function(dataset,varY) {
	covariables <- colnames(dataset[, -which(names(dataset) == varY)])
	k <- ncol(dataset[, -which(names(dataset) == varY)])
	temporal <- NULL
	for (i in seq(k)) {
		combina <- as.matrix(combinat::combn(covariables, i))
		acumula <- NULL
		for(j in seq(ncol(combina))){
			tmp <- combina[,j]
			formula2 <- paste(tmp,collapse=':')
			acumula <- c(acumula,formula2)
		}
		temporal <- c(temporal,paste0("(1|",acumula,")", collapse='+'))
	}
	return(stats::formula(paste(varY,'~',paste0(temporal,collapse="+"))))
}
#' Fit Linear Mixed-Effects Models
#'
#' @description
#' Fit a linear mixed-effects model (LMM) to data, via REML or maximum
#' likelihood.
#'
#' @author juanprimate
#'
#' @param dataset An data frame containing the variables named in formula.
#' @param varY An object of class "numeric" (like response).
#' @param formula A two-sided linear formula object describing both the
#' fixed-effects and random-effects part of the model, with the response on
#' the left of a ~ operator and the terms, separated by + operators, on the
#' right. Random-effects terms are distinguished by vertical bars (|)
#' separating expressions for design matrices from grouping factors.
#' @param REML A logical scalar - Should the estimates be chosen to optimize
#' the REML criterion (as opposed to the log-likelihood)?
#' 1) REML  <-  FALSE is used in case of comparing models with different
#' “Fixed effects” (during the simplification of model)
#' 2) REML  <-  TRUE is used in case of different random effects on the
#' comparing models. It is to estimate with restricted likelihood
#'
#' @return An object of class "lmerModLmerTest"
#'
#' @importFrom lme4 lmer
#'
#' @export
#'
lmerRandomFit <- function(dataset,varY,formula,REML) {
	positionVarY <- which(colnames(dataset)==varY)
	colnames(dataset)  <-  iconv(colnames(dataset), to = "UTF-8")
	varY <- colnames(dataset)[positionVarY]
	for (i in seq(ncol(dataset))){
		if (colnames(dataset)[i] !=varY){
			dataset[,i]  <-  as.factor(dataset[,i])
		}
	}
	#Converte a resposta p/ num\u00E9rico
	dataset[,varY]  <-  as.numeric(unclass(dataset[,varY]))
	
	return(safe_lmerBroad(formula=formula,data=dataset,REML=REML,na.action=na.omit))
}
#' Computing the covariance matrix
#'
#' @description
#' Coercing an lme4::lmer model-object (of class ’lmerMod’) to a model-object
#' of class ’lmerModLmerTest’
#' involves computing the covariance matrix of the variance parameters and the
#' gradient (Jacobian) of
#' cov(beta) with respect to the variance parameters.
#'
#' @author juanprimate
#'
#' @param fit An object of class "lmerModLmerTest".
#'
#' @importFrom utils capture.output
#'
#' @return A covariance matrix.
#'
#' @export
#'
covarianceMatrix  <-  function(fit) {
	aux1  <-  suppressWarnings(summary(fit)$varcor)
	aux  <-  utils::capture.output(summary(fit)$varcor)
	for(i in seq(length(aux)))
		if(i==1) resp  <-  aux[1] else resp  <-  rbind(resp,aux[i])

	aux  <-  apply(resp,1,function(a) unlist(strsplit(a, " ")))
	aux1  <-  lapply(aux,function(a) unlist(strsplit(a, " ")))
	aux1  <-  lapply(aux1,function(a) unlist(strsplit(a, "Name")))
	aux1  <-  lapply(aux1,function(a) unlist(strsplit(a, "Intercept")))
	aux1  <-  lapply(aux1,function(a) unlist(strsplit(a, split="(" ,fixed=TRUE)))
	aux1  <-  lapply(aux1,function(a) unlist(strsplit(a, ")")))
	aux1  <-  lapply(aux1,function(a) unlist(strsplit(a, split=" " ,fixed=TRUE)))
	# aux1=lapply(aux1,function(a) sub("Residual",taction('Res\u00EDduos'),a))
	for(i in seq(length(aux1)))
		if(i==1) resp  <-  aux1[[1]] else resp  <-  rbind(resp,aux1[[i]])

	resp  <-  suppressWarnings(data.frame(resp))
	resp  <-  resp[-1,]
	colnames(resp)  <-  c(paste('Factors'), paste('Standard deviation'))
	return(resp)
}

#' Extract and Process Random Effects from a Linear Mixed Model
#'
#' This function extracts the random effects from a fitted linear mixed model and then processes
#' the results using `randFitAnovaParte` for further analysis.
#'
#' @description
#' The function takes an object of class "lmerModLmerTest" (a fitted linear mixed model),
#' extracts the random effects using `lmerTest::rand`, and then passes these results to
#' `randFitAnovaParte` for further processing. This allows for a streamlined workflow in
#' analyzing and interpreting the random effects from mixed models.
#'
#' @param fit An object of class "lmerModLmerTest" that represents a fitted linear mixed model.
#'        The model should be fitted using the `lmer` function from the `lme4` package and
#'        converted to a "lmerModLmerTest" object using `lmerTest::as_lmerModLmerTest`.
#'
#' @return A data frame with the processed random effects, as returned by `randFitAnovaParte`.
#'
#' @examples
#' # Example of usage
#' # Assuming `fit` is an object of class "lmerModLmerTest"
#' processed_random_effects <- randFitAnova(fit)
#'
#' @author juanprimate
#'
#' @export
randFitAnova  <-  function(fit) {
	output <- as.matrix(lmerTest::rand(fit))
	return(randFitAnovaParte(output))
}
#' Process and Clean ANOVA Output Data
#'
#' This function processes the output from an ANOVA test, specifically focusing on
#' cleaning and extracting relevant information from the row names of the output data.
#'
#' @description
#' The function takes the output from an ANOVA test, extracts and cleans up the row names,
#' and formats them into a consistent structure. The cleaned row names are then set as the row names
#' of the original output data frame.
#'
#' @param output A data frame with ANOVA results. The row names of this data frame are expected to
#'        contain the terms of the ANOVA, which will be processed and cleaned by the function.
#'
#' @return A data frame with the same structure as the input `output`, but with cleaned row names.
#'
#' @examples
#' # Example of usage
#' # Assuming `anova_output` is a data frame resulting from an ANOVA test
#' cleaned_output <- randFitAnovaParte(anova_output)
#'
#' @author juanprimate
#'
#' @export
randFitAnovaParte  <-  function(output) {

	aux  <-  rownames(output)

	for(i in seq(length(aux)))
		if(i==1) resp  <-  aux[1] else resp  <-  rbind(resp,aux[i])

	aux <- apply(resp,1,function(a) unlist(strsplit(a, " ")))

	aux1 <- lapply(aux,function(a) unlist(strsplit(a, ")")))

	for(i in seq(length(aux1)))
		if(i==1) resp  <-  aux1[[1]] else resp  <-  rbind(resp,aux1[[i]])

	resp  <-  suppressWarnings(data.frame(resp))
	resp <- resp[,ncol(resp)]
	rownames(output) <- resp
	return(output)
}
#' Test of Contrasts, contest
#'
#' @description
#' Tests of vector or matrix contrasts for lmer model fits. Contrast test
#' (contest) using a custom contrast:
#' Here we make the 2-df joint test of the main effects of Gender and
#' Information
#'
#' @author juanprimate
#'
#' @param dataset An data frame containing the variables named in formula.
#' @param varY An object of class "numeric" (like response).
#' @param formula A two-sided linear formula object describing both the
#' fixed-effects and random-effects part of the model, with the response on
#' the left of a ~ operator and the terms, separated by + operators, on the
#' right. Random-effects terms are distinguished by vertical bars (|)
#' separating expressions for design matrices from grouping factors.
#' @param REML A logical scalar - Should the estimates be chosen to optimize
#' the REML criterion (as opposed to the log-likelihood)?
#' 1) REML = FALSE is used in case of comparing models with different
#' “Fixed effects” (during the simplification of model)
#' 2) REML = TRUE is used in case of different random effects on the comparing
#' models.
#'
#' @return A data.frame or a list of data.frames. If the design matrix is rank
#' deficient, lmer drops columns for the aliased coefficients from the design
#' matrix and excludes the corresponding aliased coefficients from fixef(model).
#' the estimate to intercept with confidence interval
#'
#' @importFrom lme4 fixef lmer
#' @importFrom lmerTest contest1D contestMD contest as_lmerModLmerTest
#'
#' @export
#'
contestFit <- function(dataset,varY,formula,REML) {
	positionVarY <- which(colnames(dataset)==varY)
	
	colnames(dataset)  <-  iconv(colnames(dataset), to = "UTF-8")
	varY <- colnames(dataset)[positionVarY]
	for (i in seq(ncol(dataset))){
		if (colnames(dataset)[i] !=varY){
			dataset[,i]  <-  as.factor(dataset[,i])
		}
	}

	dataset[,varY] <- as.numeric(unclass(dataset[,varY]))

	fit <- lme4::lmer(formula=formula,data=dataset,REML=REML,na.action=na.omit)

	L <- diag(length(lme4::fixef(fit)))[1, ]
	fit2 <- lmerTest::as_lmerModLmerTest(fit)
	return(lmerTest::contest(fit2, L, joint=FALSE, confint = TRUE))
}

#' Test of Contrasts, contest1D
#'
#' @description
#' Tests of vector or matrix contrasts for lmer model fits. Same test, but now
#' as a t-test instead:
#'
#' @author juanprimate
#'
#' @param fit An object of class "lmerModLmerTest".
#'
#' @importFrom lme4 fixef
#' @importFrom lmerTest contest1D contestMD
#'
#' @return A data.frame or a list of data.frames. If the design matrix is rank
#' deficient, lmer drops columns for the aliased coefficients from the design
#' matrix and excludes the corresponding aliased coefficients from fixef(model).
#' the estimate to intercept
#'
#' @export
#'
contest1DFit  <-  function(fit)
	return(contest1D(fit, L = diag(length(fixef(fit)))[1, ]))


#' Test of Contrasts, contestMD
#'
#' @description
#' Tests of vector or matrix contrasts for lmer model fits. Make the 1-df
#' F-test of the effect of Days^2:
#'
#' @author juanprimate
#'
#' @param fit An object of class "lmerModLmerTest".
#' @return A data.frame or a list of data.frames. If the design matrix is rank
#' deficient, lmer drops columns for the aliased coefficients from the design
#' matrix and excludes the corresponding aliased coefficients from fixef(model).
#' the sum square, mean sum square and degree freedom and statistic
#' @importFrom lme4 fixef
#' @importFrom lmerTest contest1D contestMD
#'
#' @export
#'

contestMDFit <- function(fit) contestMD(fit, L = diag(length(fixef(fit)))[1, ] )


#' Compute Confidence Intervals for Parameters of a [ng]lmer Fit
#'
#' @description
#' Compute confidence intervals on the parameters of a *lmer() model fit
#' (of class"merMod").
#'
#' @author juanprimate
#'
#' @param fit A fitted [ng]lmer model or profile
#'
#' @importFrom stats confint
#'
#' @return Depending on the method specified, confint() computes confidence
#' intervals by
#'
#' @export
#'
confintFitInterval <- function(fit) stats::confint(fit, oldNames = FALSE)

#' Calculate Variance-Covariance Matrix for intercept
#'
#' @description
#' Returns the variance-covariance matrix of the main parameters of intercept.
#'
#' @author juanprimate
#'
#' @param fit A fitted [ng]lmer model or profile
#' @return A matrix of the estimated covariances between the parameter
#' estimates in the linear or non-linear predictor of the model.
#'
#' @export
#'
vcovLmer <- function(fit) suppressWarnings(summary(fit)[['vcov']])

#' Extract random effect variances from lme4 mer model object
#'
#' @description
#' This function calculates the estimated variances, standard deviations, and
#' correlations between the random-effects terms in a mixed-effects model, of
#' class merMod (linear, generalized or nonlinear). The within-group error
#' variance and standard deviation are also calculated.
#'
#' @author juanprimate
#'
#' @param fit An object of class "lmerModLmerTest".
#'
#' @importFrom lme4 VarCorr
#'
#' @return An object of class VarCorr.merMod or data.frame.
#' 1.- variance
#' 2.- sd
#'
#' @export
#'
	# resp <- output[,-which(names(output) == 'var2')]
VarCorrLmer <- function(fit) {
	output <- as.data.frame(lme4::VarCorr(fit))
	return(suppressWarnings(output[,-which(names(output) == 'var2')]))
}

#' Analysis of variance component with maximum likelihood estimation
#'
#' @description Generate analysis of variance component
#'  with Random Factors
#'
#' @author juanprimate
#'
#' @param input Input a list
#'
#' @return list
#' - covarianceMatrix
#' - randFitAnova
#' - contestFit
#' - contest1DFit
#' - contestMDFit
#' - confintFitInterval
#' - vcovLmer
#' - VarCorrLmer
#'
#' @importFrom combinat combn
#' @importFrom lme4 lmer fixef VarCorr
#' @importFrom utils capture.output
#' @importFrom lmerTest contest1D contestMD contest as_lmerModLmerTest rand
#' @importFrom stats formula
#'
#' @export
#'
lmerRandomList <- function(input) {
	# Asignar el dataset seleccionado
	dataset <- input$dataset

	# Convertir el dataset en una lista
	data <- as.list(dataset)

	# Asignar la variable de respuesta seleccionada
	varY <- input$varY

	# Seleccionar los nombres de las covariables, excluyendo la variable de respuesta
	covariables <- setdiff(names(data), varY)
	k <- length(covariables)
	temporal <- NULL

	for (i in seq(k)) {
		combina <- as.matrix(combinat::combn(covariables, i))
		acumula <- NULL
		for(j in seq(ncol(combina))){
			tmp <- combina[,j]
			formula2 <- paste(tmp,collapse=':')
			acumula <- c(acumula,formula2)
		}
		temporal <- c(temporal,paste0("(1|",acumula,")", collapse='+'))
	}

	formula <- stats::formula(paste(varY,'~',paste0(temporal,collapse="+")))

	REML <- input$REML

	fit2 <- lme4::lmer(formula=formula,data=dataset,REML=REML,na.action=na.omit)
	fit <- lmerTest::as_lmerModLmerTest(fit2)

  # Calcular y almacenar los resultados
  return(listaFuncion(input$dataset, input$varY, formula, input$REML,fit))
}
#' Create a Random Effects Formula
#'
#' @description
#' This function generates a formula for random effects using the input dataset, dependent variable (`varY`), 
#' and any remaining variables in the dataset as random effects. The function combines all possible combinations 
#' of the covariates and constructs a formula that can be used in mixed models.
#'
#' @param input A list with elements:
#' \itemize{
#'   \item \code{dataset}: A data frame containing the data.
#'   \item \code{varY}: A character string indicating the dependent variable.
#'   \item \code{REML}: A logical value (although it is not used within the function).
#' }
#'
#' @return A formula object that represents the random effects structure.
#' 
#' @examples
#' dataset <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
#' input <- list(dataset = dataset, varY = "y", REML = TRUE)
#' create_formulaTodo(input)
#'
#' @author juanprimate
#'
#' @export
create_formulaTodo <- function(input) {
  # Validación de entradas
  if (!is.list(input) || !all(c("dataset", "varY", "REML") %in% names(input))) {
    stop("La entrada debe ser una lista con 'dataset', 'varY' y 'REML'.")
  } 
  
  # Extraer datos y variables
  dataset <- input$dataset
  varY <- input$varY
  covariavel <- setdiff(names(dataset), varY)
  k <- length(covariavel)
  temporal <- NULL
  # Construir la parte de efectos aleatorios
	temporal <- unlist(lapply(seq(k),function(i){
    combina <- as.matrix(combinat::combn(covariavel, i))
    paste0("(1|", apply(combina,2, paste, collapse =':'),")", collapse ='+')}))

	formula <- stats::formula(paste(varY,'~',paste0(temporal,collapse="+")))
  
  return(formula)
} 

#' ranef: Extract the modes of the random effects
#'
#' @description A generic function to extract the conditional modes of the
#' random effects from a fitted model object. For linear mixed models the
#' conditional modes of the random effects are also the conditional means.
#'
#' @author juanprimate
#'
#' @param fit An object of class "lmerModLmerTest".
#'
#' @importFrom lme4 ranef
#'
#' @return From ranef: An object of class ranef.mer composed of a list of
#' data frames, one for each grouping factor for the random effects. The
#' number of rows in the data frame is the number of levels of the grouping
#' factor. The number of columns is the dimension of the random effect
#' associated with each level of the factor.
#'
#' @export
#'
ranefLmer <- function(fit) lme4::ranef(fit)
#' Perform Safe Restricted Likelihood Ratio Test (RLRT)
#'
#' @description
#' This function performs a series of restricted likelihood ratio tests (RLRTs) 
#' on linear mixed-effects models fitted to a dataset of resistors. The models 
#' account for random effects of parts, operators, and their interaction. 
#' It checks for singular models and computes RLRT statistics for the random effects.
#'
#' @param resistors A data frame containing the dataset with at least a column 
#'   named `mohms` representing measurements and columns representing 
#'   the random effect grouping factors `part` and `oper`.
#' @return A numeric vector of RLRT statistics, where the first element is `NA` 
#'   (reserved for future use), followed by RLRT statistics for parts, operators, 
#'   and their interaction.
#' @details
#' The function uses the `safe_lmer` function to fit mixed-effects models 
#' and suppresses warnings and messages during the computation of RLRT statistics. 
#' If any model is singular, a warning is printed to indicate that results may not be valid.
#'
#' @importFrom lme4 isSingular
#' @importFrom RLRsim exactRLRT
#' @export
#' @author juanprimate
#' @examples
#' # Example usage:
#' # Assuming `resistors` is a data frame with appropriate columns:
#' # result <- exactRLRT_safe(resistors)
#' # print(result)
exactRLRT_safe <- function(resistors) {
  
  mohms.lmer <-safe_lmer(mohms~1 + (1|part) + (1|oper) + (1|part:oper),data=resistors)
  mohms.partonly.lmer <- safe_lmer(mohms~1 + (1|part),data=resistors)
  mohms.operonly.lmer <- safe_lmer(mohms~1 + (1|oper),data=resistors)
  mohms.intronly.lmer <- safe_lmer(mohms~1 + (1|part:oper),data=resistors)
  mohms.nopart.lmer <-safe_lmer(mohms~1 + (1 | oper) + (1 | part:oper),data=resistors)
  mohms.nooper.lmer <-safe_lmer(mohms~1 + (1 | part) + (1 | part:oper),data=resistors)
  mohms.nointr.lmer <-safe_lmer(mohms~1+ (1 | part) + (1 | oper),data=resistors)
  if (lme4::isSingular(mohms.lmer) || 
      lme4::isSingular(mohms.partonly.lmer) || 
      lme4::isSingular(mohms.operonly.lmer) || 
      lme4::isSingular(mohms.intronly.lmer) || 
      lme4::isSingular(mohms.nopart.lmer) || 
      lme4::isSingular(mohms.nooper.lmer) || 
      lme4::isSingular(mohms.nointr.lmer) ) 
    print('One or more models are singular. Results may not be valid.')  
  #fit1<-safe_exactRLRT(mohms.partonly.lmer,mohms.lmer,mohms.nopart.lmer)
  #fit2<-safe_exactRLRT(mohms.operonly.lmer,mohms.lmer,mohms.nooper.lmer)
  #fit3<-safe_exactRLRT(mohms.intronly.lmer,mohms.lmer,mohms.nointr.lmer)
  
  fit1<-suppressMessages(suppressWarnings(RLRsim::exactRLRT(mohms.partonly.lmer,mohms.lmer,mohms.nopart.lmer)))
  fit2<-suppressMessages(suppressWarnings(RLRsim::exactRLRT(mohms.operonly.lmer,mohms.lmer,mohms.nooper.lmer)))
  fit3<-suppressMessages(suppressWarnings(RLRsim::exactRLRT(mohms.intronly.lmer,mohms.lmer,mohms.nointr.lmer)))
  
  out<-c(NA,fit1[['statistic']],
         fit2[['statistic']],
         fit3[['statistic']])
  return(out)
}	
#' Safe Linear Mixed-Effects Model Fitting
#'
#' @description
#' This function fits a linear mixed-effects model using the `lme4::lmer` function, 
#' suppressing warnings and messages during the fitting process. It also checks 
#' for model singularity and displays a message if the model is singular.
#'
#' @param formula A formula specifying the fixed and random effects structure of the model.
#' @param data A data frame containing the variables referenced in the formula.
#' @return An object of class `merMod` representing the fitted model.
#' @details
#' The function uses `lme4::lmer` to fit the model and checks for singularity using 
#' `lme4::isSingular`. If the model is singular, it prints a warning message. 
#' Suppressing warnings and messages ensures a cleaner output when fitting the model.
#'
#' @importFrom lme4 lmer isSingular
#' @export
#' @author juanprimate
#' @examples
#' # Example usage:
#' # Assuming `resistors` is a data frame with appropriate columns:
#' # model <- safe_lmer(mohms ~ 1 + (1 | part), data = resistors)
#' # summary(model)
safe_lmer <- function(formula, data) {
  model <- suppressMessages(suppressWarnings(
    lme4::lmer(formula, data = data)
  ))
  # Verificar singularidad y mostrar mensaje si es necesario
  if (lme4::isSingular(model)) {
    print(c(
      "Boundary (singular) fit detected: see help('isSingular') for details.",
      "One or more models are singular. Results may not be valid."))
  }  
  return(model)
}
#' Ajuste de modelo lineal mixto con verificación de singularidad
#'
#' Esta función ajusta un modelo lineal mixto usando `lmer` de lme4, con la opción de manejar valores faltantes y singularidad.
#' Si se detecta un ajuste singular, se muestra un mensaje de advertencia.
#'
#' @param formula Una fórmula en el formato estándar de R para modelos mixtos (por ejemplo, `y ~ x + (1|group)`).
#' @param data Un `data.frame` que contiene las variables utilizadas en la fórmula.
#' @param REML Un valor lógico que indica si se debe usar la estimación de máxima verosimilitud restringida. Por defecto, es `TRUE`.
#' @param na.action Especifica cómo manejar los valores faltantes. Por defecto, es `na.omit` (omitir observaciones con valores faltantes).
#' 
#' @return Un objeto de clase `lmerMod` de la función `lmer`, que representa el modelo ajustado.
#' @details Si el ajuste del modelo es singular, es decir, si no se puede estimar una o más de las varianzas de los efectos aleatorios, se imprime un mensaje informando de este problema.
#' 
#' @author juanprimate
#' @export
#' @import lme4 isSingular lmer
safe_lmerBroad <- function(formula, data, REML = TRUE, na.action = na.omit) {
  model <- suppressMessages(suppressWarnings(
    lme4::lmer(formula, data = data, REML = TRUE, na.action = na.omit)
  ))
  # Verificar singularidad y mostrar mensaje si es necesario
  if (lme4::isSingular(model)) {
    print(c(
      "Boundary (singular) fit detected: see help('isSingular') for details.",
      "One or more models are singular. Results may not be valid."))
  }  
  return(model)
}
