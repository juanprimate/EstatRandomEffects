% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/semIteracao.R
\name{listaFuncionParte}
\alias{listaFuncionParte}
\title{Generate Analysis Results for Linear Mixed Model}
\usage{
listaFuncionParte(dataset, varY, formula, REML, fit, output = NULL)
}
\arguments{
\item{dataset}{A data frame containing the data used for fitting the model.}

\item{varY}{A character string representing the name of the dependent variable in the model.}

\item{formula}{A formula object specifying the model to be fitted.}

\item{REML}{A logical value indicating whether REML (TRUE) or maximum likelihood (FALSE) was used for fitting the model.}

\item{fit}{An object of class \code{lmerModLmerTest} or similar, which is the result of fitting the model.}

\item{output}{(Optional) A matrix representing the random effects output, used in the calculation of ANOVA results.}
}
\value{
A list with the following components:
\describe{
\item{covarianceMatrixFit}{The covariance matrix of the fixed effects.}
\item{randFitAnovaFit}{Results of the ANOVA on the random effects, processed by \code{randFitAnovaParte}.}
\item{contestFitResp}{Results from the \code{contestFit} function.}
\item{contest1DFitResp}{Results from the \code{contest1DFit} function.}
\item{contestMDFitResp}{Results from the \code{contestMDFit} function.}
\item{confintFitIntervalResp}{Confidence intervals of the fixed effects.}
\item{vcovLmerResp}{Variance-covariance matrix of the fixed effects.}
\item{VarCorrLmerResp}{Variance components of the random effects.}
}
}
\description{
The function takes a dataset, the name of the dependent variable, the model formula,
a logical value indicating whether REML was used, the fitted model object, and optional
output. It then calculates and returns several results related to the linear mixed model analysis.
}
\details{
This function processes the results from a linear mixed model and returns a list
containing various analysis outputs such as the covariance matrix, ANOVA results,
confidence intervals, and variance-covariance matrix.
}
\examples{
# Example usage
# Assuming `my_data` is a data frame, "response" is the dependent variable,
# and `fit` is the fitted model object.
results <- listaFuncionParte(
  dataset = my_data,
  varY = "response",
  formula = formula(response ~ (1|group)),
  REML = TRUE,
  fit = fitted_model
)

}
\author{
juanprimate
}
