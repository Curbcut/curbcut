% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/legend_breaks.R
\name{legend_breaks}
\alias{legend_breaks}
\alias{legend_breaks.q5}
\alias{legend_breaks.q5_ind}
\alias{legend_breaks.q100}
\alias{legend_breaks.qual}
\alias{legend_breaks.delta}
\alias{legend_breaks.bivar_ldelta_rq3}
\alias{legend_breaks.bivar}
\alias{legend_breaks.delta_bivar}
\title{Generic legend breaks function for Curbcut legends}
\usage{
legend_breaks(vars, ...)

\method{legend_breaks}{q5}(vars, data, ...)

\method{legend_breaks}{q5_ind}(vars, data, lang = NULL, ...)

\method{legend_breaks}{q100}(vars, lang = NULL, ...)

\method{legend_breaks}{qual}(vars, lang = NULL, ...)

\method{legend_breaks}{delta}(vars, data, ...)

\method{legend_breaks}{bivar_ldelta_rq3}(vars, data, time, ...)

\method{legend_breaks}{bivar}(vars, data, ...)

\method{legend_breaks}{delta_bivar}(vars, data, ...)
}
\arguments{
\item{vars}{<\verb{named list}> A list object with a pre-determined class. The
output of \code{\link{vars_build}}.}

\item{...}{Other arguments to be passed to the methods}

\item{data}{<\code{data.frame}> The current data. The output of \code{\link{data_get}}.}

\item{lang}{<\code{character}> String indicating the language to translate the
breaks to. Defaults to \code{NULL}, which is no translation.}

\item{time}{<\verb{reactive numeric vector}> The \code{time} at which data is displayed.
A list for var_left and var_right. The output of \code{\link{vars_build}}(...)$time.
Usually \code{r[[id]]$time}.}
}
\value{
It returns an output of \code{\link[ggplot2]{labs}}.
}
\description{
\code{legend_labels} is a generic function used to produce a vector of break
labels for the Curbcut legends. The function invokes
particular methods which depend on the class of the \code{vars} argument.
}
\section{Methods (by class)}{
\itemize{
\item \code{legend_breaks(q5)}: q5 method

\item \code{legend_breaks(q5_ind)}: q5_ind method

\item \code{legend_breaks(q100)}: q100 method

\item \code{legend_breaks(qual)}: qual method

\item \code{legend_breaks(delta)}: delta method

\item \code{legend_breaks(bivar_ldelta_rq3)}: bivar_ldelta_rq3 method

\item \code{legend_breaks(bivar)}: bivar method

\item \code{legend_breaks(delta_bivar)}: delta_bivar method

}}
