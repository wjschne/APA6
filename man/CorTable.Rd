% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CorTable.R
\name{CorTable}
\alias{CorTable}
\title{APA Correlation Table}
\usage{
CorTable(x, caption = "", notes = "", alpha = 0.05, digits = 2,
  boldSig = TRUE, includeMeanSD = TRUE, includeCaptionNumber = TRUE,
  WidthProportion = 1, use = "pairwise.complete.obs",
  landscape = FALSE)
}
\arguments{
\item{x}{A matrix or data frame with numeric values.}

\item{caption}{Caption for the correlation table. Defaults to "".}

\item{notes}{Table notes.}

\item{alpha}{A numeric value to specify the significance level.}

\item{digits}{Number of digits after the decimal place.}

\item{boldSig}{Will bold significant correlations.}

\item{includeMeanSD}{Will include mean and standard deviations.}

\item{includeCaptionNumber}{Will include a caption number.}

\item{WidthProportion}{Adjusts the width of the table.}

\item{use}{Method for dealing with missing values.}

\item{landscape}{Makes page landscape orientation.}
}
\value{
APA correlation table
}
\description{
APA correlation tables are created from data frames using CorTable
}
