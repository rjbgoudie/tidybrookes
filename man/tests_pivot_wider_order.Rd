% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tests.R
\name{tests_pivot_wider_order}
\alias{tests_pivot_wider_order}
\title{Widen test data}
\usage{
tests_pivot_wider_order(x, values_fn = NULL, include_censoring = FALSE)
}
\arguments{
\item{x}{A tidy test data frame}

\item{values_fn}{A function to summarise multiple results for a particular
test from a particular order.}

\item{include_censoring}{Logical, should censoring column be included?}
}
\value{
A data frame with the tests in columns
}
\description{
Reshapes the data so that all tests from the same \code{order_id} for the
same person appear in columns
}
\author{
R.J.B. Goudie
}
