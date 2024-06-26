% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adm.R
\name{adm_annotate}
\alias{adm_annotate}
\title{Basic checks and annotation of ADM data}
\usage{
adm_annotate(x)
}
\arguments{
\item{x}{A tidied adm data frame, as tidied by \code{\link[=adm_rename]{adm_rename()}}}
}
\value{
The supplied data frame \code{x} with additional annotations
}
\description{
Performs basic checks and annotation of ADM data.
}
\details{
Checks that
\itemize{
\item all gender data are in known values (Female, Male or Unknown)
}

Annotates each row with
\itemize{
\item \code{visit_length_days}: length of stay (in days)
\item \code{person_id_short}: shortened \code{person_id} created using \code{\link[=person_id_shorten]{person_id_shorten()}}
}
}
\examples{
adm_data_example
adm_annotate(adm_data_example) \%>\%
  relocate(visit_length_days, person_id_short)
}
\seealso{
\code{\link[=adt_annotate]{adt_annotate()}} annotates ADT data
}
\author{
R.J.B. Goudie
}
