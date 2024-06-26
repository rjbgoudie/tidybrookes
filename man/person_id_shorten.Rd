% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{person_id_shorten}
\alias{person_id_shorten}
\title{Find shortest possible unique person_id}
\usage{
person_id_shorten(person_id)
}
\arguments{
\item{person_id}{A character vector of person IDs to be shorten}
}
\value{
A character vector of the shortened person IDs
}
\description{
Finds the shortest substring of \code{person_id} that retains uniqueness. All
substrings are the first few characters of the \code{person_id}s.
}
\details{
Shortened person_ids are added by \code{\link[=adm_annotate]{adm_annotate()}} using this function.
}
\examples{

person_id_shorten(c("AAA", "ABC", "ACD", "ADE"))

person_id_shorten(c("XAAA", "XABC", "XACD", "XADE"))
}
\seealso{
\code{\link[=adm_annotate]{adm_annotate()}}
}
