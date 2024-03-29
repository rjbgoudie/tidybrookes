% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/radiology.R
\name{radiology_unrename}
\alias{radiology_unrename}
\title{Untidy raw radiology colnames}
\usage{
radiology_unrename(
  x,
  names = c(STUDY_SUBJECT_DIGEST = "person_id", Proc_Name = "name", Proc_Date =
    "procedure_datetime", Proc_Code = "code", Proc_Narrative = "narrative",
    Proc_Impression = "impression", Proc_Addenda = "addenda", Proc_Assessment =
    "assessment")
)
}
\arguments{
\item{x}{A data frame of raw radiology data with tidy names}

\item{names}{A vector of new_name = old_name pairs}
}
\value{
The supplied data frame, with column names in tidy-column-name format
}
\description{
The standard data format from Clinical Informatics is handled by default. If
variations from this format occur, custom renaming can be performed
using the \code{names} argument
}
\author{
R.J.B. Goudie
}
