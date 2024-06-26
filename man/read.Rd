% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{read_tidybrookes_csv}
\alias{read_tidybrookes_csv}
\alias{read_tidybrookes_delim}
\title{Load data from Epic in delimited (including CSV) format}
\usage{
read_tidybrookes_csv(
  file,
  col_types,
  n_max = Inf,
  na = c("", "NA"),
  quote = "\\"",
  tz = "Europe/London",
  nonexistent = NULL,
  ambiguous = NULL
)

read_tidybrookes_delim(
  file,
  col_types,
  delim = "|",
  n_max = Inf,
  na = c("", "NA"),
  quote = "\\""
)
}
\arguments{
\item{file}{Either a path to a file, a connection, or literal data (either a
single string or a raw vector), as per \code{\link[readr:read_delim]{readr::read_delim()}}}

\item{col_types}{Either a string specifying the name of the table that the
data comes from, for which a standard format can be used. Available values
are \code{adm}, \code{adt}, \code{demogs}, \code{fsheet}, \code{fsheet_io}, \code{tests}, \code{med_admin},
\code{med_prescr}, \code{diagnosis_pl}, \code{med_hist}, \code{radiology}. This is translated
into column specification using \code{\link{default_col_types}}.

Or a \code{\link[readr:cols]{readr::cols()}} format list of column names and types, which can be
used where a nonstandard data format are supplied.}

\item{n_max}{Maximum number of lines to read.}

\item{na}{Character vector of strings to interpret as missing values. Set
this option to \code{character()} to indicate no missing values.}

\item{quote}{Single character used to quote strings}

\item{tz}{A timezone name, such as \code{"Europe/London"}}

\item{nonexistent}{One of the following nonexistent time resolution
strategies, allowed to be either length 1, or the same length as the input:
\itemize{
\item \code{"roll-forward"}: The next valid instant in time.
\item \code{"roll-backward"}: The previous valid instant in time.
\item \code{"shift-forward"}: Shift the nonexistent time forward by the size of
the daylight saving time gap.
\item \verb{"shift-backward}: Shift the nonexistent time backward by the size of
the daylight saving time gap.
\item \code{"NA"}: Replace nonexistent times with \code{NA}.
\item \code{"error"}: Error on nonexistent times.
}

Using either \code{"roll-forward"} or \code{"roll-backward"} is generally recommended
over shifting, as these two strategies maintain the \emph{relative ordering}
between elements of the input.

If \code{NULL}, defaults to \code{"error"}.}

\item{ambiguous}{Passed to \link[clock:date-time-parse]{clock::date-time-parse}, with the usual values
being one of the following ambiguous time resolution strategies, allowed to
be either length 1, or the same length as the input:
\itemize{
\item \code{"earliest"}: Of the two possible times, choose the earliest one.
\item \code{"latest"}: Of the two possible times, choose the latest one.
\item \code{"NA"}: Replace ambiguous times with \code{NA}.
\item \code{"error"}: Error on ambiguous times.
}}

\item{delim}{Single character used to separate fields within a record.}
}
\description{
Convenience wrapper around \link[readr:read_delim]{readr::read_csv} and \link[readr:read_delim]{readr::read_csv} that makes
it easier to set the \code{col_types} for standard data from Epic. Also makes
handling untimezoned data more robust through use of functions from
\code{\link[clock:parse_date_time]{clock::parse_date_time}}.
}
\details{
Named \code{col_types} are extracted using \code{\link[=default_col_types]{default_col_types()}}.

At transitions to and from daylight savings times, there are nonexistent and
ambiguous times. Using the parameters \code{nonexistent} and \code{ambiguous} the
handling of these can be made explicit and consistent.
}
\examples{
adm_file_path <- tidybrookes_example("adm.csv")

# the format when col_types = "adm"
default_col_types("adm")

read_tidybrookes_csv(
  file = adm_file_path,
  col_types = "adm"
)

read_tidybrookes_csv(
  file = adm_file_path,
  col_types = "adm",
  tz = "Europe/London",
  nonexistent = "roll-forward",
  ambiguous = "latest"
)

# view default cols() format - this is what will be used if a character
# string name of a table is used, such as "fsheet"
default_col_types("fsheet")

# custom col_types can be specified
read_tidybrookes_csv(
  file = adm_file_path,
  col_types = cols(
    STUDY_SUBJECT_DIGEST = col_character(),
    fsd_id = col_character())
)

# Can use the clock package to determine handling of ambiguous and
# nonexistent dates (due to transitions to and from daylight saving time)
read_tidybrookes_csv(
  file = adm_file_path,
  col_types = cols(
    STUDY_SUBJECT_DIGEST = col_character(),
    fsd_id = col_character()),
  tz = "Europe/London",
  nonexistent = "roll-forward",
  ambiguous = "latest"
)
}
\seealso{
Named column types are extracted using \code{\link[=default_col_types]{default_col_types()}}. The
raw column names, which are messy, can be cleaned using e.g.
\code{\link[=fsheet_rename]{fsheet_rename()}}
}
