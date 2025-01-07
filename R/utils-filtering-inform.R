#' Filter, but report the filtering that occurs
#'
#' @param x A data frame
#' @param ... Passed to filter
#' @param since A character describing the reason for the filtering
#' @noRd
filter_inform <- function(x, ..., since = "for unknown reason"){
  previous <- nrow(x)
  out <- filter(x, ...)
  current <- nrow(out)
  if (current != previous){
    sign <- if_else(current < previous, "removed", "added")
    change_abs <- abs(current - previous)
    row <- if_else(change_abs == 1, "row", "rows")
    cli::cli_alert_info("{change_abs} {row} {sign} {since}")
  }
  out
}

#' Distinct, but report the filtering that occurs
#'
#' @param x A data frame
#' @param since A character describing the reason for the filtering
#' @noRd
distinct_inform <- function(x){
  since <- "since rows were exact duplicates"
  previous <- nrow(x)
  out <- distinct(x)
  current <- nrow(out)
  if (current != previous){
    sign <- if_else(current < previous, "removed", "added")
    change_abs <- abs(current - previous)
    row <- if_else(change_abs == 1, "row", "rows")
    cli::cli_alert_info("{change_abs} {row} {sign} {since}")
  }
  out
}

fn_inform <- function(x, fn, ..., since = "for unknown reason"){
  previous <- nrow(x)
  out <- fn(x, ...)
  current <- nrow(out)
  if (current != previous){
    sign <- if_else(current < previous, "removed", "added")
    change_abs <- abs(current - previous)
    row <- if_else(change_abs == 1, "row", "rows")
    cli::cli_alert_info("{change_abs} {row} {sign} {since}")
  }
  out
}

#' Check all rows of a data frame satisfy a condition
#'
#' @param x A data frame
#' @param condition A condition
#' @param name Character string, a name for the condition
#' @noRd
check_that_all <- function(x,
                           condition,
                           name = "Unnamed",
                           summary = identity){
  condition <- enquo(condition)
  unexpected <- x %>%
    mutate(satisfies_expect_before = !!condition) %>%
    filter(!satisfies_expect_before)

  if (nrow(unexpected) > 0){
    condition_str <- expr_print(condition)
    unexpected_nrow <- nrow(unexpected)
    row <- if_else(unexpected_nrow == 1, "row", "rows")
    cli::cli_alert_warning(
      c("{unexpected_nrow} {row} not satisfying ",
        "{name} condition: ",
        "{condition_str}"))
    print(summary(unexpected))
  }
}
