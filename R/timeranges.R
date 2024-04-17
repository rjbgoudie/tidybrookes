#' Intersect two timeranges
#'
#' @export
intersect_timeranges <- function(timerange1, timerange2){
  stopifnot(all(!is.na(timerange1$start1_datetime),
                !is.na(timerange2$start2_datetime),
                !is.na(timerange1$end1_datetime),
                !is.na(timerange2$end2_datetime)))

  out <- full_join(timerange1 %>%
              mutate(interval1 = interval(start1_datetime, end1_datetime)),
            timerange2 %>%
              mutate(interval2 = interval(start2_datetime, end2_datetime))) %>%
    mutate(interval_intersect = lubridate::intersect(interval1, interval2)) %>%
    filter(int_length(interval_intersect) > 0) %>%
    mutate(start_datetime = int_start(interval_intersect),
           end_datetime = int_end(interval_intersect)) %>%
    select(-interval1,
           -interval2,
           -start1_datetime,
           -end1_datetime,
           -start2_datetime,
           -end2_datetime,
           -interval_intersect)
}
