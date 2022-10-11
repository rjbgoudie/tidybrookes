facet_grid_symbol <- function(){
  list(
    facet_grid(symbol ~ ., scales = "free_y", switch = "y"),
    theme(strip.placement = "outside"))
}

facet_grid_person <- function(){
  list(
    facet_grid(person_id_short ~ ., scales = "free_y", switch = "y"),
    theme(strip.placement = "outside"))
}

facet_grid_person_symbol <- function(){
  list(
    facet_grid(person_id_short ~ symbol, scales = "free_y", switch = "y"),
    theme(strip.placement = "outside"))
}

facet_grid_symbol_person <- function(){
  list(
    facet_grid(symbol ~ person_id_short, scales = "free_y", switch = "y"),
    theme(strip.placement = "outside"))
}



remove_all_but_last_x_axis <- function(x){
  purrr::map_at(x, -length(x),
                function(a){
                  a + theme(axis.text.x = element_blank(),
                            axis.ticks.x = element_blank()) +
                    xlab(NULL)
                } )
}

remove_all_but_first_y_axis <- function(x){
  purrr::map_at(x, -1,
                function(a){
                  a + theme(axis.text.y = element_blank(),
                            axis.ticks.y = element_blank()) +
                    ylab(NULL)
                } )
}


x_since_visit_start_days <- function(limits = function(.x) c(0 , max(.x))){
  list(aes(x = days_since_visit_start),
       scale_x_continuous(limits = limits),
       xlab("Time since visit start (days)"),
       geom_vline(aes(xintercept = 0), colour = "green"),
       geom_vline(aes(xintercept = visit_length_days), colour = "red"))
}

x_visit_start <- function(limits = NULL){
  list(aes(x = visit_start_datetime),
       scale_x_datetime_clean(limits = limits))
}

x_datetime <- function(limits = NULL){
  list(aes(x = datetime),
       scale_x_datetime_clean(limits = limits),
       geom_vline(aes(xintercept = visit_start_datetime), colour = "green"),
       geom_vline(aes(xintercept = visit_end_datetime), colour = "red"))
}

geom_numeric_point_logical_tile <- function(){
  list(
    geom_point(aes(y = value_as_number),
               data = ~ filter(.x, type == "numeric")),
    geom_tile(aes(y = symbol,
                  fill = value_as_logical),
              data = ~filter(.x, type == "logical")),
    scale_fill_manual(values = c(`TRUE` = "orange",
                                 `FALSE` = "blue"),
                      na.value = "grey50")
  )
}

geom_numeric_point_logical_point <- function(){
  list(
    geom_point(aes(y = value_as_number),
               data = ~ filter(.x, type == "numeric")),
    geom_point(aes(y = symbol,
                   colour = value_as_logical),
               data = ~filter(.x, type == "logical")),
    scale_colour_manual(values = c(`TRUE` = "orange",
                                   `FALSE` = "blue"),
                        na.value = "grey50")
  )
}

geom_numeric_point_lines_logical_point <- function(){
  list(
    geom_point(aes(y = value_as_number),
               data = ~ filter(.x, type == "numeric")),
    geom_line(aes(y = value_as_number,
                  group = symbol),
               data = ~ filter(.x, type == "numeric")),
    geom_point(aes(y = symbol,
                   colour = value_as_logical),
               data = ~filter(.x, type == "logical")),
    geom_text(aes(y = symbol,
                  colour = value_as_character,
                  label = value_as_character),
               data = ~filter(.x, type == "character")),
    scale_colour_manual(values = c(`TRUE` = "orange",
                                   `FALSE` = "blue"),
                        na.value = "grey50")
  )
}

common_x_scale <- function(p){
  purrr:::map(p, ~layer_scales(.x)$x$get_limits()) %>%
    unlist %>% range
}
common_y_scale <- function(p){
  purrr:::map(p, ~layer_scales(.x)$y$get_limits()) %>%
    unlist %>% range
}

scale_x_datetime_clean <- function(...){
  scale_x_datetime(...,
                   labels = scales::label_date_short())
}

gg_replace_data <- function(p, data){
  p$data <- data
  p
}

split_plot_by_type <- function(p, stack = "vertical"){
  x <- p$data

  x_numeric <- filter_numeric(x)
  x_logical <- filter_logical(x)
  x_character <- filter_character(x)

  x_list <- list(x_numeric, x_logical, x_character)
  x_list <- purrr::keep(x_list, function(x) nrow(x) > 0)

  p_list <- purrr::map(x_list, ~ p)
  plot_list <- purrr::map2(p_list, x_list, ~ gg_replace_data(.x, .y))

  if (stack == "vertical"){
    xr <- common_x_scale(plot_list)
    plot_list <- remove_all_but_last_x_axis(plot_list)
    Reduce("/", plot_list) &
      xlim(xr)
  } else {
    yr <- common_x_scale(plot_list)
    plot_list <- remove_all_but_first_y_axis(plot_list)
    Reduce("+", plot_list) &
      xlim(yr)
  }
}

over_by_type <- function(p){
  split_plot_by_type(p, stack = "vertical")
}

beside_by_type <- function(p){
  split_plot_by_type(p, stack = "horizontal")
}

tests_plot_numeric <- function(x, range_mainly_low, range_mainly_high, range_discard_below, range_discard_above){
  ggplot(x, aes(x = value_as_number)) +
    geom_histogram(bins = 30, boundary = 0) +
    geom_vline(xintercept = c(range_mainly_low, range_mainly_high),
               colour = "orange") +
    geom_vline(xintercept = c(range_discard_below, range_discard_above),
               colour = "red") +
    labs(x = "value") +
    ylab("Count")
}

tests_histogram_time_of_day <- function(x){
  if (any(!is.na(x$value_as_logical))){
    plot_time_of_day <- ggplot(x, aes(x = as_hms(datetime),
                                      fill = value_as_logical))
    cat("yes")
  } else {
    plot_time_of_day <- ggplot(x, aes(x = as_hms(datetime)))
  }

  plot_time_of_day +
    geom_histogram(bins = 48) +
    ggtitle("Time of day") +
    labs(x = NULL, y = "Count") +
    theme(legend.position = "bottom")
}

tests_histogram_time_of_day <- function(x){
  if (any(!is.na(x$value_as_logical))){
    plot_time_of_day <- ggplot(x, aes(x = as_hms(datetime),
                                      fill = value_as_logical))
    cat("yes")
  } else {
    plot_time_of_day <- ggplot(x, aes(x = as_hms(datetime)))
  }

  plot_time_of_day +
    geom_histogram(bins = 48) +
    ggtitle("Time of day") +
    labs(x = NULL, y = "Count") +
    theme(legend.position = "bottom")
}

tests_histogram_by_date <- function(x){
  ndays <- interval(min(x$datetime), max(x$datetime))/days(1)

  if (any(!is.na(x$value_as_logical))){
    plot_by_date <- ggplot(x, aes(x = datetime,
                                  fill = value_as_logical))
  } else {
    plot_by_date <- ggplot(x, aes(x = datetime))
  }

  plot_by_date +
    geom_histogram(bins = ndays) +
    labs(x = NULL, y = "Count") +
    theme(legend.position = "bottom") +
    scale_x_datetime(labels = scales::label_date_short())
}

tests_histogram_by_weekday <- function(x){
  if (any(!is.na(x$value_as_logical))){
    plot_by_weekday <- ggplot(x, aes(x = wday(datetime, label = TRUE),
                                     fill = value_as_logical))
  } else {
    plot_by_weekday <- ggplot(x, aes(x = wday(datetime, label = TRUE)))
  }

  plot_by_weekday +
    geom_bar() +
    labs(x = NULL, y = "Count") +
    theme(legend.position = "bottom")
}

plot_adt_department_col <- function(x, facet = TRUE){
  x <- x %>%
    filter(department_just_moved)
  f <- function(x){
    #stamp("22 Apr 1999 23:59")(x)
    format(x, "%d %b %Y %H:%M")
  }
  visit_start_datetime <- f(first(x$visit_start_datetime))
  visit_end_datetime <- f(first(x$visit_end_datetime))
  visit_length_days <- round(first(x$visit_length_days), 1)
  title <- glue("{visit_start_datetime} to {visit_end_datetime} ({visit_length_days} days)")
  out <- ggplot(x,
         aes(y = forcats::fct_rev(factor(department_visit_index)),
             yend = forcats::fct_rev(factor(department_visit_index)),
             colour = type_covid,
             x = department_start_datetime,
             xend = department_end_datetime)) +
    geom_segment(alpha = 0.5, size = 10) +
    geom_text(aes(label = department,
                   colour = type_covid,
                   x = department_start_datetime,
                   y = forcats::fct_rev(factor(department_visit_index))),
              hjust = -0.05,
              # vjust = 1,
              size = 3
    ) +
    labs(y = NULL) +
    ggtitle(title) +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.background = element_rect(colour = "black", fill = "white"),
          legend.position = "bottom",
          strip.placement = "outside")
  if (facet){
    out  +
      facet_grid(person_id_short + visit_id ~ .,
                 scales = "free",
                 space = "free_y",
                 switch = "both")
  } else{
    out
  }
}

plot_adt_department_timeline <- function(x){
  x <- x %>%
    filter(department_just_moved)
  ggplot(x,
         aes(ymax = interval(visit_start_datetime, end_datetime)/ddays(1),
             ymin = interval(visit_start_datetime, start_datetime)/ddays(1),
             colour = type_covid,
             x = 0)) +
    geom_linerange() +
    geom_point(aes(y = interval(visit_start_datetime, start_datetime)/ddays(1))) +
    geom_text(aes(label = department, y = interval(visit_start_datetime, start_datetime)/ddays(1)),
              hjust = "inward",
              vjust = "middle") +
    labs(x = NULL, y = "Time in department (days)") +
    scale_y_reverse() +
    xlim(0, 0.05) +
    facet_wrap(person_id_short ~ ., scales = "free") +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_blank())
}

#
tests_plot_point <- function(x){
  if (any(!is.na(x$value_as_logical))){
    plot_time_of_day <- ggplot(x, aes(x = datetime,
                                      fill = value_as_logical,
                                      colour = value_as_logical,
                                      y = symbol)) +
      geom_point()
  } else {
    plot_time_of_day <- ggplot(x, aes(x = datetime,
                                      y = value_as_number)) +
      geom_point()
  }

  plot_time_of_day +
    labs(x = NULL, y = "Value") +
    theme(legend.position = "bottom") +
    scale_x_datetime_clean()
}

tests_plot_tile <- function(x){
  if (any(!is.na(x$value_as_logical))){
    plot_time_of_day <- ggplot(x, aes(x = datetime,
                                      fill = value_as_logical,
                                      colour = value_as_logical,
                                      y = person_id)) +
      geom_tile()
  } else {
    plot_time_of_day <- ggplot(x, aes(x = datetime,
                                      y = person_id,
                                      fill = value_as_number)) +
      geom_tile()
  }

  plot_time_of_day +
    labs(x = NULL, y = "Value") +
    theme(legend.position = "bottom") +
    scale_x_datetime_clean()
}

plot_condition_gantt <- function(x, facet = TRUE){

  x <- x %>%
    group_by(person_id) %>%
    mutate(condition_index = 1:n())
  out <- ggplot(x,
                aes(y = forcats::fct_rev(factor(condition_index)),
                    yend = forcats::fct_rev(factor(condition_index)),
                    colour = symbol,
                    x = datetime,
                    xend =   resolved_datetime)) +
    geom_segment(alpha = 0.5, size = 10) +
    geom_text(aes(label = description_displayed,
                  colour = symbol,
                  x = datetime,
                  y = forcats::fct_rev(factor(condition_index))),
              hjust = -0.05,
              # vjust = 1,
              size = 3
    ) +
    labs(y = NULL) +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          strip.background = element_rect(colour = "black", fill = "white"),
          legend.position = "bottom",
          strip.placement = "outside")
  if (facet){
    out  +
      facet_grid(person_id_short + visit_id ~ .,
                 scales = "free",
                 space = "free_y",
                 switch = "both")
  } else{
    out
  }
}
