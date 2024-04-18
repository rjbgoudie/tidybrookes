fixed_labels <-
  tribble(~department, ~is_ed,
          "ADD EMERGENCY DEPT", TRUE,
          "ADD C2 WARD", FALSE,
          "ADD D2 WARD", FALSE,
          "DIALYSIS CENTRE", FALSE,
          "ADD A1 WARD", FALSE,
          "ADD A2 WARD", FALSE,
          "ADD A3 WARD", FALSE,
          "ADD A4 WARD", FALSE,
          "ADD X WARD", FALSE,
          "ADD Y WARD", FALSE,
          "ADD Z WARD", FALSE)

adt_data_example <-
  read_tidybrookes_csv(
    file = tidybrookes_example("adt.csv"),
    col_types = "adt"
  ) %>%
  adt_rename %>%
  adt_annotate(annotate_fn = identity,
               fixed_labels = fixed_labels)

usethis::use_data(adt_data_example, overwrite = TRUE)
