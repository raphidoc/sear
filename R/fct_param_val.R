#' L2_param_val
#'
#' @description Take a L1b data tibble nested as `Parameter`, `Data` pair.
#' Inside the Data tibble, the parameter value must be identified by `Value`
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
L2_param_val <- function(L1b) {
  L2 <- L1b %>%
    unnest(c(Data)) %>%
    group_by(Parameter, ) %>%
    filter(QC > 0) %>%
    summarize(Value = mean(Value)) %>%
    pivot_wider(
      names_from = Parameter,
      values_from = Value
    )

  # Populate UUID if exist
  if (any(names(L1b) == "UUID")) {
    L2 <- L2 %>%
      mutate(UUID = unique(L1b$UUID))
  }

  L2
}

update_L1b_param_val <- function(L1b, TableName, Con) {
  L1b <- L1b %>%
    unnest(cols = c(Data))

  # Individual CASE WHEN for variable to change: QC
  # As the WHERE constraint on UUID is already present on the final query could remove UUID from CASE WHEN
  qryQC <- glue::glue_sql_collapse(purrr::pmap_chr(
    list(..1 = L1b$Parameter, ..2 = L1b$ID, ..3 = L1b$QC),
    .f = ~ glue::glue(
      "WHEN Parameter = '", ..1, "' AND ID = ", ..2, " THEN ", ..3
    )
  ), sep = "\n")

  # Assemble query
  qry <- glue::glue_sql(
    "UPDATE ", TableName, "
            SET QC = CASE
                  ", qryQC, "
                  ELSE QC
                  END
            WHERE UUID = '", unique(L1b$UUID), "';"
  )

  # NA value in R are equal to NULL in SQL
  qry <- glue::glue_sql(stringr::str_replace_all(qry, "NA", "NULL"))

  DBI::dbExecute(Con, qry)
}

update_L2_param_val <- function(L2, TableName, Con) {
  L2 <- L2 %>%
    pivot_longer(
      cols = !matches("UUID"),
      names_to = "Parameter",
      values_to = "Value"
    )

  # Individual CASE WHEN for variable to change: QC
  # As the WHERE constraint on UUID is already present on the final query could remove UUID from CASE WHEN
  qryL2 <- glue::glue_sql_collapse(purrr::pmap_chr(
    list(..1 = L2$Parameter, ..2 = L2$Value),
    .f = ~ glue::glue(
      "'", ..1, "'  = ", ..2
    )
  ), sep = ",\n")

  # Assemble query
  qry <- glue::glue_sql(
    "UPDATE ", TableName, "
              SET ", qryL2, "
            WHERE UUID = '", unique(L2$UUID), "';"
  )

  # NA value in R are equal to NULL in SQL
  qry <- glue::glue_sql(stringr::str_replace_all(qry, "NA", "NULL"))

  DBI::dbExecute(Con, qry)
}
