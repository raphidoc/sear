#' L2_param_val
#'
#' @description Take a L1b data tibble nested as `parameter`, `Data` pair.
#' Inside the Data tibble, the parameter value must be identified by `value`
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
L2_param_val <- function(L1b) {
  L2 <- L1b %>%
    unnest(c(Data)) %>%
    group_by(parameter) %>%
    filter(qc > 0) %>%
    summarize(value = median(value, na.rm = T)) %>%
    pivot_wider(
      names_from = parameter,
      values_from = value
    )

  # Populate uuid_l2 if exist
  if (any(names(L1b) == "uuid_l2")) {
    L2 <- L2 %>%
      mutate(uuid_l2 = unique(L1b$uuid_l2))
  }

  L2
}

update_L1b_param_val <- function(L1b, TableName, Con) {
  L1b <- L1b %>%
    unnest(cols = c(Data))

  # Individual CASE WHEN for variable to change: qc
  # As the WHERE constraint on uuid_l2 is already present on the final query could remove uuid_l2 from CASE WHEN
  qryQC <- glue::glue_sql_collapse(purrr::pmap_chr(
    list(..1 = L1b$parameter, ..2 = L1b$id, ..3 = L1b$qc),
    .f = ~ glue::glue(
      "WHEN parameter = '", ..1, "' AND id = ", ..2, " THEN ", ..3
    )
  ), sep = "\n")

  # Assemble query
  qry <- glue::glue_sql(
    "UPDATE ", TableName, "
            SET qc = CASE
                  ", qryQC, "
                  ELSE qc
                  END
            WHERE uuid_l2 = '", unique(L1b$uuid_l2), "';"
  )

  # NA value in R are equal to NULL in SQL
  qry <- glue::glue_sql(stringr::str_replace_all(qry, "NA", "NULL"))

  DBI::dbExecute(Con, qry)
}

update_L2_param_val <- function(L2, TableName, Con) {
  L2 <- L2 %>%
    pivot_longer(
      cols = !matches("uuid_l2"),
      names_to = "parameter",
      values_to = "value"
    )

  # Individual CASE WHEN for variable to change: qc
  # As the WHERE constraint on uuid_l2 is already present on the final query could remove uuid_l2 from CASE WHEN
  qryL2 <- glue::glue_sql_collapse(purrr::pmap_chr(
    list(..1 = L2$parameter, ..2 = L2$value),
    .f = ~ glue::glue(
      "'", ..1, "'  = ", ..2
    )
  ), sep = ",\n")

  # Assemble query
  qry <- glue::glue_sql(
    "UPDATE ", TableName, "
              SET ", qryL2, "
            WHERE uuid_l2 = '", unique(L2$uuid_l2), "';"
  )

  # NA value in R are equal to NULL in SQL
  qry <- glue::glue_sql(stringr::str_replace_all(qry, "NA", "NULL"))

  DBI::dbExecute(Con, qry)
}
