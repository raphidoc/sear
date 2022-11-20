#' L2_param_val
#'
#' @description Take a L1b data tibble nested as `Parameter`, `Data` pair.
#' Inside the Data tibble, the parameter value must be identified by `Value`
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
L2_param_val <- function(L1b) {

  L1b %>%
    unnest(c(Data)) %>%
    group_by(Parameter) %>%
    filter(QC > 0) %>%
    summarize(Value = mean(Value)) %>%
    pivot_wider(
      names_from = Parameter,
      values_from = Value
    )
}
