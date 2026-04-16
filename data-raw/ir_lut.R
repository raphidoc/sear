## code to prepare `ir_lut` dataset goes here
library(dplyr)

ir_lut <- readr::read_csv(fs::path_package("sear", "extdata", "ir_midlat-summer_lut.csv"))

ir_lut <- ir_lut %>%
  mutate(
    wavelength = wavelength * 1000,
    ir = i_diffuse/i_direct
  )

ir_lut <- ir_lut %>%
  filter(sol_zen != 80) %>% # Produce NaN, limit of the 6S model !
  group_by(wavelength, sol_zen) %>%
  dplyr::summarise(
    across(where(is.numeric),
           list(
             mean = ~ mean(.x, na.rm = T),
             median = ~ median(.x, na.rm = T),
             sd = ~ sd(.x, na.rm = T),
             min = ~ min(.x, na.rm = T),
             max = ~ max(.x, na.rm = T)
           ),
           .names= "{.col}_{.fn}")
  ) %>%
  ungroup() %>%
  select(wavelength, sol_zen, ir_mean, ir_median, ir_sd, ir_min, ir_max)

usethis::use_data(ir_lut, overwrite = TRUE)
