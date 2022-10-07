Tests and Coverage
================
07 October, 2022 17:58:19

- <a href="#coverage" id="toc-coverage">Coverage</a>
- <a href="#unit-tests" id="toc-unit-tests">Unit Tests</a>

This output is created by
[covrpage](https://github.com/yonicd/covrpage).

## Coverage

Coverage summary is created using the
[covr](https://github.com/r-lib/covr) package.

| Object                                                    | Coverage (%) |
|:----------------------------------------------------------|:------------:|
| sear                                                      |    40.06     |
| [R/fct_hocr.R](../R/fct_hocr.R)                           |     0.00     |
| [R/fct_mtelog.R](../R/fct_mtelog.R)                       |     0.00     |
| [R/mod_load_data.R](../R/mod_load_data.R)                 |     0.00     |
| [R/mod_station_bb3.R](../R/mod_station_bb3.R)             |     0.00     |
| [R/mod_station_ctd.R](../R/mod_station_ctd.R)             |     0.00     |
| [R/mod_station_hocr.R](../R/mod_station_hocr.R)           |     0.00     |
| [R/mod_station_seaowl.R](../R/mod_station_seaowl.R)       |     0.00     |
| [R/mod_transect_L1L2.R](../R/mod_transect_L1L2.R)         |     0.00     |
| [R/run_app.R](../R/run_app.R)                             |     0.00     |
| [R/utils_zoom_center.R](../R/utils_zoom_center.R)         |     0.00     |
| [R/mod_manage_obs.R](../R/mod_manage_obs.R)               |    22.44     |
| [R/mod_select_data.R](../R/mod_select_data.R)             |    30.45     |
| [R/mod_manage_DB.R](../R/mod_manage_DB.R)                 |    32.61     |
| [R/zzz.R](../R/zzz.R)                                     |    42.86     |
| [R/mod_manage_project.R](../R/mod_manage_project.R)       |    50.00     |
| [R/mod_parse_mtelog.R](../R/mod_parse_mtelog.R)           |    56.36     |
| [R/mod_load_mtelog.R](../R/mod_load_mtelog.R)             |    60.00     |
| [R/mod_load_cal.R](../R/mod_load_cal.R)                   |    66.67     |
| [R/mod_process_L1b.R](../R/mod_process_L1b.R)             |    68.52     |
| [R/app_ui.R](../R/app_ui.R)                               |    71.01     |
| [R/mod_station_L1L2.R](../R/mod_station_L1L2.R)           |    72.25     |
| [R/mod_select_instrument.R](../R/mod_select_instrument.R) |    75.00     |
| [R/golem_utils_server.R](../R/golem_utils_server.R)       |    77.78     |
| [R/golem_utils_ui.R](../R/golem_utils_ui.R)               |    87.94     |
| [R/app_config.R](../R/app_config.R)                       |    100.00    |
| [R/app_server.R](../R/app_server.R)                       |    100.00    |
| [R/fct_cal_hocr.R](../R/fct_cal_hocr.R)                   |    100.00    |

<br>

## Unit Tests

Unit Test summary is created using the
[testthat](https://github.com/r-lib/testthat) package.

| file                                                            |   n |    time | error | failed | skipped | warning | icon |
|:----------------------------------------------------------------|----:|--------:|------:|-------:|--------:|--------:|:-----|
| [test-app.R](testthat/test-app.R)                               |   1 |   0.083 |     0 |      0 |       0 |       0 |      |
| [test-fct_hocr.R](testthat/test-fct_hocr.R)                     |   2 | 134.661 |     0 |      0 |       0 |       1 | ⚠️   |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R) |  13 |   0.136 |     0 |      0 |       0 |       0 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R)         |  51 |   0.149 |     0 |      0 |       0 |       0 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R)   |  10 |   0.045 |     0 |      0 |       1 |       0 | 🔶   |

<details open>
<summary>
Show Detailed Test Results
</summary>

| file                                                                    | context            | test                           | status  |   n |    time | icon |
|:------------------------------------------------------------------------|:-------------------|:-------------------------------|:--------|----:|--------:|:-----|
| [test-app.R](testthat/test-app.R#L2)                                    | app                | multiplication works           | PASS    |   1 |   0.083 |      |
| [test-fct_hocr.R](testthat/test-fct_hocr.R#L4)                          | fct_hocr           | read hocr binary from MTE      | WARNING |   2 | 134.661 | ⚠️   |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L2)      | golem_utils_server | not_in works                   | PASS    |   2 |   0.006 |      |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L7)      | golem_utils_server | not_null works                 | PASS    |   2 |   0.075 |      |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L12)     | golem_utils_server | not_na works                   | PASS    |   2 |   0.013 |      |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L17_L22) | golem_utils_server | drop_nulls works               | PASS    |   1 |   0.007 |      |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L26_L29) | golem_utils_server | %\|\|% works                   | PASS    |   2 |   0.013 |      |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L37_L40) | golem_utils_server | %\|NA\|% works                 | PASS    |   2 |   0.012 |      |
| [test-golem_utils_server.R](testthat/test-golem_utils_server.R#L48_L50) | golem_utils_server | rv and rvtl work               | PASS    |   2 |   0.010 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L2)              | golem_utils_ui     | Test with_red_star works       | PASS    |   2 |   0.009 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L10)             | golem_utils_ui     | Test list_to_li works          | PASS    |   3 |   0.013 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L22_L28)         | golem_utils_ui     | Test list_to_p works           | PASS    |   3 |   0.014 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L53)             | golem_utils_ui     | Test named_to_li works         | PASS    |   3 |   0.016 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L66)             | golem_utils_ui     | Test tagRemoveAttributes works | PASS    |   4 |   0.014 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L82)             | golem_utils_ui     | Test undisplay works           | PASS    |   8 |   0.028 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L110)            | golem_utils_ui     | Test display works             | PASS    |   4 |   0.010 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L124)            | golem_utils_ui     | Test jq_hide works             | PASS    |   2 |   0.004 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L132)            | golem_utils_ui     | Test rep_br works              | PASS    |   2 |   0.003 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L140)            | golem_utils_ui     | Test enurl works               | PASS    |   2 |   0.004 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L148)            | golem_utils_ui     | Test columns wrappers works    | PASS    |  16 |   0.030 |      |
| [test-golem_utils_ui.R](testthat/test-golem_utils_ui.R#L172)            | golem_utils_ui     | Test make_action_button works  | PASS    |   2 |   0.004 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L3)        | golem-recommended  | app ui                         | PASS    |   2 |   0.024 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L13)       | golem-recommended  | app server                     | PASS    |   4 |   0.009 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L24_L26)   | golem-recommended  | app_sys works                  | PASS    |   1 |   0.004 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L36_L42)   | golem-recommended  | golem-config works             | PASS    |   2 |   0.007 |      |
| [test-golem-recommended.R](testthat/test-golem-recommended.R#L72)       | golem-recommended  | app launches                   | SKIPPED |   1 |   0.001 | 🔶   |

| Failed | Warning | Skipped |
|:-------|:--------|:--------|
| 🛑     | ⚠️      | 🔶      |

</details>
<details>
<summary>
Session Info
</summary>

| Field    | Value                        |
|:---------|:-----------------------------|
| Version  | R version 4.2.1 (2022-06-23) |
| Platform | x86_64-pc-linux-gnu (64-bit) |
| Running  | Pop!\_OS 22.04 LTS           |
| Language | en_US                        |
| Timezone | America/New_York             |

| Package  | Version |
|:---------|:--------|
| testthat | 3.1.4   |
| covr     | 3.6.1   |
| covrpage | 0.1     |

</details>
<!--- Final Status : skipped/warning --->
