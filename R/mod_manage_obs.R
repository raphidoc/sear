#' obs_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_manage_obs_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' obs_manager Server Functions
#'
#' @noRd
mod_manage_obs_server <- function(id, DB, L2){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



    observeEvent(
      req(L2$Save()),
      {
        browser()

        ObsUUID <- uuid::UUIDgenerate(
          use.time = T,
          output = "string"
          )

        ObsMeta <- L2$StationTbl() %>%
          mutate(UUID = ObsUUID)

        HOCRL1b <- L2$HOCR$L1bHOCR() %>%
          unnest(cols = c(AproxData)) %>%
          mutate(UUID = ObsUUID)

        AOPs <- L2$HOCR$AOPs() %>%
          mutate(UUID = ObsUUID)

        # Good explanation of the difference between UUID and hash,
        # This way could create collision as it does not guarantee uniqueness
        # openssl::md5(paste0(as.character(ObsMeta),
        #                     as.character(HOCRL1b),
        #                     as.character(AOPs),
        #                     collapse = ""))

        DBI::dbWriteTable(DB$Con(), "ObsMeta", ObsMeta, append = TRUE)
        DBI::dbWriteTable(DB$Con(), "HOCRL1b", HOCRL1b, append = TRUE)
        DBI::dbWriteTable(DB$Con(), "AOPs", AOPs, append = TRUE)

      })




  })
}

## To be copied in the UI
# mod_manage_obs_ui("manage_obs")

## To be copied in the server
# mod_manage_obs_server("manage_obs")
