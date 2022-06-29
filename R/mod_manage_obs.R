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
mod_manage_obs_server <- function(id, DB, L2, SelData, Station){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


# UUID selection event trigger data display -------------------------------
# by populating the reactive data table needed

    observeEvent(
      req(SelData$SelUUID()),
      {
        browser()
        SelData$SelUUID()

        # Have to query data based on UUID

        qry <- paste0("SELECT * FROM Metadata WHERE UUID='",SelData$SelUUID(),"';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Station$Metadata <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        qry <- paste0("SELECT * FROM HOCRL1b WHERE UUID='",SelData$SelUUID(),"';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Station$HOCR$L1b <- tibble(DBI::dbFetch(res)) %>%
          group_by(ID) %>%
          nest(AproxData = !matches("Instrument|SN"))
        DBI::dbClearResult(res)

        qry <- paste0("SELECT * FROM HOCRL2 WHERE UUID='",SelData$SelUUID(),"';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Station$HOCR$L2 <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        #dbDisconnect(con)

        # Station$Metadata <- tibble(DBI::dbReadTable(DB$Con(), "Metadata"))
        # Station$HOCR$L1b <- tibble(DBI::dbReadTable(DB$Con(), "HOCRL1b"))
        # Station$HOCR$L2 <- tibble(DBI::dbReadTable(DB$Con(), "HOCRL2"))


        # L2$StationTbl(Metadata)
        #
        # L2$HOCR()$L1bHOCR(HOCRL1b)
        #
        # L2$HOCR()$AOPs(HOCRL2)

      })

# Save button send data to SQLite -----------------------------------------
# If UUID already exist, update SQLite
    observeEvent(
      req(L2$Save()),
      {
        browser()

        ObsUUID <- uuid::UUIDgenerate(
          use.time = T,
          output = "string"
          )

        Metadata <- Station$Metadata %>%
          mutate(UUID = ObsUUID)

        HOCRL1b <- Station$HOCR$L1b %>%
          unnest(cols = c(AproxData)) %>%
          mutate(UUID = ObsUUID)

        HOCRL2 <- Station$HOCR$L2 %>%
          mutate(UUID = ObsUUID)

        # Good explanation of the difference between UUID and hash,
        # This way could create collision as it does not guarantee uniqueness
        # openssl::md5(paste0(as.character(ObsMeta),
        #                     as.character(HOCRL1b),
        #                     as.character(AOPs),
        #                     collapse = ""))

        DBI::dbWriteTable(DB$Con(), "Metadata", Metadata, append = TRUE)
        DBI::dbWriteTable(DB$Con(), "HOCRL1b", HOCRL1b, append = TRUE)
        DBI::dbWriteTable(DB$Con(), "HOCRL2", HOCRL2, append = TRUE)

        DB$ObsMeta(tibble(DBI::dbGetQuery(Con(), "SELECT * FROM Metadata")))

      })

  })
}

## To be copied in the UI
# mod_manage_obs_ui("manage_obs")

## To be copied in the server
# mod_manage_obs_server("manage_obs")
