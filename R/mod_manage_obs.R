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
    observeEvent(
      req(L2$Save()),
      {
        #browser()

        # If UUID already exist, update SQLite
        if (any(str_detect(names(Station$Metadata), "UUID"))) {

          browser()

          # Update Metadata table
          Metadata <- Station$Metadata %>%
            mutate(
              ProTime = as.character(as.POSIXlt(Sys.time(), tz = "UTC")),
              Analyst = "Raphael Mabit",
              Mail = "raphael.mabit@gmail.com"
            )

          qry <- glue::glue(
            "UPDATE Metadata
            SET Comment = '", Metadata$Comment,"',
                ProTime = '", Metadata$ProTime,"',
                Analyst = '", Metadata$Analyst,"',
                Mail = '", Metadata$Mail,"'
            WHERE UUID = '", Station$Metadata$UUID, "';"
          )

          DBI::dbSendStatement(DB$Con(), qry)

          # Update HOCRL1b table
          HOCRL1b <- Station$HOCR$L1b %>%
            unnest(cols = c(AproxData))

          # List L1bID the link between the instruments spectrum.
          SumQC <- HOCRL1b %>% group_by(UUID, ID, QC) %>% summarise()

          # Create specific query for each L1bID
          # As we only update QC, the pair (UUID, ID) is a primary key of the QC column
          # And now I realize that HOCRL1b does not complies with tidy data principles
          # a signle observational unit by table. Should a specific table be created for QC data ?

          qry <- glue::glue_sql_collapse(purrr::pmap_chr(
            list(..1 = SumQC$UUID, ..2 = SumQC$ID , ..3 = SumQC$QC),
            .f = ~ glue::glue(
              "UPDATE HOCRL1b
              SET QC = '", ..3,"'
              WHERE (UUID || ID = '", glue::glue(..1, ..2), "');"
            )
          ))

          DBI::dbSendStatement(DB$Con(), qry)

          # Update HOCRL2 table
          # HOCRL2 <- Station$HOCR$L2
          #
          # qry <- glue::glue(
          #   "UPDATE HOCRL2
          #   SET Rrs = ", HOCRL2$Rrs,",
          #       KLu = ", HOCRL2$KLu,"
          #   WHERE UUID = ", Station$Metadata$UUID, ";"
          # )
          #
          # DBI::dbSendStatement(DB$Con(), qry)

        } else {

          browser()

          ObsUUID <- uuid::UUIDgenerate(
            use.time = T,
            output = "string"
          )

          # Good explanation of the difference between UUID and hash,
          # This way could create collision as it does not guarantee uniqueness
          # openssl::md5(paste0(as.character(ObsMeta),
          #                     as.character(HOCRL1b),
          #                     as.character(AOPs),
          #                     collapse = ""))

          Metadata <- Station$Metadata %>%
            mutate(
              UUID = ObsUUID,
              ProTime = as.character(as.POSIXlt(Sys.time(), tz = "UTC")),
              Analyst = "Raphael Mabit",
              Mail = "raphael.mabit@gmail.com"
              )

          HOCRL1b <- Station$HOCR$L1b %>%
            unnest(cols = c(AproxData)) %>%
            select(!CalData) %>%
            mutate(UUID = ObsUUID)

          HOCRL2 <- Station$HOCR$L2 %>%
            mutate(UUID = ObsUUID)

          DBI::dbWriteTable(DB$Con(), "Metadata", Metadata, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "HOCRL1b", HOCRL1b, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "HOCRL2", HOCRL2, append = TRUE)

        }

        DB$ObsMeta(reactive(tibble(DBI::dbGetQuery(DB$Con(), "SELECT * FROM Metadata"))))

      })

# Delete button remove data from SQLite -----------------------------------

    observeEvent(
      req(L2$Delete()),
      {
        #browser()

        qry <- glue::glue("DELETE FROM Metadata WHERE UUID='",Station$Metadata$UUID,"';")

        DBI::dbSendStatement(DB$Con(), qry)

        DB$ObsMeta(reactive(tibble(DBI::dbGetQuery(DB$Con(), "SELECT * FROM Metadata"))))

      })

  })
}




## To be copied in the UI
# mod_manage_obs_ui("manage_obs")

## To be copied in the server
# mod_manage_obs_server("manage_obs")
