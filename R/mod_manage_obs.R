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

    #shinyFeedback::useShinyFeedback(),
    tags$head(tags$script(src = "message-handler.js")),

    actionButton(ns("Delete"), "Delete", class = "btn btn-danger", icon = icon("glyphicon glyphicon-trash", lib = "glyphicon")),
    actionButton(ns("Save"), "Save", icon = icon("glyphicon glyphicon-save", lib = "glyphicon"))

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

        # UUID have to ship with Instrument and SN to be passed to HOCR$L2
        qry <- paste0("SELECT * FROM HOCRL1b WHERE UUID='",SelData$SelUUID(),"';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Station$HOCR$L1b <- tibble(DBI::dbFetch(res)) %>%
          group_by(ID) %>%
          nest(AproxData = !matches("Instrument|SN|UUID"))
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
      req(input$Save),
      {

        # Does UUID is present in Metadata colnames ?
        UUIDPresent <- any(str_detect(names(Station$Metadata), "UUID"))

        # Does UUID exist in database, check ObsMeta
        if (UUIDPresent) {
          UUIDExist <- any(Station$Metadata$UUID %in% DB$ObsMeta()()$UUID)
        } else {
          UUIDExist <- F
        }

        # If UUID already exist, update record in SQLite
        if (UUIDPresent & UUIDExist) {

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

          # Execute the statement and return the number of line affected
          MetaUp <- DBI::dbExecute(DB$Con(), qry)

          # Update HOCRL1b table
          HOCRL1b <- Station$HOCR$L1b %>%
            unnest(cols = c(AproxData))

          # List L1bID the link between the instruments spectrum.
          SumQC <- HOCRL1b %>% group_by(UUID, ID, QC) %>% summarise()

          # Create specific query for each L1bID
          # As we only update QC, the pair (UUID, ID) is a primary key of the QC column
          # And now I realize that HOCRL1b does not complies with tidy data principles
          # a signle observational unit by table. Should a specific table be created for QC data ?

          qry <- purrr::pmap_chr(
            list(..1 = SumQC$UUID, ..2 = SumQC$ID , ..3 = SumQC$QC),
            .f = ~ glue::glue(
              "
              UPDATE HOCRL1b
              SET QC = '", ..3,"'
              WHERE (UUID || ID = '", glue::glue(..1, ..2), "');
              "
            )
          )

          # At the moment RSQLite does not provide support for multiple statement
          # in one query: https://github.com/r-dbi/RSQLite/issues/313
          # Have to send each query separately ...
          # Maybe raise an issue to provide support for that ?

          # Result is that we issue query to update QC everywhere, event if it doesn't change ...

          # Execute the statement and return the number of line affected
          L1bUp <- unlist(purrr::map(qry, ~ DBI::dbExecute(DB$Con(), glue::glue_sql(.x))))

          # Update HOCRL2 table
          HOCRL2 <- Station$HOCR$L2

          # Individual CASE WHEN for each variables to change: Rrs, KLu
          # As the WHERE constraint on UUID is already present on the final query could remove UUID from CASE WHEN
          qryRrs <- glue::glue_sql_collapse(purrr::pmap_chr(
            list(..1 = HOCRL2$UUID, ..2 = HOCRL2$Wavelength , ..3 = HOCRL2$Rrs, ..4 = HOCRL2$KLu),
            .f = ~ glue::glue(
              "WHEN UUID = '",..1,"' AND Wavelength = ",..2," THEN ",..3
            )
          ), sep = "\n")

          qryKLu <- glue::glue_sql_collapse(purrr::pmap_chr(
            list(..1 = HOCRL2$UUID, ..2 = HOCRL2$Wavelength , ..3 = HOCRL2$KLu),
            .f = ~ glue::glue(
              "WHEN UUID = '",..1,"' AND Wavelength = ",..2," THEN ",..3
            )
          ), sep = "\n")

          # Assemble query
          qry <- glue::glue_sql(
            "UPDATE HOCRL2
            SET Rrs = CASE
                  ", qryRrs,"
                  ELSE Rrs
                  END,
                KLu = CASE
                  ", qryKLu,"
                  ELSE KLu
                  END
            WHERE UUID = '",Station$Metadata$UUID,"';"
          )

          # NA value in R are equal to NULL in SQL
          qry <- glue::glue_sql(stringr::str_replace_all(qry, "NA", "NULL"))

          # Execute the statement and return the number of line affected
          L2Up <- DBI::dbExecute(DB$Con(), qry)

          # Check that the number of line affected is correct, should probably improve this
          # MetaUp must be only one line affected, if more means UUID collision
          # L1bUp == 3 * 137 wavelengths * bins number
          # L2Up == User input wavelength
          test <- all(MetaUp == 1, unique(L1bUp) == 411,  L2Up == 150)

          # Feedback to the user
          session$sendCustomMessage(
            type = 'testmessage',
            message =
              glue::glue(
                "Metadata: ",MetaUp," entry updated\n",
                "HOCRL1b: ", sum(L1bUp)," entry updated\n",
                "HOCRL2: ",L2Up," entry updated\n")
            )

          # Shiny feedback doesn't work with actionButton
          # shinyFeedback::feedbackSuccess(
          #   inputId = "Save",
          #   show = test,
          #   text = "Updated",
          #   color = "#5cb85c",
          #   icon = shiny::icon("ok", lib = "glyphicon"),
          #   session = shiny::getDefaultReactiveDomain()
          # )

        } else {

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
            mutate(UUID = ObsUUID)

          HOCRL2 <- Station$HOCR$L2 %>%
            mutate(UUID = ObsUUID)

          DBI::dbWriteTable(DB$Con(), "Metadata", Metadata, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "HOCRL1b", HOCRL1b, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "HOCRL2", HOCRL2, append = TRUE)

          # Feedback to the user
          session$sendCustomMessage(
            type = 'testmessage',
            message = "Saved"
            # glue::glue(
            #   "Metadata: ",MetaUp," entry updated\n",
            #   "HOCRL1b: ", sum(L1bUp)," entry updated\n",
            #   "HOCRL2: ",L2Up," entry updated\n")
          )

          # Update the list of observation
          DB$ObsMeta(reactive(tibble(DBI::dbGetQuery(DB$Con(), "SELECT * FROM Metadata"))))
        }

      })

# Delete button remove data from SQLite -----------------------------------

    modal_confirm <- modalDialog(
      "Are you sure you want to continue?",
      title = "Deleting files",
      footer = tagList(
        actionButton(ns("cancel"), "Cancel"),
        actionButton(ns("ok"), "Delete", class = "btn btn-danger")
      )
    )

    observeEvent(req(input$Delete), {
        showModal(modal_confirm)
      })

    # If user confirm delete
    observeEvent(input$ok, {
        removeModal()

        qry <- glue::glue("DELETE FROM Metadata WHERE UUID='",Station$Metadata$UUID,"';")

        LineDel <- DBI::dbExecute(DB$Con(), qry)

        # Feedback to the user
        session$sendCustomMessage(
          type = 'testmessage',
          message =
            glue::glue(
              LineDel," line deleted\n")
        )

        # Update the list of observation
        DB$ObsMeta(reactive(tibble(DBI::dbGetQuery(DB$Con(), "SELECT * FROM Metadata"))))
      })

    # If user cancel
    observeEvent(input$cancel, {
      removeModal()
    })

  })
}




## To be copied in the UI
# mod_manage_obs_ui("manage_obs")

## To be copied in the server
# mod_manage_obs_server("manage_obs")