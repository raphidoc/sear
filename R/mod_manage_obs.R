#' obs_manager UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_manage_obs_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # shinyFeedback::useShinyFeedback(),
    tags$head(tags$script(src = "message-handler.js")),
    uiOutput(ns("Delete")),
    uiOutput(ns("Save"))
  )
}

#' obs_manager Server Functions
#'
#' @noRd
mod_manage_obs_server <- function(id, DB, L2, L1aSelect, L2Select, Obs, L2Obs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$Save <- renderUI({
      validate(need(nrow(Obs$HOCR$L2) != 0, message = "Process to L2 before saving"))

      actionButton(ns("Save"), "Save", icon = icon("glyphicon glyphicon-save", lib = "glyphicon"))

    })

    output$Delete <- renderUI({
      req(!is.na(Obs$Metadata$UUID))

      actionButton(ns("Delete"), "Delete", class = "btn btn-danger", icon = icon("glyphicon glyphicon-trash", lib = "glyphicon"))

    })

    observeEvent(
      c(
        L2Select$SelUUID()
      ),
      {
        # Metadata
        # Have to query data based on UUID
        qry <- paste0("SELECT * FROM Metadata WHERE UUID IN ('", paste0(L2Select$SelUUID(), collapse = "','"), "');")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        L2Obs$Metadata <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # Have to query data based on UUID
        qry <- paste0("SELECT * FROM MetadataL1b WHERE UUID IN ('", paste0(L2Select$SelUUID(), collapse = "','"), "');")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        L2Obs$MetadataL1b <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # HOCR
        qry <- paste0("SELECT * FROM HOCRL2 WHERE UUID IN ('", paste0(L2Select$SelUUID(), collapse = "','"), "');")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        L2Obs$HOCR <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # SBE19
        qry <- paste0("SELECT * FROM SBE19L2 WHERE UUID IN ('", paste0(L2Select$SelUUID(), collapse = "','"), "');")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        L2Obs$SBE19 <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # SeaOWL
        qry <- paste0("SELECT * FROM SeaOWLL2 WHERE UUID IN ('", paste0(L2Select$SelUUID(), collapse = "','"), "');")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        L2Obs$SeaOWL <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # BBFL2
        qry <- paste0("SELECT * FROM BBFL2L2 WHERE UUID IN ('", paste0(L2Select$SelUUID(), collapse = "','"), "');")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        L2Obs$BBFL2 <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # Biosonic
        qry <- paste0("SELECT * FROM BioSonicL2 WHERE UUID IN ('", paste0(L2Select$SelUUID(), collapse = "','"), "');")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        L2Obs$BioSonic <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

      }
    )


    # UUID selection event trigger data display -------------------------------
    # by populating the Obs reactiveValues data table needed
    observeEvent(
      c(
        L1aSelect$SelUUID()
      ),
      {
        # Metadata
        # Have to query data based on UUID
        qry <- paste0("SELECT * FROM Metadata WHERE UUID='", L1aSelect$SelUUID(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$Metadata <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # MetadataL1b
        qry <- paste0("SELECT * FROM MetadataL1b WHERE UUID='", L1aSelect$SelUUID(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$MetadataL1b <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # HOCR
        # UUID have to ship with Instrument and SN to be passed to HOCR$L2
        qry <- paste0("SELECT * FROM HOCRL1b WHERE UUID='", L1aSelect$SelUUID(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$HOCR$L1b <- tibble(DBI::dbFetch(res)) %>%
          group_by(ID) %>%
          nest(AproxData = !matches("Instrument|SN|UUID"))
        DBI::dbClearResult(res)

        qry <- paste0("SELECT * FROM HOCRL2 WHERE UUID='", L1aSelect$SelUUID(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$HOCR$L2 <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # SBE19
        qry <- paste0("SELECT * FROM SBE19L1b WHERE UUID='", L1aSelect$SelUUID(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$SBE19$L1b <- tibble(DBI::dbFetch(res)) %>%
          group_by(ID) %>%
          nest(Data = !matches("Parameter|UUID"))
        DBI::dbClearResult(res)

        qry <- paste0("SELECT * FROM SBE19L2 WHERE UUID='", L1aSelect$SelUUID(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$SBE19$L2 <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # SeaOWL
        qry <- paste0("SELECT * FROM SeaOWLL1b WHERE UUID='", L1aSelect$SelUUID(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$SeaOWL$L1b <- tibble(DBI::dbFetch(res)) %>%
          group_by(ID) %>%
          nest(Data = !matches("Parameter|UUID"))
        DBI::dbClearResult(res)

        qry <- paste0("SELECT * FROM SeaOWLL2 WHERE UUID='", L1aSelect$SelUUID(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$SeaOWL$L2 <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # BBFL2
        qry <- paste0("SELECT * FROM BBFL2L1b WHERE UUID='", L1aSelect$SelUUID(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$BBFL2$L1b <- tibble(DBI::dbFetch(res)) %>%
          group_by(ID) %>%
          nest(Data = !matches("Parameter|UUID"))
        DBI::dbClearResult(res)

        qry <- paste0("SELECT * FROM BBFL2L2 WHERE UUID='", L1aSelect$SelUUID(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$BBFL2$L2 <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # BioSonic
        qry <- paste0("SELECT * FROM BioSonicL1b WHERE UUID='", L1aSelect$SelUUID(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$BioSonic$L1b <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        qry <- paste0("SELECT * FROM BioSonicL2 WHERE UUID='", L1aSelect$SelUUID(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$BioSonic$L2 <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

      }
    )

# Save to SQLite ----------------------------------------------------------
    UUID <- reactiveVal()

    observeEvent(
      req(input$Save),
      {

        # Sanitize tables ---------------------------------------------------------
        # Remove variables used for dev of Rb retrieval that don't belong in tables

        Obs$HOCR$L2 <- Obs$HOCR$L2 %>%
          select(matches("Wavelength|Rrs|Rrs_loess|KLu|KLu_loess|RbI|UUID"))

        # Does UUID is present in Metadata colnames ?
        # Now it is initiated on start so always TRUE
        UUIDPresent <- any(str_detect(names(Obs$Metadata), "UUID"))

        # Does UUID exist in database, check ObsMeta
        if (UUIDPresent) {
          UUIDExist <- any(Obs$Metadata$UUID %in% DB$ObsMeta()$UUID)
        } else {
          UUIDExist <- F
        }

        # If UUID already exist, update record in SQLite
        if (UUIDPresent & UUIDExist) {

# Update Metadata ---------------------------------------------------------

          Metadata <- Obs$Metadata %>%
            mutate(
              ProTime = as.character(as.POSIXlt(Sys.time(), tz = "UTC")),
              Analyst = "Raphael Mabit",
              Mail = "raphael.mabit@gmail.com"
            )

          qry <- glue::glue(
            "UPDATE Metadata
            SET Comment = '", Metadata$Comment, "',
                ProTime = '", Metadata$ProTime, "',
                Analyst = '", Metadata$Analyst, "',
                Mail = '", Metadata$Mail, "'
            WHERE UUID = '", Obs$Metadata$UUID, "';"
          )

          # Execute the statement and return the number of line affected
          MetaUp <- DBI::dbExecute(DB$Con(), qry)


# Update HOCR -------------------------------------------------------------

          HOCRL1b <- Obs$HOCR$L1b %>%
            unnest(cols = c(AproxData))

          # List L1bID the link between the instruments spectrum.
          SumQC <- HOCRL1b %>%
            group_by(UUID, ID, QC) %>%
            summarise()

          # Create specific query for each L1bID
          # As we only update QC, the pair (UUID, ID) is a primary key of the QC column
          # And now I realize that HOCRL1b does not complies with tidy data principles
          # a signle observational unit by table. Should a specific table be created for QC data ?

          qry <- purrr::pmap_chr(
            list(..1 = SumQC$UUID, ..2 = SumQC$ID, ..3 = SumQC$QC),
            .f = ~ glue::glue(
              "
              UPDATE HOCRL1b
              SET QC = '", ..3, "'
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
          HOCRL1bUp <- unlist(purrr::map(qry, ~ DBI::dbExecute(DB$Con(), glue::glue_sql(.x))))

          # Update HOCR L2 table
          HOCRL2 <- Obs$HOCR$L2

          # Individual CASE WHEN for each variables to change: Rrs, KLu
          # As the WHERE constraint on UUID is already present on the final query could remove UUID from CASE WHEN
          qryRrs <- glue::glue_sql_collapse(purrr::pmap_chr(
            list(..1 = HOCRL2$UUID, ..2 = HOCRL2$Wavelength, ..3 = HOCRL2$Rrs),
            .f = ~ glue::glue(
              "WHEN UUID = '", ..1, "' AND Wavelength = ", ..2, " THEN ", ..3
            )
          ), sep = "\n")

          qryRrs_loess <- glue::glue_sql_collapse(purrr::pmap_chr(
            list(..1 = HOCRL2$UUID, ..2 = HOCRL2$Wavelength, ..3 = HOCRL2$Rrs_loess),
            .f = ~ glue::glue(
              "WHEN UUID = '", ..1, "' AND Wavelength = ", ..2, " THEN ", ..3
            )
          ), sep = "\n")

          qryKLu <- glue::glue_sql_collapse(purrr::pmap_chr(
            list(..1 = HOCRL2$UUID, ..2 = HOCRL2$Wavelength, ..3 = HOCRL2$KLu),
            .f = ~ glue::glue(
              "WHEN UUID = '", ..1, "' AND Wavelength = ", ..2, " THEN ", ..3
            )
          ), sep = "\n")

          qryKLu_loess <- glue::glue_sql_collapse(purrr::pmap_chr(
            list(..1 = HOCRL2$UUID, ..2 = HOCRL2$Wavelength, ..3 = HOCRL2$KLu_loess),
            .f = ~ glue::glue(
              "WHEN UUID = '", ..1, "' AND Wavelength = ", ..2, " THEN ", ..3
            )
          ), sep = "\n")

          qryRbI <- glue::glue_sql_collapse(purrr::pmap_chr(
            list(..1 = HOCRL2$UUID, ..2 = HOCRL2$Wavelength, ..3 = HOCRL2$RbI),
            .f = ~ glue::glue(
              "WHEN UUID = '", ..1, "' AND Wavelength = ", ..2, " THEN ", ..3
            )
          ), sep = "\n")

          # Assemble query
          qry <- glue::glue_sql(
            "UPDATE HOCRL2
            SET Rrs = CASE
                  ", qryRrs, "
                  ELSE Rrs
                  END,
                Rrs_loess = CASE
                  ", qryRrs_loess, "
                  ELSE Rrs_loess
                  END,
                KLu = CASE
                  ", qryKLu, "
                  ELSE KLu
                  END,
                KLu_loess = CASE
                  ", qryKLu_loess, "
                  ELSE KLu_loess
                  END,
                RbI = CASE
                  ", qryRbI, "
                  ELSE RbI
                  END
            WHERE UUID = '", Obs$Metadata$UUID, "';"
          )

          # NA value in R are equal to NULL in SQL
          qry <- glue::glue_sql(stringr::str_replace_all(qry, "NA", "NULL"))

          # Execute the statement and return the number of line affected
          HOCRL2Up <- DBI::dbExecute(DB$Con(), qry)


# Update SBE19 ------------------------------------------------------------

          SBE19L1bUp <- update_L1b_param_val(L1b = Obs$SBE19$L1b, TableName = "SBE19L1b", Con = DB$Con())

          SBE19L2Up <- update_L2_param_val(L2 = Obs$SBE19$L2,TableName = "SBE19L2", Con = DB$Con())

# Update SeaOWL ------------------------------------------------------------

          SeaOWLL1bUp <- update_L1b_param_val(L1b = Obs$SeaOWL$L1b, TableName = "SeaOWLL1b", Con = DB$Con())

          SeaOWLL2Up <- update_L2_param_val(L2 = Obs$SeaOWL$L2,TableName = "SeaOWLL2", Con = DB$Con())


# Update BBFL2 ------------------------------------------------------------

          BBFL2L1bUp <- update_L1b_param_val(L1b = Obs$BBFL2$L1b, TableName = "BBFL2L1b", Con = DB$Con())

          BBFL2L2Up <- update_L2_param_val(L2 = Obs$BBFL2$L2,TableName = "BBFL2L2", Con = DB$Con())


# Update BioSonic ---------------------------------------------------------

          # there is no BioSonic data manipulation for now so no need to update


# Update feedback UI ------------------------------------------------------

          # Check that the number of line affected is correct, should probably improve this
          # MetaUp must be only one line affected, if more means UUID collision
          # L1bUp == 3 * 137 wavelengths * bins number
          # L2Up == User input wavelength
          test <- all(MetaUp == 1)#, unique(HOCRL1bUp) == 411, HOCRL2Up == 150)

          # Feedback to the user
          session$sendCustomMessage(
            type = "testmessage",
            message =
              glue::glue(
                "Metadata: ", MetaUp, " entry updated\n",
                "HOCRL1b: ", sum(HOCRL1bUp), " entry updated\n",
                "HOCRL2: ", HOCRL2Up, " entry updated\n"
              )
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


# Initial save if UUID doesn't exist --------------------------------------

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


          Metadata <- Obs$Metadata %>%
            mutate(
              UUID = ObsUUID,
              ProTime = as.character(as.POSIXlt(Sys.time(), tz = "UTC")),
              Analyst = "Raphael Mabit",
              Mail = "raphael.mabit@gmail.com"
            )

          MetadataL1b <- Obs$MetadataL1b %>%
            mutate(
              UUID = ObsUUID
            )

          HOCRL1b <- Obs$HOCR$L1b %>%
            unnest(cols = c(AproxData)) %>%
            mutate(
              UUID = ObsUUID
              )

          HOCRL2 <- Obs$HOCR$L2 %>%
            mutate(UUID = ObsUUID)

          SBE19L1b <- Obs$SBE19$L1b %>%
            unnest(c(Data)) %>%
            mutate(
              UUID = ObsUUID
              )

          SBE19L2 <- Obs$SBE19$L2  %>%
            mutate(UUID = ObsUUID)

          SeaOWLL1b <- Obs$SeaOWL$L1b %>%
            unnest(c(Data)) %>%
            mutate(
              UUID = ObsUUID
              )

          SeaOWLL2 <- Obs$SeaOWL$L2  %>%
            mutate(UUID = ObsUUID)

          BBFL2L1b <- Obs$BBFL2$L1b %>%
            unnest(c(Data)) %>%
            mutate(
              UUID = ObsUUID
              )

          BBFL2L2 <- Obs$BBFL2$L2  %>%
            mutate(UUID = ObsUUID)

          BioSonicL1b <- Obs$BioSonic$L1b  %>%
            mutate(
              UUID = ObsUUID
              )

          BioSonicL2 <- Obs$BioSonic$L2  %>%
            mutate(UUID = ObsUUID)

          DBI::dbWriteTable(DB$Con(), "Metadata", Metadata, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "MetadataL1b", MetadataL1b, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "HOCRL1b", HOCRL1b, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "HOCRL2", HOCRL2, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "SBE19L1b", SBE19L1b, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "SBE19L2", SBE19L2, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "SeaOWLL1b", SeaOWLL1b, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "SeaOWLL2", SeaOWLL2, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "BBFL2L1b", BBFL2L1b, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "BBFL2L2", BBFL2L2, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "BioSonicL1b", BioSonicL1b, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "BioSonicL2", BioSonicL2, append = TRUE)

          # Feedback to the user
          session$sendCustomMessage(
            type = "testmessage",
            message = "Saved"
            # glue::glue(
            #   "Metadata: ",MetaUp," entry updated\n",
            #   "HOCRL1b: ", sum(L1bUp)," entry updated\n",
            #   "HOCRL2: ",L2Up," entry updated\n")
          )

          # Update the list of observation
          DB$ObsMeta(tibble(DBI::dbGetQuery(DB$Con(), "SELECT * FROM Metadata")))

          qry <- glue::glue_sql("SELECT * FROM Metadata WHERE UUID = '", ObsUUID, "';")

          Obs$Metadata <- tibble(DBI::dbGetQuery(DB$Con(), qry))
        }
      }
    )

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

      qry <- glue::glue("DELETE FROM Metadata WHERE UUID='", Obs$Metadata$UUID, "';")

      LineDel <- DBI::dbExecute(DB$Con(), qry)

      # Feedback to the user
      session$sendCustomMessage(
        type = "testmessage",
        message =
          glue::glue(
            LineDel, " line deleted\n"
          )
      )

      # Update the list of observation
      DB$ObsMeta(tibble(DBI::dbGetQuery(DB$Con(), "SELECT * FROM Metadata")))
    })

    # If user cancel
    observeEvent(input$cancel, {
      removeModal()
    })

    # Module export -----------------------------------------------------------
    list(
      Save = reactive(input$Save),
      UUID = UUID
    )
  })
}




## To be copied in the UI
# mod_manage_obs_ui("manage_obs")

## To be copied in the server
# mod_manage_obs_server("manage_obs")
