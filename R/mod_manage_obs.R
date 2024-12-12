#' obs_manager UI Function
#'
#' @description a shiny Module.
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
      req(!is.na(Obs$metadata_l2$uuid_l2))

      actionButton(ns("Delete"), "Delete", class = "btn btn-danger", icon = icon("glyphicon glyphicon-trash", lib = "glyphicon"))
    })

    observeEvent(
      c(
        L2Select$Seluuid_l2()
      ),
      {
        # metadata_l2
        # Have to query data based on uuid_l2
        qry <- paste0("SELECT * FROM metadata_l2 WHERE uuid_l2 IN ('", paste0(L2Select$Seluuid_l2(), collapse = "','"), "');")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        L2Obs$metadata_l2 <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # Have to query data based on uuid_l2
        qry <- paste0("SELECT * FROM metadata_l1b WHERE uuid_l2 IN ('", paste0(L2Select$Seluuid_l2(), collapse = "','"), "');")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        L2Obs$metadata_l1b <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # HOCR
        qry <- paste0("SELECT * FROM hocr_l2 WHERE uuid_l2 IN ('", paste0(L2Select$Seluuid_l2(), collapse = "','"), "');")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        L2Obs$HOCR <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # SBE19
        qry <- paste0("SELECT * FROM sbe19_l2 WHERE uuid_l2 IN ('", paste0(L2Select$Seluuid_l2(), collapse = "','"), "');")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        L2Obs$SBE19 <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # SeaOWL
        qry <- paste0("SELECT * FROM seaowl_l2 WHERE uuid_l2 IN ('", paste0(L2Select$Seluuid_l2(), collapse = "','"), "');")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        L2Obs$SeaOWL <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # BBFL2
        qry <- paste0("SELECT * FROM bbfl2_l2 WHERE uuid_l2 IN ('", paste0(L2Select$Seluuid_l2(), collapse = "','"), "');")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        L2Obs$BBFL2 <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # Biosonic
        qry <- paste0("SELECT * FROM biosonic_l2 WHERE uuid_l2 IN ('", paste0(L2Select$Seluuid_l2(), collapse = "','"), "');")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        L2Obs$BioSonic <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)
      }
    )


    # uuid_l2 selection event trigger data display -------------------------------
    # by populating the Obs reactiveValues data table needed
    observeEvent(
      c(
        L1aSelect$Seluuid_l2()
      ),
      {
        # metadata_l2
        # Have to query data based on uuid_l2
        qry <- paste0("SELECT * FROM metadata_l2 WHERE uuid_l2='", L1aSelect$Seluuid_l2(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$metadata_l2 <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # metadata_l1b
        qry <- paste0("SELECT * FROM metadata_l1b WHERE uuid_l2='", L1aSelect$Seluuid_l2(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$metadata_l1b <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # HOCR
        # uuid_l2 have to ship with instrument and sn to be passed to HOCR$L2
        qry <- paste0("SELECT * FROM hocr_l1b WHERE uuid_l2='", L1aSelect$Seluuid_l2(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$HOCR$L1b <- tibble(DBI::dbFetch(res)) %>%
          group_by(id) %>%
          nest(cal_data = !matches("instrument|sn|uuid_l2"))
        DBI::dbClearResult(res)

        qry <- paste0("SELECT * FROM hocr_l2 WHERE uuid_l2='", L1aSelect$Seluuid_l2(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$HOCR$L2 <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # SBE19
        qry <- paste0("SELECT * FROM sbe19_l1b WHERE uuid_l2='", L1aSelect$Seluuid_l2(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$SBE19$L1b <- tibble(DBI::dbFetch(res)) %>%
          group_by(id) %>%
          nest(Data = !matches("parameter|uuid_l2"))
        DBI::dbClearResult(res)

        qry <- paste0("SELECT * FROM sbe19_l2 WHERE uuid_l2='", L1aSelect$Seluuid_l2(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$SBE19$L2 <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # SeaOWL
        qry <- paste0("SELECT * FROM seaowl_l1b WHERE uuid_l2='", L1aSelect$Seluuid_l2(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$SeaOWL$L1b <- tibble(DBI::dbFetch(res)) %>%
          group_by(id) %>%
          nest(Data = !matches("parameter|uuid_l2"))
        DBI::dbClearResult(res)

        qry <- paste0("SELECT * FROM seaowl_l2 WHERE uuid_l2='", L1aSelect$Seluuid_l2(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$SeaOWL$L2 <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # BBFL2
        qry <- paste0("SELECT * FROM bbfl2_l1b WHERE uuid_l2='", L1aSelect$Seluuid_l2(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$BBFL2$L1b <- tibble(DBI::dbFetch(res)) %>%
          group_by(id) %>%
          nest(Data = !matches("parameter|uuid_l2"))
        DBI::dbClearResult(res)

        qry <- paste0("SELECT * FROM bbfl2_l2 WHERE uuid_l2='", L1aSelect$Seluuid_l2(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$BBFL2$L2 <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # BioSonic
        qry <- paste0("SELECT * FROM biosonic_l1b WHERE uuid_l2='", L1aSelect$Seluuid_l2(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$BioSonic$L1b <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        qry <- paste0("SELECT * FROM biosonic_l2 WHERE uuid_l2='", L1aSelect$Seluuid_l2(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$BioSonic$L2 <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        # HydroBall
        qry <- paste0("SELECT * FROM hydroball_l1b WHERE uuid_l2='", L1aSelect$Seluuid_l2(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$HydroBall$L1b <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)

        qry <- paste0("SELECT * FROM hydroball_l2 WHERE uuid_l2='", L1aSelect$Seluuid_l2(), "';")
        res <- DBI::dbSendQuery(DB$Con(), qry)
        Obs$HydroBall$L2 <- tibble(DBI::dbFetch(res))
        DBI::dbClearResult(res)
      }
    )

    # Save to SQLite ----------------------------------------------------------
    uuid_l2 <- reactiveVal()

    observeEvent(
      req(input$Save),
      {
        # Sanitize tables ---------------------------------------------------------
        # Remove variables used for dev of Rb retrieval that don't belong in tables

        Obs$HOCR$L2 <- Obs$HOCR$L2 %>%
          select(matches("wavelength|Rrs|Rrs_loess|KLu|KLu_loess|RbI|uuid_l2"))

        # Does uuid_l2 is present in metadata_l2 colnames ?
        # Now it is initiated on start so always TRUE
        uuid_l2Present <- any(str_detect(names(Obs$metadata_l2), "uuid_l2"))

        # Does uuid_l2 exist in database, check ObsMeta
        if (uuid_l2Present) {
          uuid_l2Exist <- any(Obs$metadata_l2$uuid_l2 %in% DB$ObsMeta()$uuid_l2)
        } else {
          uuid_l2Exist <- F
        }

        # If uuid_l2 already exist, update record in SQLite
        if (uuid_l2Present & uuid_l2Exist) {
          # Update metadata_l2 ---------------------------------------------------------

          metadata_l2 <- Obs$metadata_l2 %>%
            mutate(
              date_time_processing = as.character(as.POSIXlt(Sys.time(), tz = "utc")),
              analyst = "Raphael Mabit",
              mail = "raphael.mabit@gmail.com"
            )

          qry <- glue::glue(
            "UPDATE metadata_l2
            SET comment = '", metadata_l2$comment, "',
                date_time_processing = '", metadata_l2$date_time_processing, "',
                analyst = '", metadata_l2$analyst, "',
                mail = '", metadata_l2$mail, "'
            WHERE uuid_l2 = '", Obs$metadata_l2$uuid_l2, "';"
          )

          # Execute the statement and return the number of line affected
          MetaUp <- DBI::dbExecute(DB$Con(), qry)


          # Update HOCR -------------------------------------------------------------

          hocr_l1b <- Obs$HOCR$L1b %>%
            unnest(cols = c(cal_data))

          # List L1bID the link between the instruments spectrum.
          SumQC <- hocr_l1b %>%
            group_by(uuid_l2, id, qc) %>%
            summarise()

          # Create specific query for each L1bID
          # As we only update qc, the pair (uuid_l2, id) is a primary key of the qc column
          # And now I realize that hocr_l1b does not complies with tidy data principles
          # a signle observational unit by table. Should a specific table be created for qc data ?

          qry <- purrr::pmap_chr(
            list(..1 = SumQC$uuid_l2, ..2 = SumQC$id, ..3 = SumQC$qc),
            .f = ~ glue::glue(
              "
              UPDATE hocr_l1b
              SET qc = '", ..3, "'
              WHERE (uuid_l2 || id = '", glue::glue(..1, ..2), "');
              "
            )
          )

          # At the moment RSQLite does not provide support for multiple statement
          # in one query: https://github.com/r-dbi/RSQLite/issues/313
          # Have to send each query separately ...
          # Maybe raise an issue to provide support for that ?

          # Result is that we issue query to update qc everywhere, event if it doesn't change ...

          # Execute the statement and return the number of line affected
          HOCRL1bUp <- unlist(purrr::map(qry, ~ DBI::dbExecute(DB$Con(), glue::glue_sql(.x))))

          # Update HOCR L2 table
          hocr_l2 <- Obs$HOCR$L2

          # Individual CASE WHEN for each variables to change: Rrs, KLu
          # As the WHERE constraint on uuid_l2 is already present on the final query could remove uuid_l2 from CASE WHEN
          qryRrs <- glue::glue_sql_collapse(purrr::pmap_chr(
            list(..1 = hocr_l2$uuid_l2, ..2 = hocr_l2$wavelength, ..3 = hocr_l2$Rrs),
            .f = ~ glue::glue(
              "WHEN uuid_l2 = '", ..1, "' AND wavelength = ", ..2, " THEN ", ..3
            )
          ), sep = "\n")

          qryRrs_loess <- glue::glue_sql_collapse(purrr::pmap_chr(
            list(..1 = hocr_l2$uuid_l2, ..2 = hocr_l2$wavelength, ..3 = hocr_l2$Rrs_loess),
            .f = ~ glue::glue(
              "WHEN uuid_l2 = '", ..1, "' AND wavelength = ", ..2, " THEN ", ..3
            )
          ), sep = "\n")

          qryKLu <- glue::glue_sql_collapse(purrr::pmap_chr(
            list(..1 = hocr_l2$uuid_l2, ..2 = hocr_l2$wavelength, ..3 = hocr_l2$KLu),
            .f = ~ glue::glue(
              "WHEN uuid_l2 = '", ..1, "' AND wavelength = ", ..2, " THEN ", ..3
            )
          ), sep = "\n")

          qryKLu_loess <- glue::glue_sql_collapse(purrr::pmap_chr(
            list(..1 = hocr_l2$uuid_l2, ..2 = hocr_l2$wavelength, ..3 = hocr_l2$KLu_loess),
            .f = ~ glue::glue(
              "WHEN uuid_l2 = '", ..1, "' AND wavelength = ", ..2, " THEN ", ..3
            )
          ), sep = "\n")

          qryRbI <- glue::glue_sql_collapse(purrr::pmap_chr(
            list(..1 = hocr_l2$uuid_l2, ..2 = hocr_l2$wavelength, ..3 = hocr_l2$RbI),
            .f = ~ glue::glue(
              "WHEN uuid_l2 = '", ..1, "' AND wavelength = ", ..2, " THEN ", ..3
            )
          ), sep = "\n")

          # Assemble query
          qry <- glue::glue_sql(
            "UPDATE hocr_l2
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
            WHERE uuid_l2 = '", Obs$metadata_l2$uuid_l2, "';"
          )

          # NA value in R are equal to NULL in SQL
          qry <- glue::glue_sql(stringr::str_replace_all(qry, "NA", "NULL"))

          # Execute the statement and return the number of line affected
          HOCRL2Up <- DBI::dbExecute(DB$Con(), qry)


          # Update SBE19 ------------------------------------------------------------

          SBE19L1bUp <- update_L1b_param_val(L1b = Obs$SBE19$L1b, TableName = "sbe19_l1b", Con = DB$Con())

          SBE19L2Up <- update_L2_param_val(L2 = Obs$SBE19$L2, TableName = "sbe19_l2", Con = DB$Con())

          # Update SeaOWL ------------------------------------------------------------

          SeaOWLL1bUp <- update_L1b_param_val(L1b = Obs$SeaOWL$L1b, TableName = "seaowl_l1b", Con = DB$Con())

          SeaOWLL2Up <- update_L2_param_val(L2 = Obs$SeaOWL$L2, TableName = "seaowl_l2", Con = DB$Con())


          # Update BBFL2 ------------------------------------------------------------

          BBFL2L1bUp <- update_L1b_param_val(L1b = Obs$BBFL2$L1b, TableName = "bbfl2_l1b", Con = DB$Con())

          BBFL2L2Up <- update_L2_param_val(L2 = Obs$BBFL2$L2, TableName = "bbfl2_l2", Con = DB$Con())


          # Update BioSonic ---------------------------------------------------------

          # there is no BioSonic data manipulation for now so no need to update


          # Update feedback UI ------------------------------------------------------

          # Check that the number of line affected is correct, should probably improve this
          # MetaUp must be only one line affected, if more means uuid_l2 collision
          # L1bUp == 3 * 137 wavelengths * bins number
          # L2Up == User input wavelength
          test <- all(MetaUp == 1) # , unique(HOCRL1bUp) == 411, HOCRL2Up == 150)

          # Feedback to the user
          session$sendCustomMessage(
            type = "testmessage",
            message =
              glue::glue(
                "metadata_l2: ", MetaUp, " entry updated\n",
                "hocr_l1b: ", sum(HOCRL1bUp), " entry updated\n",
                "hocr_l2: ", HOCRL2Up, " entry updated\n"
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
          # Initial save if uuid_l2 doesn't exist --------------------------------------

          Obsuuid_l2 <- uuid::UUIDgenerate(
            use.time = T,
            output = "string"
          )

          # Good explanation of the difference between uuid_l2 and hash,
          # This way could create collision as it does not guarantee uniqueness
          # openssl::md5(paste0(as.character(ObsMeta),
          #                     as.character(hocr_l1b),
          #                     as.character(AOPs),
          #                     collapse = ""))

          metadata_l2 <- Obs$metadata_l2 %>%
            mutate(
              uuid_l2 = Obsuuid_l2,
              date_time_processing = as.character(as.POSIXlt(Sys.time(), tz = "utc")),
              analyst = "Raphael Mabit",
              mail = "raphael.mabit@gmail.com"
            )

          metadata_l1b <- Obs$metadata_l1b %>%
            mutate(
              uuid_l2 = Obsuuid_l2
            )

          DBI::dbWriteTable(DB$Con(), "metadata_l2", metadata_l2, append = TRUE)
          DBI::dbWriteTable(DB$Con(), "metadata_l1b", metadata_l1b, append = TRUE)

          if (nrow(Obs$HOCR$L1b) != 0) {
            hocr_l1b <- Obs$HOCR$L1b %>%
              unnest(cols = c(cal_data)) %>%
              mutate(
                uuid_l2 = Obsuuid_l2
              )

            hocr_l2 <- Obs$HOCR$L2 %>%
              mutate(uuid_l2 = Obsuuid_l2)

            DBI::dbWriteTable(DB$Con(), "hocr_l1b", hocr_l1b, append = TRUE)
            DBI::dbWriteTable(DB$Con(), "hocr_l2", hocr_l2, append = TRUE)
          }

          if (nrow(Obs$SBE19$L1b) != 0) {
            sbe19_l1b <- Obs$SBE19$L1b %>%
              unnest(c(Data)) %>%
              mutate(
                uuid_l2 = Obsuuid_l2
              )

            sbe19_l2 <- Obs$SBE19$L2 %>%
              mutate(uuid_l2 = Obsuuid_l2)

            DBI::dbWriteTable(DB$Con(), "sbe19_l1b", sbe19_l1b, append = TRUE)
            DBI::dbWriteTable(DB$Con(), "sbe19_l2", sbe19_l2, append = TRUE)
          }


          if (nrow(Obs$SeaOWL$L1b) != 0) {
            seaowl_l1b <- Obs$SeaOWL$L1b %>%
              unnest(c(Data)) %>%
              mutate(
                uuid_l2 = Obsuuid_l2
              )

            seaowl_l2 <- Obs$SeaOWL$L2 %>%
              mutate(uuid_l2 = Obsuuid_l2)

            DBI::dbWriteTable(DB$Con(), "seaowl_l1b", seaowl_l1b, append = TRUE)
            DBI::dbWriteTable(DB$Con(), "seaowl_l2", seaowl_l2, append = TRUE)
          }

          if (nrow(Obs$BBFL2$L1b) != 0) {
            bbfl2_l1b <- Obs$BBFL2$L1b %>%
              unnest(c(Data)) %>%
              mutate(
                uuid_l2 = Obsuuid_l2
              )

            bbfl2_l2 <- Obs$BBFL2$L2 %>%
              mutate(uuid_l2 = Obsuuid_l2)

            DBI::dbWriteTable(DB$Con(), "bbfl2_l1b", bbfl2_l1b, append = TRUE)
            DBI::dbWriteTable(DB$Con(), "bbfl2_l2", bbfl2_l2, append = TRUE)
          }

          if (nrow(Obs$BioSonic$L1b) != 0) {
            biosonic_l1b <- Obs$BioSonic$L1b %>%
              mutate(
                uuid_l2 = Obsuuid_l2
              )

            biosonic_l2 <- Obs$BioSonic$L2 %>%
              mutate(uuid_l2 = Obsuuid_l2)

            DBI::dbWriteTable(DB$Con(), "biosonic_l1b", biosonic_l1b, append = TRUE)
            DBI::dbWriteTable(DB$Con(), "biosonic_l2", biosonic_l2, append = TRUE)
          }

          if (nrow(Obs$HydroBall$L1b) != 0) {
            hydroball_l1b <- Obs$HydroBall$L1b %>%
              mutate(
                uuid_l2 = Obsuuid_l2
              )

            hydroball_l2 <- Obs$HydroBall$L2 %>%
              mutate(uuid_l2 = Obsuuid_l2)

            DBI::dbWriteTable(DB$Con(), "hydroball_l1b", hydroball_l1b, append = TRUE)
            DBI::dbWriteTable(DB$Con(), "hydroball_l2", hydroball_l2, append = TRUE)
          }

          # Feedback to the user
          session$sendCustomMessage(
            type = "testmessage",
            message = "Saved"
            # glue::glue(
            #   "metadata_l2: ",MetaUp," entry updated\n",
            #   "hocr_l1b: ", sum(L1bUp)," entry updated\n",
            #   "hocr_l2: ",L2Up," entry updated\n")
          )

          # Update the list of observation
          DB$ObsMeta(tibble(DBI::dbGetQuery(DB$Con(), "SELECT * FROM metadata_l2")))

          qry <- glue::glue_sql("SELECT * FROM metadata_l2 WHERE uuid_l2 = '", Obsuuid_l2, "';")

          Obs$metadata_l2 <- tibble(DBI::dbGetQuery(DB$Con(), qry))
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

      qry <- glue::glue("DELETE FROM metadata_l2 WHERE uuid_l2='", Obs$metadata_l2$uuid_l2, "';")

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
      DB$ObsMeta(tibble(DBI::dbGetQuery(DB$Con(), "SELECT * FROM metadata_l2")))
    })

    # If user cancel
    observeEvent(input$cancel, {
      removeModal()
    })

    # Module export -----------------------------------------------------------
    list(
      Save = reactive(input$Save),
      uuid_l2 = uuid_l2
    )
  })
}




## To be copied in the UI
# mod_manage_obs_ui("manage_obs")

## To be copied in the server
# mod_manage_obs_server("manage_obs")
