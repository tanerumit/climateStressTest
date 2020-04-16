
################################################################################

# fileUpload Shiny Module ----

# UI function
fileUpload_mod_UI <- function(id, label = "Upload data (csv or xlsx)") {

  # Create a namespace function using the provided id
  ns <- NS(id)

  fileInput(ns("file"), label)

  # tagList(
  #   fileInput(ns("file"), label),
  #   uiOutput(ns("subset_check")),
  #   uiOutput(ns("subset_var")),
  #   uiOutput(ns("subset_val"))
  # )

}

# Server function
fileUpload_mod_Server <- function(id) {

  moduleServer(id, function(input, output, session, nfactor.limit = 20) {

    ## To remove NOTE.
    val_label <- BinaryGroupRandom <- variable <- NULL

    # The selected file, if any
    userFile <- eventReactive(input$file, {
      # If no file is selected, don't do anything
      #validate(need(input$file, message = FALSE))
      input$file
    })

    # Read-in data in the selected file based on file type
    data <- eventReactive(input$file, {

      # Validate file formats uploaded
      validate(need((grepl("csv", userFile()$name) == T) | (grepl("xlsx", userFile()$name) == T), message = "Please upload csv or xlsx file"))

      #If it is a csv file
      if (grepl("csv", userFile()$name) == T){
        out <- data.table::fread(userFile()$datapath, check.names = F, integer64 = "double")

      #If it is a xlsx file
      } else if (grepl("xlsx", userFile()$name) == T){
        out <- data.table::data.table(readxl::read_excel(userFile()$datapath), check.names = F, integer64 = "double")

      #Else not supported
      } else {
        stop("Not supported format.")
      }

      # Exclude empty columns in th data
      naCol <- names(out)[unlist(out[, lapply(.SD, function(x){all(is.na(x))})])]
      if (length(naCol) ==0){
        naomit = NULL
      } else{
        out <- out[, .SD, .SDcols = -naCol]
        naomit = paste("Column <B>", paste(naCol, collapse = ", "), "</B> are(is) excluded because it is empty.", sep = "")
      }
      out.old <- out
      name.old <- names(out.old)
      out <- data.table::data.table(out, check.names = T)
      name.new <- names(out)
      ref <- list(name.old = name.old, name.new = name.new)
      numstart.vnum <- suppressWarnings(sapply(names(out),function(x){!is.na(as.numeric(substr(x, 1,1)))}))
      names(out)[numstart.vnum] <- paste("n_", names(out)[numstart.vnum], sep = "")
      factor_vars <- names(out)[out[, lapply(.SD, class) %in% c("factor", "character")]]
      if (length(factor_vars) > 0){
        out[, (factor_vars) := lapply(.SD, as.factor), .SDcols= factor_vars]
      }

      return(list(data = out,
                  factor_original = factor_vars,
                  ref = ref,
                  naomit = naomit))
  })

    #Subset data
    observeEvent(c(data()$factor_original, input$factor_vname), {

      output$subset_check <- renderUI({
        checkboxInput(session$ns("check_subset"), "Subset data")
      })
  })

    observeEvent(input$check_subset, {
      output$subset_var <- renderUI({
        req(input$check_subset == T)
        factor_subset <- c(data()$factor_original, input$factor_vname)

        validate(
          need(length(factor_subset) > 0 , "No factor variable for subsetting")
        )

        tagList(
          selectInput(session$ns("var_subset"), "Subset variables",
                      choices = names(data()$data), multiple = T,
                      selected = names(data()$data)[1])
        )
      })

      output$subset_val <- renderUI({
        req(input$check_subset == T)
        req(length(input$var_subset) > 0)
        var.factor <- c(data()$factor_original, input$factor_vname)

        outUI <- tagList()

        for (v in seq_along(input$var_subset)){
          if (input$var_subset[[v]] %in% var.factor){
            varlevel <- levels(as.factor(data()$data[[input$var_subset[[v]]]]))
            outUI[[v]] <- selectInput(session$ns(paste0("val_subset", v)), paste0("Subset value: ", input$var_subset[[v]]),
                                      choices = varlevel, multiple = T,
                                      selected = varlevel[1])
          } else{
            val <- stats::quantile(data()$data[[input$var_subset[[v]]]], na.rm = T)
            outUI[[v]] <- sliderInput(session$ns(paste0("val_subset", v)), paste0("Subset range: ", input$var_subset[[v]]),
                                      min = val[1], max = val[5],
                                      value = c(val[2], val[4]))
          }

        }
        outUI

      })

  })

    # We can run observers in here if we want to
    observe({
      msg <- sprintf("File %s was uploaded", userFile()$name)
      cat(msg, "\n")
  })

    # Final list of outputs
  #   outdata <- reactive({
  #
  #     out <- data()$data
  #
  #     #out[, (data()$conti_original) := lapply(.SD, function(x){as.numeric(as.vector(x))}), .SDcols = data()$conti_original]
  #
  #     if (length(input$factor_vname) > 0){
  #       out[, (input$factor_vname) := lapply(.SD, as.factor), .SDcols= input$factor_vname]
  #     }
  #
  #     ref <- data()$ref
  #     out.label <- mk.lev(out)
  #
  #     if (!is.null(input$check_subset)){
  #       if (input$check_subset){
  #         validate(
  #           need(length(input$var_subset) > 0 , "No variables for subsetting"),
  #           need(all(sapply(1:length(input$var_subset), function(x){length(input[[paste0("val_subset", x)]])})), "No value for subsetting")
  #         )
  #         var.factor <- c(data()$factor_original, input$factor_vname)
  #         #var.conti <- setdiff(data()$conti_original, input$factor_vname)
  #
  #         for (v in seq_along(input$var_subset)){
  #           if (input$var_subset[[v]] %in% var.factor){
  #             out <- out[get(input$var_subset[[v]]) %in% input[[paste0("val_subset", v)]]]
  #             #var.factor <- c(data()$factor_original, input$factor_vname)
  #             out[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
  #             out.label2 <- mk.lev(out)[, c("variable", "class", "level")]
  #             data.table::setkey(out.label, "variable", "class", "level")
  #             data.table::setkey(out.label2, "variable", "class", "level")
  #             out.label <- out.label[out.label2]
  #           } else{
  #             out <- out[get(input$var_subset[[v]]) >= input[[paste0("val_subset", v)]][1] & get(input$var_subset[[v]]) <= input[[paste0("val_subset", v)]][2]]
  #             #var.factor <- c(data()$factor_original, input$factor_vname)
  #             out[, (var.factor) := lapply(.SD, factor), .SDcols = var.factor]
  #             out.label2 <- mk.lev(out)[, c("variable", "class", "level")]
  #             data.table::setkey(out.label, "variable", "class", "level")
  #             data.table::setkey(out.label2, "variable", "class", "level")
  #             out.label <- out.label[out.label2]
  #           }
  #         }
  #
  #       }
  #     }
  #
  #     for (vn in ref[["name.new"]]){
  #       w <- which(ref[["name.new"]] == vn)
  #       out.label[variable == vn, var_label := ref[["name.old"]][w]]
  #     }
  #
  #     return(list(data = out, label = out.label, naomit = data()$naomit))
  # })
    outdata <- reactive({data()$data})

    return(outdata)
  })

}

################################################################################

# # Example ----
# if (interactive()) {
#
#   ui <- fluidPage(
#
#     fileUploadUI("datafile")
#   )
#
#   server <- function(input, output, session) {
#
#     data <- fileUploadServer("datafile")
#
#   }
#
#   shinyApp(ui, server)
# }
