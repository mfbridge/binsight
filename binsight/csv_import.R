
csv_import_server = function(state, id) {
    ns = NS(id)

    moduleServer(id, function(input, output, session) {
        CSV = reactiveValues(files = data.table(Name = c(""), Path = c("")))

        output$status = renderText("Select one or more files.")

        output$files = renderExcel({
            excelTable(data.frame(Name = c(""), Path = c("")))
        })

        scan_files = function(file.table) {
            file.table = file.table[Name != "" | Path != "",]

            if (is.null(file.table) | nrow(file.table) == 0) {
                output$status = renderText("\u26a0 No files selected.")
            } else {
                output$status = renderText(sprintf("\u26a0 %d file(s) selected.", sum(file.exists(file.table$Path))))

                var.list = list()

                # scan headers
                for (f in 1:nrow(file.table)) {
                    file.path = file.table[f, Path]

                    if (file.exists(file.path)) {
                        file.header = as.character(t(readr::read_csv(file.path, n_max = 1, skip = input$header_row - 1, col_names = F, show_col_types = F, col_types = "c"))[,1])
                        var.list[[f]] = file.header
                    }
                }

                # list of common variable names
                vars = as.character(Reduce(intersect, var.list))

                if (length(vars) == 0) {
                    showNotification("No common variable names among selected files.", type = "error")
                }

                updatePickerInput(session, "timestamp_variable", choices = vars, selected = input$timestamp_variable)
            }
        }

        observeEvent(input$new_file, {
            f = rstudioapi::selectFile(filter = "CSV Files (*.csv)")

            if (!is.null(f)) {
                new.name = strsplit(basename(f), ".", T)[[1]][1]

                new.entry = data.table(Name = new.name, Path = f)
                if (nrow(CSV$files==1)&CSV$files[1,Name]==""&CSV$files[1,Path]=="") CSV$files = new.entry
                else CSV$files = rbindlist(list(CSV$files, new.entry))

                output$files = renderExcel({
                    excelTable(CSV$files, editable = T, columnSorting = F, columnDrag = F, allowDeleteColumn = F, allowRenameColumn = F, allowComments = F,
                        columns = data.table(title = c("Name", "Path")))
                })
            }
        })

        observeEvent(c(CSV$files, input$header_row, input$data_row, input$na_values, input$column_types, input$time, input$timestamp_variable, input$timestamp_format), {
            file.table = CSV$files
            req(nrow(CSV$files) > 0)

            scan_files(file.table)

            state$data = list()

            for (f in 1:nrow(file.table)) {
                file.name = file.table[f, Name]
                file.loc = file.table[f, Path]

                if (file.name == "" | file.loc == "") next

                if (file.exists(file.loc)) {
                    file.header = as.character(t(readr::read_csv(file.loc, n_max = 1, skip = input$header_row - 1, col_names = F, show_col_types = F, col_types = "c"))[,1])
                    na.values =  strsplit(input$na_values, ",", fixed=T)[[1]]

                    tryCatch({

                        state$data[[file.name]] = as.data.table(readr::read_csv(file.loc, col_names = file.header, na = na.values, skip = input$data_row - 1, progress = T, show_col_types = F, col_types = input$column_types))

                        if (input$time == "auto") {
                            state$data[[file.name]][, (input$timestamp_variable) := as.POSIXct(get(input$timestamp_variable))]

                        } else if (input$time == "timestamp") {
                            state$data[[file.name]][, (input$timestamp_variable) := as.POSIXct(lubridate::parse_date_time(get(input$timestamp_variable), orders = input$timestamp_format))]

                        } else if (input$time == "unix") {

                        }

                        showNotification(sprintf("%s imported successfully!", file.name))

                    }, error = \(e) {
                        showNotification(sprintf("%s while trying to import %s", toString(e), file.name), type = "error")
                    })
                } else {
                    if (file.name != "") {
                        showNotification(sprintf("%s does not exist", file.name), type = "error")
                    }
                }
            }

            gargoyle::trigger("data_update")
        })
    })
}

csv_import_ui = function(id) {
    ns = NS(id)

    card(fill = F, style = "font-size: 0.8rem;", id = ns(id),
        card_header("Comma-separated values (*.csv)", class = "d-flex justify-content-between",
            div(class = "d-flex justify-content-between",
                dropMenu(arrow = F, padding = 0, placement = "bottom-start", theme = "light-border", tag = actionLink(ns("new"), label = bs_icon("plus-lg"), class = "card-menu-item"),
                    actionLink(ns("new_file"), "Add a file..."), tags$br(),
                    actionLink(ns("new_folder"), "Add a folder...")
                ),
                actionLink(ns("clear"), label = bs_icon("trash"), class = "text-danger card-menu-item")
            )
        ),
        layout_columns(col_widths = c(12),
            div(
                layout_columns(col_widths = c(6, 6),
                    numericInput(ns("header_row"), label = "Header Row #", value = 1, min = 0, step = 1),
                    numericInput(ns("data_row"), label = "First Data Row #", value = 2, min = 1, step = 1)
                ),
                div(
                    layout_columns(col_widths = c(4, 4, 2, 2),
                        pickerInput(ns("time"), label = "Time Format", choices = c("auto"="auto", "timestamp"="timestamp", "unix timestamp"="unix"), selected = "auto"),
                        div(
                            div(id = ns("timestamp"),
                                layout_columns(col_widths = c(6, 6),
                                    pickerInput(ns("timestamp_variable"), label = "Variable", choices = c(), options = pickerOptions(size = 5)),
                                    textInput(ns("timestamp_format"), label = "Format", value = "mdY HM")
                                )
                            )
                        ),
                        textInput(ns("column_types"), label = "Column type", value = "?", width = "100%"),
                        textInput(ns("na_values"), label = "Missing", value = "-,.,NA,NaN", width = "100%")
                    )
                ),
                div(
                    tags$b("Files"),
                    excelOutput(ns("files"), height = "100%")
                )
            ),

        ),
        card_footer(
            uiOutput(ns("status"), inline = T)
        )
    )
}
