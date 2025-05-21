
csv_import_server = function(state, id) {
    ns = NS(id)

    moduleServer(id, function(input, output, session) {
        CSV = reactiveValues(files = data.table(Name = c(""), Path = c("")))
        col.defs = data.frame(title=c("Name", "Path"), type = c("text", "text"), width = c(1000, 2000))

        #output$status = renderText("Select one or more files.")

        observe({
            if (input$time != "timestamp") {
                shinyjs::disable("timestamp_format")
            } else {
                shinyjs::enable("timestamp_format")
            }
        })

        output$files = renderExcel({
            excelTable(data.frame(Name = c(""), Path = c("")), columnSorting = F, columnDrag = F, allowDeleteColumn = F, allowRenameColumn = F, allowComments = F, columns = col.defs)
        })

        scan_files = function(file.table) {
            file.table = file.table[Name != "" | Path != "",]

            if (is.null(file.table) | nrow(file.table) == 0) {
                #output$status = renderText("\u26a0 No files selected.")
            } else {
                #output$status = renderText(sprintf("\u26a0 %d file(s) selected.", sum(file.exists(file.table$Path))))

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


                updateVirtualSelect("timestamp_variable", choices = vars, selected = input$timestamp_variable)
                updateVirtualSelect("id", choices = vars, selected = input$id)
                updateVirtualSelect("group", choices = vars, selected = input$group)

                if (length(vars) == 0) {
                    showNotification("No common variable names among selected files.", type = "error")
                    return(F)
                }
                return(T)
            }
            return (T)
        }

        observeEvent(input$new_file, {
            f = rstudioapi::selectFile(filter = "CSV Files (*.csv)")

            if (!is.null(f)) {
                new.name = strsplit(basename(f), ".", T)[[1]][1]

                new.entry = data.table(Name = new.name, Path = f)
                if (nrow(CSV$files==1)&CSV$files[1,Name]==""&CSV$files[1,Path]=="") CSV$files = new.entry
                else CSV$files = rbindlist(list(CSV$files, new.entry))

                output$files = renderExcel({
                    excelTable(CSV$files, columnSorting = F, columnDrag = F, allowDeleteColumn = F, allowRenameColumn = F, allowComments = F, columns = col.defs)
                })
            }
        })

        observeEvent(input$new_folder, {
            f = rstudioapi::selectDirectory()

            if (!is.null(f)) {
                csv.files = dir(f, pattern = "^.*\\.[Cc][Ss][Vv]$", full.names = T)
                for (ff in csv.files) {
                    new.name = strsplit(basename(ff), ".", T)[[1]][1]

                    new.entry = data.table(Name = new.name, Path = ff)
                    if (nrow(CSV$files==1)&CSV$files[1,Name]==""&CSV$files[1,Path]=="") CSV$files = new.entry
                    else CSV$files = rbindlist(list(CSV$files, new.entry))

                    output$files = renderExcel({
                        excelTable(CSV$files, columnSorting = F, columnDrag = F, allowDeleteColumn = F, allowRenameColumn = F, allowComments = F, columns = col.defs)
                    })
                }
            }
        })

        observeEvent(input$files, {
            CSV$files = as.data.table(excel_to_R(input$files))
        }, ignoreInit = T)

        observeEvent(input$clear, {
            CSV$files = data.table(Name = c(""), Path = c(""))
            output$files = renderExcel({
                excelTable(CSV$files, editable = T, columnSorting = F, columnDrag = F, allowDeleteColumn = F, allowRenameColumn = F, allowComments = F, columns = col.defs)
            })
        })


        observeEvent(c(CSV$files, input$header_row, input$data_row, input$na_values, input$column_types, input$time, input$timestamp_variable, input$timestamp_format, input$id, input$group), {
            file.table = CSV$files
            req(nrow(CSV$files) > 0)

            .ok = scan_files(file.table)

            if (!.ok) {
                return()
            }

            state$data = list()

            for (f in 1:nrow(file.table)) {
                file.name = file.table[f, Name]
                file.loc = file.table[f, Path]

                if (file.name == "" | file.loc == "") next

                if (file.exists(file.loc)) {
                    file.header = as.character(t(readr::read_csv(file.loc, n_max = 1, skip = input$header_row - 1, col_names = F, show_col_types = F, col_types = "c"))[,1])
                    na.values =  strsplit(input$na_values, ",", fixed=T)[[1]]


                    tryCatch({
                        if (input$timestamp_variable == "") {
                            stop(sprintf("Time variable has not been selected."))
                        }

                        state$data[[file.name]] = as.data.table(readr::read_csv(file.loc, col_names = file.header, na = na.values, skip = input$data_row - 1, progress = T, show_col_types = F, col_types = input$column_types))

                        if (input$time == "auto") {
                            state$data[[file.name]][, (input$timestamp_variable) := as.POSIXct(get(input$timestamp_variable))]

                        } else if (input$time == "timestamp") {

                            if (any(class(state$data[[file.name]][,get(input$timestamp_variable)]) != "character")) {
                                stop(sprintf("Timestamp variable type needs to be character, was %s.", paste0(class(state$data[[file.name]][,get(input$timestamp_variable)]), collapse = ", ")))
                            }
                            state$data[[file.name]][, (input$timestamp_variable) := as.POSIXct(lubridate::parse_date_time(get(input$timestamp_variable), quiet = T, orders = input$timestamp_format))]

                            if (anyNA(state$data[[file.name]][, get(input$timestamp_variable)])) {
                                warning(sprintf("Missing values in time variable '%s'.", input$timestamp_variable))
                            }

                        } else if (input$time == "unix") {

                            if (any(class(state$data[[file.name]][,get(input$timestamp_variable)]) != "numeric")) {
                                stop(sprintf("Timestamp variable type needs to be numeric, was %s.", paste0(class(state$data[[file.name]][,get(input$timestamp_variable)]), collapse = ", ")))
                            }
                            state$data[[file.name]][, (input$timestamp_variable) := as.POSIXct(lubridate::as_datetime(get(input$timestamp_variable)))]

                        }

                        state$data[[file.name]][, `Dataset` := file.name]

                        state$add_dataset(dt = state$data[[file.name]], name = file.name, .time = input$timestamp_variable, .id = input$id, .group = input$group)

                        showNotification(sprintf("%s imported successfully!", file.name))

                    }, error = \(e) {
                        showNotification(HTML(sprintf("<b>%s</b>:<br/>%s", file.name, toString(e))), type = "error")
                    }, warning = \(w) {
                        showNotification(HTML(sprintf("<b>%s</b>:<br/>%s", file.name, toString(w))), type = "warning")
                    })
                } else {
                    if (file.name != "") {
                        showNotification(HTML(sprintf("<b>%s</b> does not exist", file.name)), type = "error")
                    }
                }
            }

            gargoyle::trigger("data_update")
        })
    })
}

csv_import_ui = function(id) {
    ns = NS(id)

    layout_columns(col_widths = c(12),
        div(
            tags$div("Options", class = "hrtext"),
            layout_columns(col_widths = c(2, 2, 2, 3, 3),
                numericInput(ns("header_row"), label = tooltip("Header Row", "The row number which defines column names."), value = 1, min = 0, step = 1),
                numericInput(ns("data_row"), label = tooltip("Data Row", "The first row of data."), value = 2, min = 1, step = 1),
                virtualSelectInput(ns("time"), label = "Time Format", choices = c("auto"="auto", "timestamp"="timestamp", "unix"="unix"), selected = "auto", optionHeight = "24px"),
                virtualSelectInput(ns("timestamp_variable"), label = "Time Variable", choices = c(), multiple = F, selected = NULL, optionsCount = 10, search = T, optionHeight = "24px"),
                textInput(ns("timestamp_format"), label = tooltip("Timestamp Format", HTML("A string representing the format of the selected time variable. See R's base::strptime() for possible values."), ), value = "mdY HM")
            ),
            div(
                layout_columns(col_widths = c(3, 3, -2, 2, 2),
                    virtualSelectInput(ns("id"), label = tooltip("ID Variable", "Use a unique identifier to indicate subject-level stratification of datasets."), choices = c(), multiple = T, selected = NULL, optionsCount = 10, search = T, optionHeight = "24px", maxValues = 1),
                    virtualSelectInput(ns("group"), label = tooltip("Group Variable(s)", "Optionally group datasets or subjects according to the string factors found in these variables."), choices = c(), multiple = T, selected = NULL, optionsCount = 10, search = T, optionHeight = "24px"),
                    textInput(ns("column_types"), label = tooltip("Column Type(s)", "Use either a single character, or a sequence to define column types: c = character, i = integer, n = number, d = double, l = logical, f = factor, D = date, T = date time, t = time, ? = guess, _ or - = skip."), value = "?", width = "100%"),
                    textInput(ns("na_values"), label = tooltip("Missing Values", "Specify a comma-separated list of strings to indicate missing values."), value = "-,.,NA,NaN", width = "100%")
                )
            ),
            tags$div("Files", class = "hrtext"),
            div(
                div(class = "d-flex",
                    dropMenu(arrow = F, padding = 0, placement = "bottom-start", theme = "light-border", tag = actionLink(ns("new"), label = bs_icon("plus-lg", size = "1.0rem"), class = "card-menu-item"),
                        actionLink(ns("new_file"), "Add a file..."), tags$br(),
                        actionLink(ns("new_folder"), "Add a folder...")
                    ),
                    actionLink(ns("clear"), label = bs_icon("trash", size = "1.0rem"), class = "text-danger bg-white card-menu-item"),
                ),
                excelOutput(ns("files"), height = "100%")
            )
        ),
    )
}
