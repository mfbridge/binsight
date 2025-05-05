
binning_server = function(state, id) {
    ns = NS(id)

    moduleServer(id, function(input, output, session) {
        observe({
            if (input$start == "first") {
                shinyjs::hide(ns("time_string"), asis = T)
                shinyjs::hide(ns("time_unix"), asis = T)

            } else if (input$start == "timestamp") {
                shinyjs::show(ns("time_string"), asis = T)
                shinyjs::hide(ns("time_unix"), asis = T)

            } else if (input$start == "unix") {
                shinyjs::hide(ns("time_string"), asis = T)
                shinyjs::show(ns("time_unix"), asis = T)
            }
        })

        observe({

        })

        gargoyle::on("temp_update", {
            varlist = list()
            for (dt in names(state$temp)) {
                varlist[[dt]] = colnames(state$temp[[dt]])
            }
            varlist = as.character(Reduce(intersect, varlist))
            updatePickerInput(session, "vars", choices = varlist)
        })

        observeEvent(input$test, {
            state$final = list()
            final = list()

            binsize = input$bin_size
            timevar = "DateTime_30"
            varlist = c(timevar, input$vars, input$groups)

            if (length(state$temp) == 0) {
                for (dt in names(state$data)) {
                    state$temp[[dt]] = copy(state$data[[dt]])
                }
            }

            for (dt in names(state$temp)) {
                final[[dt]] = copy(state$temp[[dt]])

                origin.time = 0

                #browser()

                if (input$start == "first") {
                    origin.time = final[[dt]][, min(get(timevar))]
                }

                final[[dt]][, Bin := floor((get(timevar) - origin.time) / binsize)]

                ft = data.table(Bin = final[[dt]][, unique(Bin)])

                if (input$fun == "mean") {
                    for (c in input$vars) {
                        if (!any(class(final[[dt]][, get(..c)]) %in% c("integer", "numeric"))) next

                        tc = final[[dt]][, c("Bin", c), with = F]

                        tf = tc[, .(value = mean(get(..c))), by = Bin, env = list(value = c)]


                        ft = merge(ft, tf, by = "Bin")
                    }
                }

                final[[dt]] = ft
            }

            state$final = final

            gargoyle::trigger("final_update")

        })

        observeEvent(input$preview_dataset, {
            output$preview_table = DT::renderDT({
                DT::datatable(data = state$final[[input$preview_dataset]])
            })
        })
    })
}

binning_ui = function(id) {
    ns = NS(id)

    card(fill = F, style = "font-size: 0.8rem; height: 20rem;", id = id,
        card_header("Aggregation/binning", class = "d-flex justify-content-between",
            div(class = "d-flex justify-content-between",
                actionLink(ns("test"), label = bs_icon("play-fill"), class = "text-success card-menu-item")
            )
        ),

        card_body(
            layout_columns(col_widths = c(4, 8),
                pickerInput(ns("start"), "Starting Point (Bin 0)",
                    choices = c("first observation"="first"), selected = "first"),
                div(
                    textInput(ns("time_string"), "Timestamp", value = Sys.time()),
                    numericInput(ns("time_unix"), "Unix Timestamp", value = as.numeric(Sys.time()))
                )
            ),
            layout_columns(col_widths = c(3, 3, 3, 3),
                numericInput(ns("bin_size"), "Bin Size (seconds)", value = 60*15, min = 0, step = 1),
                pickerInput(ns("vars"), "Variables", choices = c(""), selected = "", multiple = T),
                pickerInput(ns("groups"), "Group Variables", choices = c(""), selected = ""),
                pickerInput(ns("fun"), "Aggregation Function", choices = c("count", "mean", "median", "min", "max", "custom"), selected = "mean")
            )
        ),

        card_footer(
            uiOutput(ns("status"), inline = T)
        )
    )
}
