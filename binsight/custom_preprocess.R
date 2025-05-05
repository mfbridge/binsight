
custom.example.string = "step2 = function(name, dt) {
    # ex: replace missing values with zeroes
    for (c in colnames(dt)) {
        dt[is.na(get(c)), (c) := 0]
    }
}

for (i in names(data)) {
    step2(i, data[[i]])
}
"

custom_preprocess_server = function(state, id) {
    ns = NS(id)
    moduleServer(id, function(input, output, session) {

        observeEvent(input$test, {
            temp = list()

            for (dt in names(state$data)) {
                temp[[dt]] = copy(state$data[[dt]])
            }

            tryCatch({
                start.t = proc.time()

                .env = env(data = temp)
                .exp = parse(text = input$code)
                .out = capture.output(eval(.exp, envir = .env))

                end.t = proc.time()

                state$temp = list()

                for (dt in names(temp)) {
                    state$temp[[dt]] = temp[[dt]]
                }

                showNotification(sprintf("Successfully executed in %0.0f ms", (end.t[3] - start.t[3]) * 1000))

                gargoyle::trigger("temp_update")

            }, error = \(e) {
                showNotification(toString(e), type = "error")
            }, warning = \(e) {
                showNotification(toString(e), type = "warning")
            })
        })
    })
}

custom_preprocess_ui = function(id) {
    ns = NS(id)

    card(fill = F, style = "font-size: 0.8rem;", id = id,
        card_header("Custom script", class = "d-flex justify-content-between",
            div(class = "d-flex justify-content-between",
                dropMenu(arrow = F, padding = 0, placement = "bottom-start", theme = "light-border",
                    tag = actionLink(ns("new"), label = bs_icon("folder2-open"), class = "card-menu-item"),
                    actionLink(ns("new_file"), "Add a file..."), tags$br(),
                    actionLink(ns("new_folder"), "Add a folder...")
                ),
                actionLink(ns("test"), label = bs_icon("play-fill"), class = "text-success card-menu-item")
            )
        ),
        card_body(padding = 0,
            layout_columns(col_widths = c(12),
                div(
                    aceEditor(ns("code"), custom.example.string, mode = "r", theme="textmate", autoComplete = "live", height = "20rem")
                ),
            )
        ),
        card_footer(
            uiOutput(ns("status"), inline = T)
        )
    )
}
