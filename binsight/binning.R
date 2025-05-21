custom.example.summary = "# calculate min, mean, median, max, for each variable over each bin
function(data, vars, id, group) {
    byvars = c('Bin', '[Bin Start]', id, group)
    byvars = byvars[byvars != '']
    rdt = data[, byvars, with = F]

    for (v in vars) {
        mins = data[, .(val = min(var, na.rm = T)), by = byvars, env = list(var = v, val = sprintf('Min(%s)',v))]
        avgs = data[, .(val = mean(var, na.rm = T)), by = byvars, env = list(var = v, val = sprintf('Mean(%s)',v))]
        meds = data[, .(val = median(var, na.rm = T)), by = byvars, env = list(var = v, val = sprintf('Median(%s)',v))]
        maxs = data[, .(val = max(var, na.rm = T)), by = byvars, env = list(var = v, val = sprintf('Max(%s)',v))]

        rdt = merge(rdt, mins, by = byvars)
        rdt = merge(rdt, avgs, by = byvars)
        rdt = merge(rdt, meds, by = byvars)
        rdt = merge(rdt, maxs, by = byvars)
    }

    return(rdt)
}

"

binning_server = function(state, id) {
    ns = NS(id)


    moduleServer(id, function(input, output, session) {

        valid = InputValidator$new()

        valid$add_rule("bin_size", \(x) {
            if (!is.na(suppressWarnings(as.numeric(x)))) {
                return(NULL)
            }

            if (is.na(lubridate::duration(x))) {
                return("Error: lubridate returned NA.")
            }

            return(NULL)
        })


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
            updateVirtualSelect("vars", choices = varlist)
        })

        observeEvent(input$test, {
            #state$final = list()
            final = list()

            # final dataset name
            if (input$output_name == "") dataset.name = lubridate::format_ISO8601(lubridate::now())
            else dataset.name = input$output_name

            # bin size
            if (!is.na(as.numeric(input$bin_size))) binsize = as.numeric(input$bin_size)
            else binsize = as.numeric(lubridate::duration(input$bin_size), "seconds")


            # temp might be un-populated
            if (length(state$temp) == 0) {
                for (dt in names(state$data)) {
                    state$temp[[dt]] = copy(state$data[[dt]])
                }
            }

            # collect datasets
            all.data = data.table()
            for (dt in names(state$data)) {
                all.data = rbindlist(list(all.data, state$temp[[dt]][, `[Dataset]` := dt]))
            }

            # bin alignment function
            align.fun = NULL
            if (input$round_type == "floor") align.fun = floor
            if (input$round_type == "round") align.fun = round
            if (input$round_type == "ceiling") align.fun = ceiling
            if (input$round_type == "none") align.fun = \(x) { return(x) }

            # bin alignment size
            if (!is.na(as.numeric(input$bin_adjust_size))) roundsize = as.numeric(input$bin_adjust_size)
            else roundsize = as.numeric(lubridate::duration(input$bin_adjust_size), "seconds")

            # vars
            time.var = unique(sapply(state$meta, \(f) { return(f$time) }))
            id.var = unique(unlist(sapply(state$meta, \(f) { return(f$id) })))
            group.vars = unique(unlist(sapply(state$meta, \(f) { return(f$group) })))

            req(time.var)

            #id.list = unique(all.data[, id.var, with = F])
            #group.list = unique(all.data[, group.vars, with = F])

            # origin times
            if (input$start == "early") {
                origin.time = all.data[, min(get(time.var))]
                all.data[, `[Origin]` := origin.time]

            } else if (input$start == "subject") {
                origin.times = all.data[, .(`[Origin]` = min(get(time.var))), by = id.var]
                all.data = merge(all.data, origin.times, by = id.var)

            } else if (input$start == "group") {
                origin.times = all.data[, .(`[Origin]` = min(get(time.var))), by = group.vars]
                all.data = merge(all.data, origin.times, by = group.vars)

            } else if (input$start == "fixed") {

            }

            if (input$bin_adjust == "floor") {
                all.data[, `[Origin]` := lubridate::floor_date(`[Origin]`, unit = input$bin_adjust_size)]

            } else if (input$bin_adjust == "round") {
                all.data[, `[Origin]` := lubridate::round_date(`[Origin]`, unit = input$bin_adjust_size)]

            } else if (input$bin_adjust == "ceiling") {
                all.data[, `[Origin]` := lubridate::ceiling_date(`[Origin]`, unit = input$bin_adjust_size)]
            }

            # assign bin numbers
            all.data[, Bin := as.integer(align.fun(as.numeric(get(time.var) - `[Origin]`, "secs") / binsize))]
            all.data[, `[Bin Start]` := binsize * align.fun(as.numeric(get(time.var) - `[Origin]`, "secs")/binsize) + `[Origin]`]

            out.data = data.table()

            # summaries
            if (input$fun == "custom") {
                ft = data.table()

                .fun = NULL
                tryCatch({
                    .fun = eval(parse(text = input$code), envir = env())
                }, error = \(e) {
                    showNotification(toString(e), type = "error")
                }, warning = \(e) {
                    showNotification(toString(e), type = "warning")
                })

                req(.fun)

                start.t = proc.time()
                tryCatch({
                    ft = do.call(.fun, args = list(data = all.data, vars = input$vars, id = id.var, group = group.vars))

                    end.t = proc.time()
                    print(sprintf("took %0.2f sec", end.t[[3]] - start.t[[3]]))


                }, error = \(e) {
                    showNotification(toString(e), type = "error")
                }, warning = \(e) {
                    showNotification(toString(e), type = "warning")
                })

                out.data = ft

            } else if (input$fun %in% c("mean", "median", "min", "max")) {
                byvars = c('Bin', '[Bin Start]', id.var, group.vars)
                byvars = byvars[byvars != ""]
                byvars = byvars[!is.na(byvars)]

                #browser()

                ft = unique(all.data[, byvars, with = F])

                for (v in input$vars) {
                    if (input$fun == "mean") ft = merge(ft, all.data[, .(value = mean(variable)), by = byvars, env = list(variable = v, value = sprintf("Mean(%s)", v))], by = byvars)
                    if (input$fun == "median") ft = merge(ft, all.data[, .(value = median(variable)), by = byvars, env = list(variable = v, value = sprintf("Median(%s)", v))], by = byvars)
                    if (input$fun == "min") ft = merge(ft, all.data[, .(value = min(variable)), by = byvars, env = list(variable = v, value = sprintf("Min(%s)", v))], by = byvars)
                    if (input$fun == "max") ft = merge(ft, all.data[, .(value = max(variable)), by = byvars, env = list(variable = v, value = sprintf("Max(%s)", v))], by = byvars)
                }

                out.data = ft
            }

            state$final[[dataset.name]] = copy(out.data)

            gargoyle::trigger("final_update")

        })

        observeEvent(input$preview_dataset, {
            output$preview_table = DT::renderDT({
                DT::datatable(data = state$final[[input$preview_dataset]])
            })
        })

        observe({
            if (input$start != "fixed") {
                shinyjs::disable("fixed_time")
            } else {
                shinyjs::enable("fixed_time")
            }
        })

        observe({
            if (input$bin_adjust != "none") {
                shinyjs::enable("bin_adjust_size")
            } else {
                shinyjs::disable("bin_adjust_size")
            }
        })

        gargoyle::on("data_update", {
            disc = c()

            for (dt in names(state$data)) {
                if (is.null(state$meta[[dt]]$id)) {
                    disc = c(disc, "subject")
                }
                if (is.null(state$meta[[dt]]$group)) {
                    disc = c(disc, "group")
                }

                print(disc)
            }

            disc = unique(disc)
            if (is.null(disc)) disc = character(0)

            updateVirtualSelect("start", disabledChoices = disc)
        })



        valid$enable()
    })
}

binning_ui = function(id) {
    ns = NS(id)

    tagList(
    #card(fill = F, style = "font-size: 0.8rem; height: 20rem;", id = id,
    #    card_header("Aggregation/binning", class = "d-flex justify-content-between",
            tags$div("Options", class = "hrtext"),
            div(class = "d-flex justify-content-between",
                actionLink(ns("test"), label = bs_icon("play-fill"), class = "text-success card-menu-item")
            ),
    #    ),

    #    card_body(
            layout_columns(col_widths = c(2, 2, 2, 2, 2, 2),
                textInput(ns("bin_size"), tooltip("Size", "A number (in seconds), or a string with unambiguous units interpretable by lubridate::duration."), value = "15min"),
                virtualSelectInput(ns("start"), tooltip("Origin (t=0)", "Bins are numbered relative to this/these points in time."),
                    choices = c("by subject"="subject", "by group"="group", "earliest"="early", "fixed"="fixed")
                , multiple = F, selected = "early", optionHeight = "24px"),
                textInput(ns("fixed_time"), "Fixed Timestamp", value = Sys.time()),
                virtualSelectInput(ns("round_type"), tooltip("Assignment", "The function used to determine which bin an observation is rounded off into."), choices = c("floor", "round", "ceiling"), selected = "floor", optionHeight = "24px"),
                virtualSelectInput(ns("bin_adjust"), tooltip("Rounding", "Optionally round bin start times to align with more convenient divisions."), choices = c("none", "floor", "round", "ceiling"), selected = "none", optionHeight = "24px"),
                textInput(ns("bin_adjust_size"), "Amount", value = "15min")
            ),
            layout_columns(col_widths = c(3, 6, 3),
                textInput(ns("output_name"), tooltip("Name", "Name of the final dataset containing aggregated and summarized observations from one or more datasets."), value = "", placeholder = "timestamp if empty"),
                virtualSelectInput(ns("vars"), "Variable(s)", choices = c(""), selected = "", multiple = T, search = T, optionsCount = 10, optionHeight = "24px"),
                virtualSelectInput(ns("fun"), "Summary Function", choices = c("minimum", "mean", "median", "maximum", "custom"), selected = "mean", optionHeight = "24px")
            ),
        div("Custom Summary Function", class = "hrtext"),
        aceEditor(ns("code"), custom.example.summary, mode = "r", theme="textmate", autoComplete = "live"),
    #    ),

    #    card_footer(
            #uiOutput(ns("status"), inline = T)
    #    )
    #)
    )
}
