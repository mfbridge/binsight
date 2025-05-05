
# Define server logic required to draw a histogram
function(input, output, session) {
    state = State$new()

    gargoyle::init("data_update")
    gargoyle::init("temp_update")
    gargoyle::init("final_update")

    csv_import_server(state, "csv")
    custom_preprocess_server(state, "custom1")
    binning_server(state, "bin")

    observeEvent(input$reprocess, {
        output$final_dataset_list = renderPrint({
            for (i in 1:length(names(state$data))) {
                fname = names(state$data)[[i]]

                strr = sprintf("%s has %d columns and %d rows\r\n", fname, length(names(state$data[[fname]])), nrow(state$data[[fname]]))

                showNotification(strr)
                cat(strr)
            }
        })
    })

    observeEvent(input$final_view, {
        View(state$data)
        browser()
    })
    observeEvent(input$temp_view, {
        View(state$temp)
        browser()
    })

    updatePreviewDatasets = function() {
        dn = as.list(names(state$data))
        dv = paste0("data/", names(state$data))
        tn = as.list(names(state$temp))
        tv = paste0("temp/", names(state$temp))
        fn = as.list(names(state$final))
        fv = paste0("final/", names(state$final))

        name.list = list()
        if (length(dn) > 0) {
            name.list = append(name.list, list("Raw Datasets" = setNames(dv, dn)))
        }
        if (length(tn) > 0) {
            name.list = append(name.list, list("Temp Datasets" = setNames(tv, tn)))
        }
        if (length(fn) > 0) {
            name.list = append(name.list, list("Final Datasets" = setNames(fv, fn)))
            updatePickerInput(session, "plot_dataset", choices = fn, selected = input$plot_dataset)
            if (!is.null(input$plot_dataset))
                updatePickerInput(session, "plot_vars", choices = names(state$final[[input$plot_dataset]]), selected = NULL)
        }

        updatePickerInput(session, "preview_dataset", choices = name.list)
    }

    observeEvent(input$preview_dataset, {
        n = strsplit(input$preview_dataset, "/", fixed = T)[[1]]

        if (n[1] == "data") {
            output$preview_table = DT::renderDataTable(state$data[[n[2]]])
        } else if (n[1] == "temp") {
            output$preview_table = DT::renderDataTable(state$temp[[n[2]]])
        } else if (n[1] == "final") {
            output$preview_table = DT::renderDataTable(state$final[[n[2]]])
        }
    })

    observeEvent(input$plot_dataset, {
        req(input$plot_dataset %in% names(state$final))
        updatePickerInput(session, "plot_vars", choices = names(state$final[[input$plot_dataset]]))
    })

    observeEvent(c(input$plot_dataset, input$plot_vars, input$plot_type), {
        req(input$plot_dataset %in% names(state$final))
        req(input$plot_vars %in% names(state$final[[input$plot_dataset]]))

        #browser()

        output$plot1 = renderPlot({
            dt = melt(state$final[[input$plot_dataset]], id.vars = c("Bin"), measure.vars = input$plot_vars)

            gg = ggplot(dt, aes(x = Bin))

            for (var in input$plot_vars) {
                var.data = dt[variable == var,]

                if (input$plot_type == "line") gg = gg + geom_line(aes(x = Bin, y = value, color = variable), data = var.data)
                if (input$plot_type == "bar") gg = gg + geom_col(aes(x = Bin, y = value, fill = variable), data = var.data, position = position_identity(), width = 1)
            }

            if (input$facet_vars) gg = gg + facet_grid(variable ~ ., scales = "free_y")

            gg
        })

    })

    gargoyle::on("data_update", {
        updatePreviewDatasets()

    })


    gargoyle::on("temp_update", {
        updatePreviewDatasets()

    })

    gargoyle::on("final_update", {
        updatePreviewDatasets()

        showNotification("finished binning datasets")
    })


}
