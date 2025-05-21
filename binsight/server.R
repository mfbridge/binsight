
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

    observeEvent(input$metadata_file, {
        #print(input$metadata_file)
        filename = input$metadata_file[1, "name"]
        filepath = input$metadata_file[1, "datapath"]

        output$metadata_table = renderExcel({
            excelTable(readxl::read_xlsx(filepath))
        })
    })

    observeEvent(input$temp_view, {
        #View(state$data)
        #browser()
        print(tinyfiledialogs::openFileDialog())
    })
    observeEvent(input$temp_view, {
        #View(state$temp)
        #browser()
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



    updatePlotView = function() {
        updateVirtualSelect("plot_dataset", choices = names(state$final))
    }

    observeEvent(input$plot_dataset, {
        req(input$plot_dataset %in% names(state$final))

        id.var = unique(unlist(sapply(state$meta, \(f) { return(f$id) })))
        group.vars = unique(unlist(sapply(state$meta, \(f) { return(f$group) })))

        coln = colnames(state$final[[input$plot_dataset]])

        # exclude a few columns
        coln = coln[!(coln %in% c("Bin", "[Bin Start]", id.var, group.vars))]
        updateVirtualSelect("plot_vars", choices = coln)

        clnx = c(id.var, group.vars, "Variable")
        clny = c(id.var, group.vars, "Variable")
        clvx = paste0("X/", clnx)
        clvy = paste0("Y/", clny)
        clvc = list(X = setNames(clvx, clnx), Y = setNames(clvy, clny))

        updateVirtualSelect("plot_facets", choices = clvc)



        if (is.null(id.var)) {
            shinyjs::disable("plot_ids")
        } else {
            shinyjs::enable("plot_ids")
            id.list = unique(state$final[[input$plot_dataset]][, id.var, with = F])
            updateVirtualSelect("plot_ids", choices = id.list, selected = unlist(id.list, use.names = F))
        }

        if (is.null(group.vars)) {
            shinyjs::disable("plot_groups")
        } else {
            shinyjs::enable("plot_groups")
            # unique group values
            gvals = sapply(state$final[[input$plot_dataset]][, group.vars, with = F], unique)
            gll = lapply(colnames(gvals), \(x) { xn = paste0(x, "/", as.character(gvals[,x])); return(setNames(xn, as.character(gvals[,x]))); })
            gllc = setNames(gll, colnames(gvals))
            updateVirtualSelect("plot_groups", choices = gllc, selected = unlist(gllc, use.names = F))
        }
    })

    observeEvent(c(input$plot_dataset, input$plot_vars, input$plot_type, input$plot_facets, input$plot_ids, input$plot_groups), {
        req(input$plot_dataset %in% names(state$final))
        req(input$plot_vars %in% names(state$final[[input$plot_dataset]]))

        #browser()

        output$plot1 = renderPlot({
            id.var = unique(unlist(sapply(state$meta, \(f) { return(f$id) })))
            group.vars = unique(unlist(sapply(state$meta, \(f) { return(f$group) })))

            byvars = c("Bin", id.var, group.vars)

            src = copy(state$final[[input$plot_dataset]])
            for (b in byvars) {
                if (b != "Bin") src[, (b) := factor(get(b))]
            }

            dt = melt(src, id.vars = byvars, measure.vars = input$plot_vars, variable.name = "Variable")

            if (!is.null(input$plot_ids)) {
                dt = dt[.id %in% input$plot_ids, env = list(.id = id.var)]
            }

            if (!is.null(input$plot_groups)) {
                gl = strsplit(input$plot_groups, "/", fixed = T)
                for (i in 1:length(gl)) {
                    gvals = sapply(Filter(\(x) { x[1] == gl[[i]][1] }, gl), \(x) return(x[2]))

                    dt = dt[.group %in% gvals, env = list(.group = gl[[i]][1])]
                }
            }


            gg = ggplot(dt, aes(x = Bin)) + scale_x_continuous()


            for (var in input$plot_vars) {
                var.data = dt[Variable == var,]
                gg = gg + geom_line(aes(x = Bin, y = value, group = .data[[id.var]], color = .data[[id.var]]), data = var.data)
                #if (input$plot_type == "bar") gg = gg + geom_col(aes(x = Bin, y = value, fill = Variable), data = var.data, position = position_identity(), width = 1)
            }



            if (!is.null(input$plot_facets)) {
                sel.x = gsub("X/", input$plot_facets[startsWith(input$plot_facets, "X/")], replacement = "", fixed = T)
                sel.y = gsub("Y/", input$plot_facets[startsWith(input$plot_facets, "Y/")], replacement = "", fixed = T)

                if (length(sel.x) == 0) sel.x = "."
                if (length(sel.y) == 0) sel.y = "."

                fform = as.formula(paste(sel.x, "~", sel.y))

                #browser()

                gg = gg +
                    facet_grid(fform, scales = "free_y")
            }

            gg = gg +
                labs(y = "Value")

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
        updatePlotView()

        showNotification("finished binning datasets")
    })


}
