
ui = tagList(
    tags$head(
        tags$link(rel = "stylesheet", type="text/css", href="style.css"),
        useShinyjs(),
    ),
    page_fillable(
        title = "binsight",
        underline = T, gap = "0px", padding = "0px", fillable_mobile = F,
        theme = bs_theme(
            preset = "flatly",

            `border-radius`= "0",
            base_font = font_collection(
                font_google("Fira Sans"),
                "sans-serif"
            ),
            code_font = font_collection(
                "mono"
            ),
            font_scale = 0.8,
        ) |>
        bs_add_variables(`popover-border-radius`= "0") |>
        bs_add_variables(`navbar-padding-y`= "0") |>
        bs_add_variables(`navbar-brand-padding-y` = "0") |>
        bs_add_variables(`btn-font-size` = "0.7rem", `btn-line-height` = "1.6rem") |>
        bs_add_variables(`dropdown-font-size`="0.7rem", `dropdown-padding-y`="0", `dropdown-item-padding-x`="0.5rem", `dropdown-item-padding-y`="0.25rem"),

        navset_card_underline(
            nav_item(tags$b("binsight")),
            nav_panel(
                title = "Import & Preprocessing",
                layout_sidebar(
                    sidebar = sidebar(width = "15rem", open = F,
                        # accordion(open = F,
                        #     accordion_panel(title = "Import",
                        #         prettyCheckboxGroup("import_types", NULL, choices = c("Comma-separated values"="csv"), selected = "csv")
                        #     ),
                        #     accordion_panel(title = "Pre-processing",
                        #         prettyCheckboxGroup("transform_types", NULL, choices = c("Custom script"="custom1"), selected = "custom1")
                        #     ),
                        #     accordion_panel(title = "Transform",
                        #         prettyCheckboxGroup("processing_types", NULL, choices = c("Aggregation/Binning"="bin"), selected = "bin")
                        #     )
                        # )

                        actionButton("reprocess", label = "Reprocess", width = "100%"),
                        actionButton("final_view", "View data in RStudio", width = "100%"),
                        actionButton("temp_view", "View temp in RStudio", width = "100%")
                    ),
                    accordion(multiple = T, open = c("csv"), id = "accordion1",
                        accordion_panel(title = HTML("<b>Import Comma-separated Value Files</b>"), value = "csv",
                            csv_import_ui("csv"),
                        ),

                        accordion_panel(title = HTML("<b>Import via Metadata Excel Sheet</b>"), value = "metaxlsx",
                            layout_columns(col_widths = c(12),
                                fileInput("metadata_file", label = NULL, accept = ".xlsx")
                            ),
                            tags$div("Files", class = "hrtext"),
                            excelOutput("metadata_table")
                        )
                    ),

                    accordion(multiple = T, open = T, id = "accordion2",
                        accordion_panel(title = HTML("<b>(Optional) Pre-processing</b>"),
                            custom_preprocess_ui("custom1"),
                        ),
                    ),

                    accordion(multiple = T, open = T, id = "accordion3",

                        accordion_panel(title = HTML("<b>Binning/aggregation</b>"),
                            binning_ui("bin")
                        )
                    ),

                    div(style="height:200px")
                )
            ),
            nav_menu(
                title = "Visualization & Analysis",
                "Summary",
                nav_panel("Data tables",
                    card(style = "font-size: 0.75rem",
                        pickerInput("preview_dataset", label = "Dataset", choices = c()),
                        DT::DTOutput("preview_table", fill = T)
                    )
                ),
                nav_panel("Values across time (line plots)",
                    layout_sidebar(
                        sidebar = sidebar(open = F,
                            #pickerInput("plot_type", label = "Plot Type", choices = c("line", "bar"))
                        ),
                        card(
                            card_header(style = "padding-bottom: 0;",
                                layout_columns(col_widths = c(3, 3, 2, 2, 2), style = "display: flex;",
                                virtualSelectInput("plot_dataset", label = "Dataset", choices = c(), optionHeight = "24px", multiple = F),
                                virtualSelectInput("plot_vars", label = "Variables", choices = c(), optionHeight = "24px", multiple = T),
                                virtualSelectInput("plot_facets", label = "Facets", choices = c(), selected = NULL, optionHeight = "24px", multiple = T),
                                virtualSelectInput("plot_ids", label = "IDs", choices = c(), optionHeight = "24px", multiple = T),
                                virtualSelectInput("plot_groups", label = "Groups", choices = c(), optionHeight = "24px", multiple = T)
                                )
                            ),
                            plotOutput("plot1")
                        )
                    )
                ),

                # nav_panel("Values across cycles (actogram, heatmap)",
                #
                # ),

                "Analysis",
                # nav_panel(tooltip("Groups (GLMM, RMANOVA)", "Comparisons across time, optionally accounting for repeated measures.", placement = "right"),
                #     layout_sidebar(
                #         sidebar = sidebar(
                #             "options go here"
                #         ),
                #         card(
                #             card_header(style = "display: flex;",
                #                 pickerInput("analysis_dataset", label = "Dataset", choices = c(), inline = T),
                #                 pickerInput("analysis_fixed", label = "Fixed Effect(s)", choices = c(), multiple = T, inline = T),
                #                 pickerInput("analysis_random", label = "Random Effect(s)", choices = c(), multiple = T, inline = T),
                #             ),
                #             plotOutput("plot2")
                #         )
                #     )
                # ),
                nav_panel(tooltip(HTML("<i>t</i>&em;test, Wilcoxon signed-rank sum test"), "Simple comparisons of two populations", placement = "right"), "test")
            )
        )
    )
)
