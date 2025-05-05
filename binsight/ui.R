
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
            font_scale = 1.0,
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
                    accordion(id = "step1_accordion", multiple = T, open = T,
                        accordion_panel(title = HTML("<b>1 &mdash; Import</b>"),
                            csv_import_ui("csv"),
                        ),

                        tags$br(),

                        accordion_panel(title = HTML("<b>2 &mdash; (Optional) Pre-processing</b>"),
                            custom_preprocess_ui("custom1"),
                        ),

                        tags$br(),

                        accordion_panel(title = HTML("<b>3 &mdash; Transform</b>"),
                            binning_ui("bin")
                        )
                    )
                )
            ),
            nav_menu(
                title = "Visualization & Analysis",
                "Summary",
                nav_panel("Data tables",
                    card(style = "font-size: 0.75rem",
                        pickerInput("preview_dataset", label = "Dataset", choices = c()),
                        DT::DTOutput("preview_table", fill = T)
                        # card_footer(
                        #     uiOutput(("preview_status"), inline = T)
                        # ),
                    )
                ),
                nav_panel("Values across time (line, bar plots)",
                    layout_sidebar(
                        sidebar = sidebar(
                            pickerInput("plot_type", label = "Type", choices = c("line", "bar")),
                            checkboxInput("facet_vars", label = "Facet by Variable", value = F)
                        ),
                        card(
                            card_header(style = "display: flex;",
                                pickerInput("plot_dataset", label = "Dataset", choices = c(), inline = T),
                                pickerInput("plot_vars", label = "Variable(s)", choices = c(), multiple = T, inline = T)
                            ),
                            plotOutput("plot1")
                        )
                    )
                ),

                nav_panel("Values across cycles (actogram, heatmap)",

                ),

                "Analysis",
                nav_panel(tooltip("Groups (GLMM, RMANOVA)", "Comparisons across time, optionally accounting for repeated measures.", placement = "right"),
                    layout_sidebar(
                        sidebar = sidebar(
                            "options go here"
                        ),
                        card(
                            card_header(style = "display: flex;",
                                pickerInput("analysis_dataset", label = "Dataset", choices = c(), inline = T),
                                pickerInput("analysis_fixed", label = "Fixed Effect(s)", choices = c(), multiple = T, inline = T),
                                pickerInput("analysis_random", label = "Random Effect(s)", choices = c(), multiple = T, inline = T),
                            ),
                            plotOutput("plot2")
                        )
                    )
                ),
                nav_panel(tooltip("Interventions (t-test, Wilcoxon)", "Comparison before and after an intervention.", placement = "right"), "test")
            )
        )
    )
)
