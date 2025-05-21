library(shiny)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(shinyjs)
library(excelR)
library(data.table)
library(shinyAce)
library(readr)
library(R6)
library(gargoyle)
library(rlang)
library(DT)
library(ggplot2)
library(shinyvalidate)

options(shiny.fullstacktrace = T, shiny.maxRequestSize = 1024**3)

State = R6::R6Class("State",
    public = list(
        meta = list(),
        data = list(),
        temp = list(),
        final = list(),

        add_dataset = function(dt = NULL, name, .time, .id = NULL, .group = NULL, ...) {
            self$meta[[name]] = list(
                time = .time,
                id = .id,
                group = .group
            )

            self$data[[name]] = copy(dt)

            setkeyv(self$data[[name]], .time)
        },

        remove_dataset = function(name) {

        }
    )
)

source("csv_import.R", local = T)
source("custom_preprocess.R", local = T)
source("binning.R", local = T)
