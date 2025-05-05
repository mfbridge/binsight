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

options(shiny.fullstacktrace = T)

State = R6::R6Class("State",
    public = list(
        data = list(),
        temp = list(),
        final = list()
    )
)

source("csv_import.R", local = T)
source("custom_preprocess.R", local = T)
source("binning.R", local = T)
