rm(list=ls())

options(useFancyQuotes = FALSE)
library(shiny)
library(shinyTree)
library(shinyjs)

library(data.tree)
library(jsonlite)
library(rlang)

dim <- getShinyOption(".data")
