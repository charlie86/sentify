library(httr)
library(shiny)
library(shinyjs)
library(shinyBS)
library(tidyverse)
library(shinymaterial)
library(tibble)
library(highcharter)
library(RColorBrewer)
library(shinycssloaders)
library(htmltools)
library(lubridate)
library(lazyeval)
library(spotifyr)

rm(list = ls())

source('helpers.R')

jscode <-
    '$(document).on("shiny:connected", function(e) {
  var jsWidth = screen.width;
  Shiny.onInputChange("GetScreenWidth",jsWidth);
});
'
base_url <- 'https://api.spotify.com/v1/'

neon_colors <- c(
    '#84DE02'
    , '#FF4466'
    , '#4BC7CF'
    , '#FF85CF'
    , '#FFDF46'
    , '#391285'
    , '#E88E5A'
    , '#DDE26A'
    , '#C53151'
    , '#B05C52'
    , '#FD5240'
    , '#FF4681'
    , '#FF6D3A'
    , '#FF404C'
    , '#A0E6FF'
)

pca_vars <- c('danceability', 'energy', 'loudness', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms')