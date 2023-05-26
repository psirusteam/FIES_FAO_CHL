################################################################################
## Title:        Modelo de área (\theta ~ beta)                             ##
## Authors:      Stalyn Guerrero- Andrés Gutiérrez                            ##
## Date:         05-2023                                                      ##
################################################################################

## Lectura de librerías 
rm(list = ls())
library(tidyverse)
library(magrittr)
library(patchwork)
library(thematic)
library(sp)
library(sf)
library(tmap)

theme_set(theme_bw())
thematic_on(
  bg = "white", fg = "black", accent = "red",
  font = font_spec("Oxanium", scale = 1.25)
)
select <- dplyr::select
dam2 <- "comuna"
## Lectura de base de datos

estimacionesBench  <- readRDS("Data/estimacionesBench.rds")

## Leer Shapefile del país
ShapeSAE <- read_sf("Shape/CHL_dam2.shp")


mapa <- tm_shape(ShapeSAE %>%
                   left_join(estimacionesBench,  by = "dam2"))

brks_lp <- seq(0,0.45,0.09)
tmap_options(check.and.fix = TRUE)
Mapa_lp <-
  mapa + tm_polygons(
    c("theta_pred_RBench"),
    breaks = brks_lp,
    title = "Inseguridad Alimentaria",
    palette = "Blues",
    colorNA = "#A6ACAF"
  ) + tm_layout(asp = 1.5)

Mapa_lp

tmap_save(
  Mapa_lp,
  "Data/RecursosBook/04/CHL_FIES.jpeg",
  width = 3000,
  height = 2000,
  asp = 0
)
