
# Max upload size up to 1GB (per file)
options(shiny.maxRequestSize = 1024^3)

# Run this code before deploy to shinyapps.io
# library(BiocManager)
# options(repos = BiocManager::repositories())

# Load R library
library(shiny)
library(vcfR)
library(dplyr)
library(ggvenn)
library(grid)
library(ggplot2)
library(zip)
library(ggVennDiagram)
library(DT)
