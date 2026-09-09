library(chronofix)

orderly::orderly_shared_resource("util.R")
source("util.R")

version_check("chronofix", "0.0.9")

orderly_dependency("ebola_fit", "latest", 
                   files = c("inputs/samples.rds" = "outputs/samples.rds",
                             "inputs/data.rds" = "outputs/data.rds"))

orderly_artefact(description = "chronofix linelist",
                 files = "outputs/chronofix_linelist.xlsx")

samples <- readRDS("inputs/samples.rds")
data <- readRDS("inputs/data.rds")

dir.create("outputs", showWarnings = FALSE)
chronofix::chronofix_linelist(samples, data, 
                              filename = "outputs/chronofix_linelist.xlsx")
