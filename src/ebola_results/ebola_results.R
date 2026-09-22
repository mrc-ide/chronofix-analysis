library(chronofix)
library(orderly)
library(ggplot2)
library(rmarkdown)

orderly::orderly_shared_resource("util.R")
source("util.R")
orderly::orderly_resource("support.R")
source("support.R")

orderly::orderly_resource("paper_numbers.Rmd")

version_check("chronofix", "0.0.9")

orderly_dependency("ebola_fit", "latest", 
                   files = c("inputs/samples.rds" = "outputs/samples.rds",
                             "inputs/pars_summary.rds" = "outputs/pars_summary.rds",
                             "inputs/data.rds" = "outputs/data.rds",
                             "inputs/data_with_onset_inferred.rds" = "outputs/data_with_onset_inferred.rds",
                             "inputs/delay_map.rds" = "outputs/delay_map.rds"))

orderly_artefact(description = "chronofix linelist",
                 files = "outputs/chronofix_linelist.xlsx")

orderly_artefact(description = "paper numbers",
                 files = "outputs/paper_numbers.html")

#orderly_artefact(description = "figures",
#                 files = "figures/delays_plot.pdf")

samples <- readRDS("inputs/samples.rds")
data_with_onset_inferred <- readRDS("inputs/data_with_onset_inferred.rds")
pars_summary <- readRDS("inputs/pars_summary.rds")

dir.create("outputs", showWarnings = FALSE)
chronofix::chronofix_linelist(samples, 
                              filename = "outputs/chronofix_linelist.xlsx")

#dir.create("figures", showWarnings = FALSE)
#ggsave("figures/delays_plot.pdf", 
#       chronofix::chronofix_plot_delays(samples, delay_map),
#       width = 20, height = 12)

## Render rmd
rmarkdown::render("paper_numbers.Rmd", output_dir = "outputs")
