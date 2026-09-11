library(chronofix)
library(orderly)
library(ggplot2)

orderly::orderly_shared_resource("util.R")
source("util.R")

version_check("chronofix", "0.0.9")

orderly_dependency("ebola_fit", "latest", 
                   files = c("inputs/samples.rds" = "outputs/samples.rds",
                             "inputs/data.rds" = "outputs/data.rds",
                             "inputs/delay_map.rds" = "outputs/delay_map.rds"))

orderly_artefact(description = "chronofix linelist",
                 files = "outputs/chronofix_linelist.xlsx")

orderly_artefact(description = "figures",
                 files = "figures/delays_plot.pdf")

samples <- readRDS("inputs/samples.rds")
data <- readRDS("inputs/data.rds")

dir.create("outputs", showWarnings = FALSE)
chronofix::chronofix_linelist(samples, data, 
                              filename = "outputs/chronofix_linelist.xlsx")

dir.create("figures", showWarnings = FALSE)
ggsave("figures/delays_plot.pdf", 
       chronofix::chronofix_plot_delays(samples, delay_map),
       width = 20, height = 12)

