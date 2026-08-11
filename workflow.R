#setwd("/Volumes/chronofix/Rebecca/chronofix-analysis")
#pak::pkg_install("mrc-ide/chronofix@flatten-chains")
#pak::pkg_install("mrc-ide/chronofix@generate_linelist")
#pak::pkg_install("mrc-ide/chronofix")
#pak::pkg_install("mrc-ide/monty@mrc-6769")

library(orderly)
library(hipercow)

# orderly_location_fetch_metadata()
# orderly_location_pull(task_id, location = "chronofix_network")
orderly_location_fetch_metadata("chronofix_network")

hipercow_provision(method = "pkgdepends")

# Create a named list containing the simulation parameters for all scenarios
orderly_run("sim_params")

# Simulate data for all scenarios
sim100 <- task_create_expr(
  orderly::orderly_run("sim_data", parameters = list(nsims = 100)),
  parallel = hipercow_parallel("parallel"),
  resources = hipercow::hipercow_resources(cores = 4)
)

task_status(sim100)
task_info(sim100)
task_result(sim100)

# MCMC output -----------------------------------------------------------------

## all simulation scenarios
# "baseline" x
# "low_missingness"
# "no_missing"
# "no_error"
# "no_error_no_missing"
# "low_error"
# "high_error"
# "very_small_sample"
# "small_sample"
# "moderate_sample"
# "very_large_sample"
# "long_delays"
# "short_delays"
# "high_variability"
# "low_variability"
# "lognormal_delays"
# "same_means" x

baseline <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "baseline",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(baseline)


no_missing <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "no_missing",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(no_missing)


no_error <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "no_error",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(no_error)


no_error_no_missing <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "no_error_no_missing",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(no_error_no_missing)


low_missingness <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "low_missingness",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(low_missingness)


low_error <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "low_error",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(low_error)


high_error <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "high_error",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(high_error)


## sample sizes

very_small_sample <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "very_small_sample",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(very_small_sample)


small_sample <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "small_sample",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(small_sample)


moderate_sample <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "moderate_sample",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(moderate_sample)


very_large_sample <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "very_large_sample",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(very_large_sample)


long_delays <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "long_delays",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(long_delays)


short_delays <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "short_delays",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(short_delays)


high_variability <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "high_variability",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(high_variability)


low_variability <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "low_variability",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(low_variability)


lognormal_delays <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "lognormal_delays",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(lognormal_delays)


same_mean_delays <- 
  hipercow::task_create_bulk_expr(
    orderly::orderly_run("sim_estim",
                         parameters = list(scenario = "same_means",
                                           dataset = dataset)),
    data.frame(dataset = seq_len(100)),
    resources = hipercow::hipercow_resources(cores = 4))

hipercow_bundle_result(same_mean_delays)

# Collate ------------------------------------------------------------------

resources <- hipercow_resources(cores = 32)

## Baseline
baseline_collate <- task_create_expr(
  orderly::orderly_run(
    "sim_collate",
    parameters = list(scenario = "baseline")),
  resources = resources
)
task_result(baseline_collate)

## No error
no_error_collate <- task_create_expr(
  orderly::orderly_run(
    "sim_collate",
    parameters = list(scenario = "no_error")),
  resources = resources
)
task_result(no_error_collate)

## No missing
no_missing_collate <- task_create_expr(
  orderly::orderly_run(
    "sim_collate",
    parameters = list(scenario = "no_missing")),
  resources = resources
)
task_result(no_missing_collate)

## No error and no missing
no_error_no_missing_collate <- task_create_expr(
  orderly::orderly_run(
    "sim_collate",
    parameters = list(scenario = "no_error_no_missing")),
  resources = resources
)
task_result(no_error_no_missing_collate)

## Low error
low_error_collate <- task_create_expr(
  orderly::orderly_run(
    "sim_collate",
    parameters = list(scenario = "low_error")),
  resources = resources
)
task_result(low_error_collate)

## High error
high_error_collate <- task_create_expr(
  orderly::orderly_run(
    "sim_collate",
    parameters = list(scenario = "high_error")),
  resources = resources
)
task_result(high_error_collate)

## Low missingness
low_missingness_collate <- task_create_expr(
  orderly::orderly_run(
    "sim_collate",
    parameters = list(scenario = "low_missingness")),
  resources = resources
)
task_result(low_missingness_collate)

## Very small sample
very_small_sample_collate <- task_create_expr(
  orderly::orderly_run(
    "sim_collate",
    parameters = list(scenario = "very_small_sample")),
  resources = resources
)
task_result(very_small_sample_collate)

## Small sample
small_sample_collate <- task_create_expr(
  orderly::orderly_run(
    "sim_collate",
    parameters = list(scenario = "small_sample")),
  resources = resources
)
task_result(small_sample_collate)

## Moderate sample
moderate_sample_collate <- task_create_expr(
  orderly::orderly_run(
    "sim_collate",
    parameters = list(scenario = "moderate_sample")),
  resources = resources
)
task_result(moderate_sample_collate)

## Very large sample
very_large_sample_collate <- task_create_expr(
  orderly::orderly_run(
    "sim_collate",
    parameters = list(scenario = "very_large_sample")),
  resources = resources
)
task_result(very_large_sample_collate)

## Long delays
long_delays_collate <- task_create_expr(
  orderly::orderly_run(
    "sim_collate",
    parameters = list(scenario = "long_delays")),
  resources = resources
)
task_result(long_delays_collate)

## Short delays
short_delays_collate <- task_create_expr(
  orderly::orderly_run(
    "sim_collate",
    parameters = list(scenario = "short_delays")),
  resources = resources
)
task_result(short_delays_collate)

## High variability
high_variability_collate <- task_create_expr(
  orderly::orderly_run(
    "sim_collate",
    parameters = list(scenario = "high_variability")),
  resources = resources
)
task_result(high_variability_collate)

## Low variability
low_variability_collate <- task_create_expr(
  orderly::orderly_run(
    "sim_collate",
    parameters = list(scenario = "low_variability")),
  resources = resources
)
task_result(low_variability_collate)

## Log-normal delays
lognormal_delays_collate <- task_create_expr(
  orderly::orderly_run(
    "sim_collate",
    parameters = list(scenario = "lognormal_delays")),
  resources = resources
)
task_result(lognormal_delays_collate)

## Same delay means
same_mean_delays_collate <- task_create_expr(
  orderly::orderly_run(
    "sim_collate",
    parameters = list(scenario = "same_means")),
  resources = resources
)
task_result(same_mean_delays_collate)


# Visualisations -------------------------------------------------------------

## sanity check diagnostics -----------------------

resources <- hipercow_resources(cores = 1)
sanity <- task_create_expr(
  orderly::orderly_run(
    "sim_comparison",
    parameters = list(comparison = "sanity")),
  resources = resources
)

task_info(sanity)
task_result(sanity)

## variable error diagnostics -----------------------

variable_error <- task_create_expr(
  orderly::orderly_run(
    "sim_comparison",
    parameters = list(comparison = "variable_error")),
  resources = resources
)

task_info(variable_error)
task_result(variable_error)

## variable group sample size -----------------------

variable_sample <- task_create_expr(
  orderly::orderly_run(
    "sim_comparison",
    parameters = list(comparison = "variable_sample_size")),
  resources = resources
)

task_info(variable_sample)
task_result(variable_sample)


## variable delay diagnostics -----------------------

variable_delays <- task_create_expr(
  orderly::orderly_run(
    "sim_comparison",
    parameters = list(comparison = "variable_delay_length")),
  resources = resources
)

task_info(variable_delays)
task_result(variable_delays)

## variable cv -----------------------

variable_cv <- task_create_expr(
  orderly::orderly_run(
    "sim_comparison",
    parameters = list(comparison = "variable_delay_cv")),
  resources = resources
)

task_info(variable_cv)
task_result(variable_cv)


## variable delay type -----------------------

variable_distr <- task_create_expr(
  orderly::orderly_run(
    "sim_comparison",
    parameters = list(comparison = "variable_delay_distribution")),
  resources = resources
)

task_info(variable_distr)
task_result(variable_distr)

## variable delay means vs all delays with the same mean -----------------------

same_delay_mean <- task_create_expr(
  orderly::orderly_run(
    "sim_comparison",
    parameters = list(comparison = "same_means")),
  resources = resources
)

task_info(same_delay_mean)
task_result(same_delay_mean)
