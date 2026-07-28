load_df <- function(type) {
  scenario_labels <- scenario_labels[intersect(names(scenario_labels), scenarios)]
  scenario_labels <- factor(scenario_labels, levels = scenario_labels)
  
  map_dfr(scenarios, ~readRDS(glue("{type}/{type}_{.x}.rds")),
          .id = "dataset")
}
