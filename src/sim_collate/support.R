load_df <- function(nsims, type) {
  purrr::map_dfr(seq_len(nsims), ~readRDS(glue("inputs/{type}/{type}_{.x}.rds")),
                 .id = "dataset")
}
