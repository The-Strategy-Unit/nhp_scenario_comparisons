library(dplyr)

source("R/nhp_outputs/fct_get_data.R")
# load the example of a model run
nottingham <- jsonlite::read_json(
  here::here(
    "jsons", 
    "rx1-241204-rx1-low-sc04-01-20241206-165358_results.json"
    )
  ) |> 
  parse_results()


# load the  nee mitigators
nee <- readRDS(
  here::here(
    "supporting_data", 
    "nee_table.Rds"
    )
  )

# function to update the params in a json with the results of the nee exercise, 
# where an NEE strategy has been chosen
update_intervals <- function(r, lookup) {
  # Define paths to update
  categories <- list(
    list("params", "activity_avoidance", "ip"),
    list("params", "activity_avoidance", "op"),
    list("params", "activity_avoidance", "aae"),
    list("params", "efficiencies", "ip"),
    list("params", "efficiencies", "op")
  )
  
  # Iterate over each category path
  for (path in categories) {
    sublist <- r[[path[[1]]]][[path[[2]]]][[path[[3]]]]
    
    # Identify strategies that exist in lookup
    valid_strategies <- intersect(names(sublist), lookup$param_name)
    
    # Remove strategies not in lookup
    sublist <- sublist[valid_strategies]
    
    # Update valid strategies
    for (strategy in valid_strategies) {
      match_row <- lookup[lookup$param_name == strategy, ]
      if (nrow(match_row) > 0) {
        sublist[[strategy]]$interval <- list(match_row$LowerCI, match_row$UpperCI)
      }
    }
    
    # Assign modified sublist back
    r[[path[[1]]]][[path[[2]]]][[path[[3]]]] <- sublist
  }
  
  return(r)
}


# create copy of nottingham nee
nottingham_nee <- nottingham

# substitute in the NEE data
nottingham_nee <- update_intervals(nottingham, nee)

# quick check:
nottingham_nee$params$activity_avoidance$ip$frail_elderly_high$interval[[1]] # 83.60302
nottingham_nee$params$activity_avoidance$ip$frail_elderly_high$interval[[2]] # 99.56677

nee |> 
  filter(param_name == "frail_elderly_high") |> 
  select(LowerCI, UpperCI)

# A tibble: 1 × 2
# LowerCI UpperCI
# <dbl>   <dbl>
# 83.6    99.6

