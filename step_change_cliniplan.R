library(jsonlite)
library(tidyverse)
library(dplyr)
library(gt)
library(here)
library(ggplot2)
library(ggeasy)
library(zeallot)
library(plotly)

# Functions
file_names <- list.files(path = 'R', pattern = "\\.R$")
lapply(paste0('R/',file_names), source)

file_names_nhs_output <- list.files(path = 'R/nhp_outputs', pattern = "\\.R$")
lapply(paste0('R/nhp_outputs/',file_names_nhs_output), source)

# read Json ndg1
result_notts  <- "jsons/rx1-241204-rx1-low-sc04-01-20241206-165358_results.json" |>
  jsonlite::read_json() |>
  parse_results() 

#Convert Json into dataframe
 df1_convert_to_table <- as.data.frame(bind_rows(result_notts$results$step_counts))|> 
   select(pod,change_factor,
             activity_type,
             strategy,measure,value)  |> 
   filter(pod != "ip_elective_daycase", 
          measure %in% c("admissions",
                         "beddays")) |> select(-pod) |> group_by(activity_type,change_factor,
                                                                 strategy,measure) |> 
   summarize(value = sum(value))
          

### data pre-processing ####

### creating step change 

pivot_data <- df1_convert_to_table  |> 
  pivot_wider(
    names_from = measure, 
    values_from = value
  )

pivot_data_adj <- pivot_data |> mutate( adj_bed_days = +beddays-admissions)
denominator <- pivot_data_adj |> 
  filter(change_factor == "baseline" ) |> 
  pull(adj_bed_days) |> 
  as.numeric()  


pivot_data_adj2 <- pivot_data_adj |> 
  mutate( adj_bed_days_percent = +adj_bed_days/denominator,
   adj_bed_days_year = +adj_bed_days/365)

calc_baseline_adjustment <- pivot_data_adj2 |> 
  filter (change_factor == "baseline_adjustment") |> 
  mutate(adj_bed_test = +denominator/365) |>  pull (adj_bed_test)


ndg_variants_baseline_adjustment <- pivot_data_adj2 |> 
  mutate(change_factor = factor(change_factor, levels=c("baseline","baseline_adjustment","birth_adjustment","covid_adjustment","health_status_adjustment",
                                                           "demographic_adjustment","non-demographic_adjustment","activity_avoidance", "efficiencies","model_interaction_term","waiting_list_adjustment"))) |> 
  arrange(change_factor)  

ndg_variants_baseline_adjustment <- ndg_variants_baseline_adjustment |>
  ungroup() |> 
  mutate(
    calc_adj_bed_percent_v2 = c(
      NA_real_, # First row
      calc_baseline_adjustment * (1 + adj_bed_days_percent[2]), # Second row
      accumulate(adj_bed_days_percent[-c(1, 2)], ~ .x * (1 + .y), .init = calc_baseline_adjustment)[-1] # Rest of the rows
    )
  )


ndg_variants_baseline_adjustment <- ndg_variants_baseline_adjustment |> 
  mutate(adj_lag = calc_adj_bed_percent_v2 - lag(calc_adj_bed_percent_v2))

calc_change_birth <- ndg_variants_baseline_adjustment |> 
  filter (change_factor %in% c("birth_adjustment","covid_adjustment", "health_status_adjustment" ,"demographic_adjustment","non-demographic_adjustment")) |> 
  group_by(activity_type) |> 
  summarise(adj_sum = sum(adj_lag), .groups = "drop") |> mutate(change_factor = "birth_adjustment",
                                                              strategy = NA)

calc_activity_avoidance <- ndg_variants_baseline_adjustment |> 
  filter (change_factor == "activity_avoidance" ) |> group_by(activity_type) |> 
  summarise(adj_sum = sum(adj_lag), .groups = "drop") |> mutate(change_factor = "activity_avoidance",
                                                              strategy = "alcohol_partially_attributable_acute")

calc_efficiencies <- ndg_variants_baseline_adjustment |> 
  filter (change_factor == "efficiencies" ) |> group_by(activity_type) |> 
  summarise(adj_sum = sum(adj_lag), .groups = "drop")  |> mutate(change_factor = "efficiencies",
                                                               strategy = "ambulatory_emergency_care_high")

calc_model_interaction_term  <- ndg_variants_baseline_adjustment |> 
  filter (change_factor == "model_interaction_term" ) |> group_by(activity_type) |> 
  summarise(adj_sum = sum(calc_adj_bed_percent_v2)/1661, .groups = "drop") |> mutate(change_factor = "model_interaction_term",
                                                              strategy = NA)

adj_sum <- bind_rows(calc_change_birth,
                     calc_activity_avoidance,
                     calc_efficiencies,
                     calc_model_interaction_term)

ndg_variants_baseline_adjustment <- left_join(x = ndg_variants_baseline_adjustment,
                                              y = adj_sum,
                                              by = c("activity_type","change_factor","strategy"))

calc_change_birth_sum <- ndg_variants_baseline_adjustment |> 
  filter (change_factor == "birth_adjustment") |> group_by(activity_type) |> 
  summarise(adj_sum_v2 = sum(adj_sum), .groups = "drop") |> mutate(change_factor = "birth_adjustment",
                                                                strategy = NA)

occupancy_rate <- 0.85
# create new table ro calculate 85% occupancy bed rate
adjusted_bed_table <- ndg_variants_baseline_adjustment |> 
  select(activity_type,change_factor,strategy,admissions,adj_bed_days,adj_bed_days_percent) |> 
  filter(change_factor %in% c("activity_avoidance","efficiencies")) |> 
  mutate(adj_bed_days_percent = abs(round(adj_bed_days/365/occupancy_rate, digits = 4)),
         adj_bed_days = abs(round(adj_bed_days,digits = 0)),
         admissions = abs(round(admissions,digits = 0))) |> 
  rename(at_85 = adj_bed_days_percent) |> ungroup() |>  arrange(desc(at_85)) 

top_12 <- head(adjusted_bed_table, 12) |> ungroup()  |>  arrange(desc(at_85)) |> ungroup()
other <- adjusted_bed_table[-(1:12), ]  |>  arrange(desc(at_85)) |> group_by(activity_type) |> 
  summarise(admissions = sum(admissions),
            adj_bed_days = sum(adj_bed_days),
            at_85 = sum(at_85),
            .groups = "drop") |> ungroup() |> mutate(change_factor = "other_mitigator",
                                        strategy = "other_mitigator") 

calc_85_rate <- bind_rows(top_12,other,
                          .id = "Type") |>  
                mutate(strategy = factor(strategy, levels = unique(strategy)))


  ggplot(calc_85_rate,
       aes(x=reorder(strategy,at_85), y=at_85, fill = Type)) + 
  geom_bar(stat = "identity") +
  coord_flip()  +
  ggtitle("Adjusted Beds at 85% Occupancy") +
  ylab("Beds") +
  xlab("Mitigator") +
    scale_fill_manual(values = c("#f9bf07","#686f73"), name="Type",
                      labels = c("Top 12", "Others")) +
  easy_center_title() + theme(text = element_text(family = "Segoe UI")) +
  theme(axis.text.x = element_text(family = "Segoe UI", size = 12, color="black")) +
  theme(axis.text.y = element_text(family = "Segoe UI", size = 12, color="black")) +
  theme(axis.title.x = element_text(family = "Segoe UI", size = 12, color="black")) +
  theme(axis.title.y = element_text(family = "Segoe UI", size = 12, color="black")) +
  theme(legend.title = element_text(family = "Segoe UI", size = 12, color="black")) +
  theme(legend.text = element_text(family = "Segoe UI", size = 12, color="black"))
  