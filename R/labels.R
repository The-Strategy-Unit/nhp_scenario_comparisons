# Function to ensure plot titles use the full English labels where we are filtering on a field
get_label <- function(value, aliases) {  
  names(aliases)[match(value, aliases)]
}

activity_type_pretty_names <- c(
  "Inpatient" = "ip",
  "Outpatient" = "op",
  "A&E" = "aae"
)

measure_pretty_names <- c(
  "Admissions" = "admissions",
  "Bed Days" = "beddays",
  "Attendances" = "attendances",
  "Tele-attendances" = "tele_attendances",
  "Arrivals" = "arrivals"
  
)