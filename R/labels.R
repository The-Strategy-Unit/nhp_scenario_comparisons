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
  "Procedures" = "procedures",
  "Attendances" = "attendances",
  "Tele-attendances" = "tele_attendances",
  "Arrivals" = "arrivals",
  "Walk-in" = "walk-in",
  "Ambulance" = "ambulance"
)

pod_categories <- list(
  "Inpatients" = c(
    "Non-Elective Admission" = "ip_non-elective_admission",
    "Elective Admission" = "ip_elective_admission",
    "Daycase Admission" = "ip_elective_daycase",
    "Maternity Admission" = "ip_maternity_admission",
    "Regular Day Attender Admission" = "ip_regular_day_attender",
    "Regular Night Attender Admission" = "ip_regular_night_attender"
  ),
  "Outpatients" = c(
    "First Outpatient Attendance" = "op_first",
    "Follow-up Outpatient Attendance" = "op_follow-up",
    "Outpatient Procedure" = "op_procedure"
  ),
  "A&E" = c(
    "Type 1 Department" = "aae_type-01",
    "Type 2 Department" = "aae_type-02",
    "Type 3 Department" = "aae_type-03",
    "Type 4 Department" = "aae_type-04",
    "Type 5 Department" = "aae_type-05"
  )
)
