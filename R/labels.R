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

pod_pretty_names <- c(
  "Inpatients - Non-Elective Admission" = "ip_non-elective_admission",
  "Inpatients - Elective Admission" = "ip_elective_admission",
  "Inpatients - Daycase Admission" = "ip_elective_daycase",
  "Inpatients - Maternity Admission" = "ip_maternity_admission",
  "Inpatients - Regular Day Attender Admission" = "ip_regular_day_attender",
  "Inpatients - Regular Night Attender Admission" = "ip_regular_night_attender",
  "Outpatients - First Outpatient Attendance" = "op_first",
  "Outpatients - Follow-up Outpatient Attendance" = "op_follow-up",
  "Outpatients - Outpatient Procedure" = "op_procedure",
  "A&E - Type 1 Department" = "aae_type-01",
  "A&E - Type 2 Department" = "aae_type-02",
  "A&E - Type 3 Department" = "aae_type-03",
  "A&E - Type 4 Department" = "aae_type-04",
  "A&E - Type 5 Department" = "aae_type-05"
)
