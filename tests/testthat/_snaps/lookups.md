# preloaded lookups are as expected

    Code
      full_apm_lookup
    Output
      # A tibble: 27 x 5
         activity_type activity_type_label pod                       pod_label measure
         <chr>         <fct>               <chr>                     <fct>     <chr>  
       1 ip            Inpatient           ip_non-elective_admission Non-Elec~ admiss~
       2 ip            Inpatient           ip_non-elective_admission Non-Elec~ beddays
       3 ip            Inpatient           ip_elective_admission     Elective~ admiss~
       4 ip            Inpatient           ip_elective_admission     Elective~ beddays
       5 ip            Inpatient           ip_elective_daycase       Daycase ~ admiss~
       6 ip            Inpatient           ip_elective_daycase       Daycase ~ beddays
       7 ip            Inpatient           ip_maternity_admission    Maternit~ admiss~
       8 ip            Inpatient           ip_maternity_admission    Maternit~ beddays
       9 ip            Inpatient           ip_regular_day_attender   Regular ~ admiss~
      10 ip            Inpatient           ip_regular_day_attender   Regular ~ beddays
      # i 17 more rows

---

    Code
      core_mat_tbl
    Output
      # A tibble: 6 x 2
        measure          activity_type
        <chr>            <chr>        
      1 admissions       ip           
      2 beddays          ip           
      3 attendances      op           
      4 tele_attendances op           
      5 walk-in          aae          
      6 ambulance        aae          

---

    Code
      atl_lookup
    Output
      # A tibble: 3 x 2
        activity_type activity_type_label
        <chr>         <fct>              
      1 ip            Inpatient          
      2 op            Outpatient         
      3 aae           A&E                

---

    Code
      full_atp_lookup
    Output
      # A tibble: 14 x 3
         activity_type_label pod                       pod_label                      
         <fct>               <chr>                     <fct>                          
       1 Inpatient           ip_non-elective_admission Non-Elective Admission         
       2 Inpatient           ip_elective_admission     Elective Admission             
       3 Inpatient           ip_elective_daycase       Daycase Admission              
       4 Inpatient           ip_maternity_admission    Maternity Admission            
       5 Inpatient           ip_regular_day_attender   Regular Day Attender Admission 
       6 Inpatient           ip_regular_night_attender Regular Night Attender Admissi~
       7 Outpatient          op_first                  First Outpatient Attendance    
       8 Outpatient          op_follow-up              Follow-up Outpatient Attendance
       9 Outpatient          op_procedure              Outpatient Procedure           
      10 A&E                 aae_type-01               Type 1 Department              
      11 A&E                 aae_type-02               Type 2 Department              
      12 A&E                 aae_type-03               Type 3 Department              
      13 A&E                 aae_type-04               Type 4 Department              
      14 A&E                 aae_type-05               Type 5 Department (SDEC)       

---

    Code
      cond_apm_lookup
    Output
      # A tibble: 18 x 5
         activity_type activity_type_label pod                       pod_label measure
         <chr>         <fct>               <chr>                     <fct>     <chr>  
       1 ip            Inpatient           ip_non-elective_admission Non-Elec~ admiss~
       2 ip            Inpatient           ip_non-elective_admission Non-Elec~ beddays
       3 ip            Inpatient           ip_elective_admission     Elective~ admiss~
       4 ip            Inpatient           ip_elective_admission     Elective~ beddays
       5 ip            Inpatient           ip_elective_daycase       Daycase ~ admiss~
       6 ip            Inpatient           ip_elective_daycase       Daycase ~ beddays
       7 ip            Inpatient           ip_maternity_admission    Maternit~ admiss~
       8 ip            Inpatient           ip_maternity_admission    Maternit~ beddays
       9 ip            Inpatient           ip_regular_day_attender   Regular ~ admiss~
      10 ip            Inpatient           ip_regular_day_attender   Regular ~ beddays
      11 ip            Inpatient           ip_regular_night_attender Regular ~ admiss~
      12 ip            Inpatient           ip_regular_night_attender Regular ~ beddays
      13 op            Outpatient          op_first                  First Ou~ attend~
      14 op            Outpatient          op_first                  First Ou~ tele_a~
      15 op            Outpatient          op_follow-up              Follow-u~ attend~
      16 op            Outpatient          op_follow-up              Follow-u~ tele_a~
      17 op            Outpatient          op_procedure              Outpatie~ attend~
      18 aae           A&E                 aae                       A&E Arri~ arriva~

---

    Code
      cond_ap_lookup
    Output
      # A tibble: 10 x 4
         activity_type activity_type_label pod                       pod_label        
         <chr>         <fct>               <chr>                     <fct>            
       1 ip            Inpatient           ip_non-elective_admission Non-Elective Adm~
       2 ip            Inpatient           ip_elective_admission     Elective Admissi~
       3 ip            Inpatient           ip_elective_daycase       Daycase Admission
       4 ip            Inpatient           ip_maternity_admission    Maternity Admiss~
       5 ip            Inpatient           ip_regular_day_attender   Regular Day Atte~
       6 ip            Inpatient           ip_regular_night_attender Regular Night At~
       7 op            Outpatient          op_first                  First Outpatient~
       8 op            Outpatient          op_follow-up              Follow-up Outpat~
       9 op            Outpatient          op_procedure              Outpatient Proce~
      10 aae           A&E                 aae                       A&E Arrivals     

---

    Code
      tpma_lookup
    Output
      # A tibble: 92 x 4
         strategy                               activity_type change_factor tpma_label
         <chr>                                  <chr>         <chr>         <glue>    
       1 discharged_no_treatment_adult_ambulan~ aae           activity_avo~ Discharge~
       2 discharged_no_treatment_adult_walk-in  aae           activity_avo~ Discharge~
       3 discharged_no_treatment_child_ambulan~ aae           activity_avo~ Discharge~
       4 discharged_no_treatment_child_walk-in  aae           activity_avo~ Discharge~
       5 frequent_attenders_adult_ambulance     aae           activity_avo~ Frequent ~
       6 frequent_attenders_adult_walk-in       aae           activity_avo~ Frequent ~
       7 frequent_attenders_child_ambulance     aae           activity_avo~ Frequent ~
       8 frequent_attenders_child_walk-in       aae           activity_avo~ Frequent ~
       9 left_before_seen_adult_ambulance       aae           activity_avo~ Patients ~
      10 left_before_seen_adult_walk-in         aae           activity_avo~ Patients ~
      # i 82 more rows

