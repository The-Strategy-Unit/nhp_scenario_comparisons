# nhp_scenario_analysis  
This repo provides code for comparing a given New Hospital Programme (NHP) scheme's Demand and Capacity (D&C) Model runs to one another.

A scheme might have runs where they have used the principal non-demographic growth (NDG) variant and another where they have used the second variant.

And there may be high mitigation / low mitigation scenarios.

The latter is of more interest given that that the activity mitigators form the vast majority of the D&C Model inputs that are set locally.

We consider primarily the principal projection, and look at the impacts on demand by activity type (inpatients, outpatients, A&E), pod (e.g. elective, non-elective, maternity, outpatient, ...), measure (e.g. admissions, attendances, bed days, ...). 

In the case of activity mitigators, we also split these by whether they are activity avoidance or efficiency gains.
