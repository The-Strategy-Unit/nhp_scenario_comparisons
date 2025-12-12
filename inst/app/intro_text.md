## Introduction and purpose

Welcome to the New Hospital Programme (NHP) scenario comparison tool. This tool allows NHP schemes to compare their demand modelling scenarios to one another. This is based on the existing visualisation in the <a href="https://connect.strategyunitwm.nhs.uk/nhp/outputs/" target="_blank">main outputs app</a>, providing side-by-side versions of the visualisations and figures therein. The documentation for the demand model is available <a href=https://connect.strategyunitwm.nhs.uk/nhp/project_information/ target="_blank">here</a>. 

## Instructions

To use the app, select which scheme's results you wish to compare. (We do not allow comparisons between different schemes as we wish to ensure like-for-like comparisons). 

The "Select Scenario 1" drop down will auto-populate with the scenarios that have been ran for the chosen scheme.

Once you have chosen the first scenario, the "Select Scenario 2" drop down will populate itself with scenarios which are comparable (see guidance on scenario selections tab).

There are some instances of scenario names being duplicated so you can use the scenario runtime drop downs to ensure that you have the exact scenarios that you wish to compare.

## Content

We provide the following comparisons:

### Summary

This shows the high-level comparison of the two scenarios by point of delivery. There is a separate plot for each activity type, which can be toggled via a drop down. The relevant measure (e.g. admissions or bed days for inpatient activity) can be set too.

### Length of stay

This shows the number of admissions or the number of bed days for each scenario by length of stay (LoS) group. The point of delivery (elective, non-elective, maternity, day cases) can be set via a drop down. The length of stay groups are as follows:

- 0-day
- 1-7 days
- 8-14 days
- 15-21 days
- 22+ days

### Waterfall

This shows the cumulative impact of each of the major change factors in the outputs for each scenario. It helps to show how we get from baseline value to the principal projections, highlighting which change factors are adding activity and which are reducing activity. The activity type needs to be chosen as does the relevant measure.

### Activity Avoidance Impact

This summarises the reductions in types of potentially mitigatable activity (TPMA) due to assumptions made regarding activity avoidance. The TPMAs are ordered in descending scale of activity reduction. The activity type and measure need to be set via drop down.

### Efficiencies Impact

This is the same as the activity avoidance plot, but for efficiency TPMA assumptions. NB there are no inpatient admissions efficiency reductions by definition.

### P10-P90 Intervals

This shows the principal projection as well as the 10th and 90th percentiles (p10 and p90) of all the model runs for each point of delivery (chosen via drop down). 

### Beeswarm

This presents the distribution of model runs. Each dot represents an individual model run from the scenario. The thickness of the swarm indicates the density of runs in certain ranges. The activity type and measure to be set via drop down.

### ECDF

The curve shows the empirical (observed) cumulative distribution of all the model runs. The coloured dashed lines show the p10 and p90 of each scenario. The activity type and measure to be set via drop down.

## Note about bed days

Bed days are defined as the difference in days between discharge and admission, plus one day. One bed day is added to account for zero length of stay spells/partial days at the beginning and end of a spell. See the <a href=https://connect.strategyunitwm.nhs.uk/nhp/project_information/ target="_blank">model project information site</a> for definitions of terms.

## Feedback

We welcome any feedback on this tool as part of its continuous development. If you spot any issues or things you would like included, please let us know by providing feedback using the "Give feedback" button at the top of the app.
