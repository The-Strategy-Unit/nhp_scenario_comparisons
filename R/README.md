# `R/`

The main `populate-template.R` script in the root sources its functions from this directory. That includes:

* `azure.R`, which contains functions to fetch data from Azure and perform some wrangling
* `generate-figures.R`, which creates charts and tables and writes them
* `generate-values.R`, which generates all the calculated values to be inserted into the body of the template document
* `insert-into-template.R`, which has functions to insert all the elements into the template and save it
* the `nhp_outputs/` directory, which contains functions copied/adapted from the [nhp_outputs repo](https://github.com/The-Strategy-Unit/nhp_outputs) that handle data preparation and chart generation
