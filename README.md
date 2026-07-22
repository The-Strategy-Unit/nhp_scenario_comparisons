# Analyse and compare NHP schemes' D&C scenarios

## Purpose

Compare New Hospital Programme (NHP) schemes' demand modelling scenarios' results in the horizon year. This is primarily for a given scheme to compare several of its own scenarios to each other, to enable them to iteratively arrive at a final scenario. The scenarios are based on the NHP [demand model](https://github.com/the-Strategy-Unit/nhp_project_information).

The app is [deployed to Posit Connect](https://connect.strategyunitwm.nhs.uk/scenario_comparison/) (login/permissions required).

## For developers

### Tools

The app is built primarily with the R packages [{shiny}](https://shiny.posit.co/) and [{bslib}](https://rstudio.github.io/bslib/). It is based on the [{nolem} structure](https://github.com/statsRhian/nolem/tree/main).

### Run the app locally

#### Install packages

You must ensure you have installed the packages listed in the `DESCRIPTION`.
These can be installed with `pak::local_install_deps(dependencies = TRUE)`.
This repo doesn't use {renv}.

#### Add environmental variables

You must set some environmental variables before you can run the app locally on your machine.
To do this, add an `.Renviron` file to the project directory that contains the variables named in the `.Renviron.sample` file.
You can ask a member of the Data Science team to help populate this file.

Remember to restart your session or run `readRenviron(".Renviron")` after you've updated your `.Renviron` file.
If you're having authorisation issues (e.g. a 403 is being returned), try clearing your tokens with `AzureAuth::clean_token_directory()` and try again.

#### Run the app

Run the `app.R` script to run the app, assuming you've installed the packages and set up the environment variables.

### Deploy

Run the `dev/deploy.R` script to deploy to connect, ensuring the relevant `appId` is in the code to deploy to either dev or prod.
The appID is available from the existing Posit Connect published app from the Settings menu > Info and is called `Content ID`

### Data

#### Model runs

All the model runs are stored within Azure. 
You will need an `account@mlcsu.nhs.uk` to access these and have been granted permission to access these. 
The `.Renviron` variables specify the exact credentials.

Also ensure that you are logged into the `mlcsu` account as it may be that you are automatically logged into the `nhs.net`.
To ensure this is logged in you can open the [Azure Portal](https://portal.azure.com/auth/login/) where the log in box will prompt for choosing which account to use.
If only one account shows you should also be able to add Microsoft accounts.

You will be required to confirm the account multiple times when first rendering the plots on the app as these relate to the data sources.

#### Supporting_data

- `tx_lookup.json`
- `datasets.json`
- `golem-config.yml`
- `scheme-lookup.csv`




