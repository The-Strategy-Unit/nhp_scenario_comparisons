AzureRMR::create_azure_login()
app <- AzureRMR::get_azure_login()$token$client$client_id
AzureAuth::get_azure_token(
  resource = "https://storage.azure.com/",
  tenant = "common",
  app = app
)
pkgload::load_all(".")
run_app()
