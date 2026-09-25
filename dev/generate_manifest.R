# Run this file any time the packages are changed; else leave it.

files <- c(
  "DESCRIPTION",
  "NAMESPACE",
  "app.R",
  fs::dir_ls("R"),
  fs::dir_ls("inst", recurse = TRUE, type = "file")
)

rsconnect::writeManifest(appFiles = files)
