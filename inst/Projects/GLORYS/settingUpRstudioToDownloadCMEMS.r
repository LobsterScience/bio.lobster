#cmems to R download

install.packages('reticulate')
require(reticulate)
reticulate::install_python()

virtualenv_create(envname = "CopernicusMarine")

virtualenv_install("CopernicusMarine", packages = c("copernicusmarine"))

reticulate::use_virtualenv("CopernicusMarine", required = TRUE)

cmt <- import("copernicusmarine")
cmt$login(cmems_uid, cmems_pwd)
