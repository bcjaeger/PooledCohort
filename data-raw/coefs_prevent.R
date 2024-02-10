

coefs_prevent <- list(
  base_10 = readxl::read_xlsx("data-raw/coefs_prevent.xlsx", sheet = 'base_10'),
  acr_10 = readxl::read_xlsx("data-raw/coefs_prevent.xlsx", sheet = 'acr_10'),
  hba1c_10 = readxl::read_xlsx("data-raw/coefs_prevent.xlsx", sheet = 'hba1c_10'),
  sdi_10 = readxl::read_xlsx("data-raw/coefs_prevent.xlsx", sheet = 'sdi_10'),
  full_10 = readxl::read_xlsx("data-raw/coefs_prevent.xlsx", sheet = 'full_10')
)

usethis::use_data(coefs_prevent, internal = TRUE, overwrite = TRUE)
