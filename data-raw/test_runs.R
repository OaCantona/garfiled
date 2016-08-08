library(garfiled)

test_run_12km <- read_tcx_run("data-raw/1289212841.tcx")
devtools::use_data(test_run_12km, overwrite = TRUE)
