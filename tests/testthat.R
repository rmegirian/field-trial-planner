library(testthat)

# The app is a plain Shiny project rather than a package, so the R/ files are
# sourced directly. Run from the project root with:
#   Rscript tests/testthat.R
for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)

test_dir("tests/testthat", stop_on_failure = TRUE)
