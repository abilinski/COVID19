library(rmarkdown)
library(devtools)

load_all() # load the covid.epi package

# Rmarkdown Configuration Settings
 
# local level specific parameters
params <- list(
  locality = "Santa Clara County",
  DoublingTime = 2.5
  # ... 
)

# Output File Path
output_file <- "./example_local_level_report.docx"

# Render the document
rmarkdown::render(
  # system.file gets the path to the rmarkdown from inside the package
  system.file("rmd/local_level_report.rmd", package = 'covid.epi'), 
  output_format = 'word_document',
  output_file = output_file,
  params = params
  )

