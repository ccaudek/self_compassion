library("targets")
library("tarchetypes")
library("tibble")

suppressPackageStartupMessages(library("brms"))

# By default, R uses polynomial contrasts for ordered factors in linear models
# options("contrasts")
# Make ordered factors use treatment contrasts instead
options(contrasts = rep("contr.treatment", 2))
# Or do it on a single variable:
# contrasts(df$x) <- "contr.treatment"

set.seed(9936)  # From random.org

# Bayes-specific stuff
options(
  mc.cores = parallel::detectCores(),
  brms.backend = "cmdstanr"
)

options(
  tidyverse.quiet = TRUE,
  dplyr.summarise.inform = FALSE
)

future::plan(future::multisession)

tar_option_set(packages = c(
  "tidyverse", "here", "readxl", "naniar", "janitor", "kableExtra", 
  "knitr", "brms", "tidybayes", "broom", "broom.mixed", "cmdstanr"
  )
)


