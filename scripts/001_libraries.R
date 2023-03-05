if (!require("pacman")) {
  install.packages("pacman")

  library(pacman)
}

pacman::p_load(
  tidyverse,
  basedosdados,
  # tblhelpr,
  glue,
  styler,
  summarytools,
  lintr,
  Cairo,
  arrow,
  duckdb,
  fixest,
  stargazer,
  mice,
  # missMethods,
  # ranger,
  data.table,
  devtools,
  RcmdrMisc,
  psych,
  # did,
  did2s,
  tictoc
)
