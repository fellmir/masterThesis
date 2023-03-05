#### libraries ----

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(
  tidyverse,
  glue,
  arrow,
  fixest,
  data.table,
  did2s,
  janitor,
  fs,
  tictoc,
  modelsummary
)

#### beginning ----

if (!fs::dir_exists("results")) fs::dir_create("results")
if (!fs::dir_exists("results/AP")) fs::dir_create("results/AP")
if (!fs::dir_exists("results/AP/dynamic")) fs::dir_create("results/AP/dynamic")
if (!fs::dir_exists("results/AP/dynamic/html")) fs::dir_create("results/AP/dynamic/html")

options(modelsummary_format_numeric_latex = "plain")

### log_avg_income_defl ----

# first model
model.01.tidy <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_log_avg_income_defl_in_0_AP.rds") %>% 
  broom::tidy()
gc()
model.01.glance <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_log_avg_income_defl_in_0_AP.rds") %>% 
  broom::glance()
gc()

# second model
model.02.tidy <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_log_avg_income_defl_in_race_id_gender_id_under_40_AP.rds") %>% 
  broom::tidy()
gc()
model.02.glance <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_log_avg_income_defl_in_race_id_gender_id_under_40_AP.rds") %>% 
  broom::glance()
gc()

# third model
model.03.tidy <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_log_avg_income_defl_in_race_id_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::tidy()
gc()
model.03.glance <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_log_avg_income_defl_in_race_id_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::glance()
gc()

# lists of tidy objects for modelsummaries
model.01 <- list(
  tidy = as.data.frame(model.01.tidy),
  glance = as.data.frame(model.01.glance)
)
model.02 <- list(
  tidy = as.data.frame(model.02.tidy),
  glance = as.data.frame(model.02.glance)
)
model.03 <- list(
  tidy = as.data.frame(model.03.tidy),
  glance = as.data.frame(model.03.glance)
)

# transforming objects into "modelsummary" class objects
class(model.01) <- c("modelsummary_list", class(model.01))
class(model.02) <- c("modelsummary_list", class(model.02))
class(model.03) <- c("modelsummary_list", class(model.03))

# checking if estimates are in place
modelsummary::get_estimates(model.01)
modelsummary::get_estimates(model.02)
modelsummary::get_estimates(model.03)

# notes to appear below the reg. table
notesAP <- "Notes: standard errors clustered by AP"

# image output
modelsummary::modelsummary(list(model.01, model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo|Log"), title = "Dependent variable: Log Average Income (deflated) -- second-stage estimation, AP level", notes = notesAP)

# txt "sinking"
sink("results/AP/dynamic/log_avg_income_2nd_AP.txt")

modelsummary::modelsummary(list(model.01, model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo|Log"), title = "Dependent variable: Log Average Income (deflated) -- second-stage estimation, AP level", notes = notesAP, output = "latex")

sink()

rm(list = ls())

gc()


### vinculo_ativo_3112 ----

# first model
model.01.tidy <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_vinculo_ativo_3112_in_0_AP.rds") %>% 
  broom::tidy()
gc()
model.01.glance <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_vinculo_ativo_3112_in_0_AP.rds") %>% 
  broom::glance()
gc()

# second model
model.02.tidy <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_vinculo_ativo_3112_in_race_id_gender_id_under_40_AP.rds") %>% 
  broom::tidy()
gc()
model.02.glance <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_vinculo_ativo_3112_in_race_id_gender_id_under_40_AP.rds") %>% 
  broom::glance()
gc()

# third model
model.03.tidy <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_vinculo_ativo_3112_in_race_id_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::tidy()
gc()
model.03.glance <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_vinculo_ativo_3112_in_race_id_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::glance()
gc()

# lists of tidy objects for modelsummaries
model.01 <- list(
  tidy = as.data.frame(model.01.tidy),
  glance = as.data.frame(model.01.glance)
)
model.02 <- list(
  tidy = as.data.frame(model.02.tidy),
  glance = as.data.frame(model.02.glance)
)
model.03 <- list(
  tidy = as.data.frame(model.03.tidy),
  glance = as.data.frame(model.03.glance)
)

# transforming objects into "modelsummary" class objects
class(model.01) <- c("modelsummary_list", class(model.01))
class(model.02) <- c("modelsummary_list", class(model.02))
class(model.03) <- c("modelsummary_list", class(model.03))

# checking if estimates are in place
modelsummary::get_estimates(model.01)
modelsummary::get_estimates(model.02)
modelsummary::get_estimates(model.03)

# notes to appear below the reg. table
notesAP <- "Notes: standard errors clustered by AP"

# image output
modelsummary::modelsummary(list(model.01, model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Still at work by end of the year (Y/n) -- second-stage estimation, AP level", notes = notesAP)

# txt "sinking"
sink("results/AP/dynamic/vinculo_ativo_3112_2nd_AP.txt")

modelsummary::modelsummary(list(model.01, model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Still at work by end of the year (Y/n) -- second-stage estimation, AP level", notes = notesAP, output = "latex")

sink()

rm(list = ls())

gc()


### indicador_simples ----

# first model
model.01.tidy <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_indicador_simples_in_0_AP.rds") %>% 
  broom::tidy()
gc()
model.01.glance <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_indicador_simples_in_0_AP.rds") %>% 
  broom::glance()
gc()

# second model
model.02.tidy <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_indicador_simples_in_race_id_gender_id_under_40_AP.rds") %>% 
  broom::tidy()
gc()
model.02.glance <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_indicador_simples_in_race_id_gender_id_under_40_AP.rds") %>% 
  broom::glance()
gc()

# third model
model.03.tidy <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_indicador_simples_in_race_id_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::tidy()
gc()
model.03.glance <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_indicador_simples_in_race_id_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::glance()
gc()

# lists of tidy objects for modelsummaries
model.01 <- list(
  tidy = as.data.frame(model.01.tidy),
  glance = as.data.frame(model.01.glance)
)
model.02 <- list(
  tidy = as.data.frame(model.02.tidy),
  glance = as.data.frame(model.02.glance)
)
model.03 <- list(
  tidy = as.data.frame(model.03.tidy),
  glance = as.data.frame(model.03.glance)
)

# transforming objects into "modelsummary" class objects
class(model.01) <- c("modelsummary_list", class(model.01))
class(model.02) <- c("modelsummary_list", class(model.02))
class(model.03) <- c("modelsummary_list", class(model.03))

# checking if estimates are in place
modelsummary::get_estimates(model.01)
modelsummary::get_estimates(model.02)
modelsummary::get_estimates(model.03)

# notes to appear below the reg. table
notesAP <- "Notes: standard errors clustered by AP"

# image output
modelsummary::modelsummary(list(model.01, model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Worker at \"Simples Nacional\" company (Y/n) -- second-stage estimation, AP level", notes = notesAP)

# txt "sinking"
sink("results/AP/dynamic/indicador_simples_2nd_AP.txt")

modelsummary::modelsummary(list(model.01, model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Worker at \"Simples Nacional\" company (Y/n) -- second-stage estimation, AP level", notes = notesAP, output = "latex")

sink()

rm(list = ls())

gc()


### q1_id ----

# first model
model.01.tidy <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_q1_id_in_0_AP.rds") %>% 
  broom::tidy()
gc()
model.01.glance <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_q1_id_in_0_AP.rds") %>% 
  broom::glance()
gc()

# second model
model.02.tidy <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_q1_id_in_race_id_gender_id_under_40_AP.rds") %>% 
  broom::tidy()
gc()
model.02.glance <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_q1_id_in_race_id_gender_id_under_40_AP.rds") %>% 
  broom::glance()
gc()

# third model
model.03.tidy <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_q1_id_in_race_id_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::tidy()
gc()
model.03.glance <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_q1_id_in_race_id_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::glance()
gc()

# lists of tidy objects for modelsummaries
model.01 <- list(
  tidy = as.data.frame(model.01.tidy),
  glance = as.data.frame(model.01.glance)
)
model.02 <- list(
  tidy = as.data.frame(model.02.tidy),
  glance = as.data.frame(model.02.glance)
)
model.03 <- list(
  tidy = as.data.frame(model.03.tidy),
  glance = as.data.frame(model.03.glance)
)

# transforming objects into "modelsummary" class objects
class(model.01) <- c("modelsummary_list", class(model.01))
class(model.02) <- c("modelsummary_list", class(model.02))
class(model.03) <- c("modelsummary_list", class(model.03))

# checking if estimates are in place
modelsummary::get_estimates(model.01)
modelsummary::get_estimates(model.02)
modelsummary::get_estimates(model.03)

# notes to appear below the reg. table
notesAP <- "Notes: standard errors clustered by AP"

# image output
modelsummary::modelsummary(list(model.01, model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Worker earning under 2 minimum wages/month (Y/n) -- second-stage estimation, AP level", notes = notesAP)

# txt "sinking"
sink("results/AP/dynamic/q1_id_2nd_AP.txt")

modelsummary::modelsummary(list(model.01, model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Worker earning under 2 minimum wages/month (Y/n) -- second-stage estimation, AP level", notes = notesAP, output = "latex")

sink()

rm(list = ls())

gc()


### gender_id ----

# first model
model.01.tidy <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_gender_id_in_0_AP.rds") %>% 
  broom::tidy()
gc()
model.01.glance <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_gender_id_in_0_AP.rds") %>% 
  broom::glance()
gc()

# second model
model.02.tidy <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_gender_id_in_race_id_under_40_AP.rds") %>%
  broom::tidy()
gc()
model.02.glance <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_gender_id_in_race_id_under_40_AP.rds") %>%
  broom::glance()
gc()

# third model
model.03.tidy <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_gender_id_in_race_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::tidy()
gc()
model.03.glance <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_gender_id_in_race_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::glance()
gc()

# lists of tidy objects for modelsummaries
model.01 <- list(
  tidy = as.data.frame(model.01.tidy),
  glance = as.data.frame(model.01.glance)
)
model.02 <- list(
  tidy = as.data.frame(model.02.tidy),
  glance = as.data.frame(model.02.glance)
)
model.03 <- list(
  tidy = as.data.frame(model.03.tidy),
  glance = as.data.frame(model.03.glance)
)

# transforming objects into "modelsummary" class objects
class(model.01) <- c("modelsummary_list", class(model.01))
class(model.02) <- c("modelsummary_list", class(model.02))
class(model.03) <- c("modelsummary_list", class(model.03))

# checking if estimates are in place
modelsummary::get_estimates(model.01)
modelsummary::get_estimates(model.02)
modelsummary::get_estimates(model.03)

# notes to appear below the reg. table
notesAP <- "Notes: standard errors clustered by AP"

# image output
modelsummary::modelsummary(list(model.01, model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Female worker (Y/n) -- second-stage estimation, AP level", notes = notesAP)

# txt "sinking"
sink("results/AP/dynamic/gender_id_2nd_AP.txt")

modelsummary::modelsummary(list(model.01, model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Female worker (Y/n) -- second-stage estimation, AP level", notes = notesAP, output = "latex")

sink()

rm(list = ls())

gc()


### race_id ----

# first model
model.01.tidy <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_race_id_in_0_AP.rds") %>% 
  broom::tidy()
gc()
model.01.glance <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_race_id_in_0_AP.rds") %>% 
  broom::glance()
gc()

# second model
model.02.tidy <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_race_id_in_gender_id_under_40_AP.rds") %>%
  broom::tidy()
gc()
model.02.glance <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_race_id_in_gender_id_under_40_AP.rds") %>%
  broom::glance()
gc()

# third model
model.03.tidy <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_race_id_in_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::tidy()
gc()
model.03.glance <- readRDS("outputs/AP/dynamic/2nd/dynamic_2nd_race_id_in_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::glance()
gc()

# lists of tidy objects for modelsummaries
model.01 <- list(
  tidy = as.data.frame(model.01.tidy),
  glance = as.data.frame(model.01.glance)
)
model.02 <- list(
  tidy = as.data.frame(model.02.tidy),
  glance = as.data.frame(model.02.glance)
)
model.03 <- list(
  tidy = as.data.frame(model.03.tidy),
  glance = as.data.frame(model.03.glance)
)

# transforming objects into "modelsummary" class objects
class(model.01) <- c("modelsummary_list", class(model.01))
class(model.02) <- c("modelsummary_list", class(model.02))
class(model.03) <- c("modelsummary_list", class(model.03))

# checking if estimates are in place
modelsummary::get_estimates(model.01)
modelsummary::get_estimates(model.02)
modelsummary::get_estimates(model.03)

# notes to appear below the reg. table
notesAP <- "Notes: standard errors clustered by AP"

# image output
modelsummary::modelsummary(list(model.01, model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Non-white worker (Y/n) -- second-stage estimation, AP level", notes = notesAP)

# txt "sinking"
sink("results/AP/dynamic/race_id_2nd_AP.txt")

modelsummary::modelsummary(list(model.01, model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Non-white worker (Y/n) -- second-stage estimation, AP level", notes = notesAP, output = "latex")

sink()

rm(list = ls())

gc()


