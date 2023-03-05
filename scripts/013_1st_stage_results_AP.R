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

coef.order <- c(
  "race_id",
  "gender_id",
  "under_40",
  "public_work",
  "educ_id1",
  "educ_id2",
  "educ_id3",
  "educ_id4",
  "educ_id5",
  "educ_id6",
  "educ_id7",
  "educ_id8",
  "educ_id9",
  "educ_id10",
  "educ_id11",
  "tamanho_estabelecimento1",
  "tamanho_estabelecimento2",
  "tamanho_estabelecimento3",
  "tamanho_estabelecimento4",
  "tamanho_estabelecimento5",
  "tamanho_estabelecimento6",
  "tamanho_estabelecimento7",
  "tamanho_estabelecimento8",
  "tamanho_estabelecimento9",
  "tamanho_estabelecimento10"
)

### log_avg_income_defl ----

# first model
# model.01.tidy <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_log_avg_income_defl_in_0_AP.rds") %>% 
#   broom::tidy()
# gc()
# model.01.glance <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_log_avg_income_defl_in_0_AP.rds") %>% 
#   broom::glance()
# gc()

# second model
model.02.tidy <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_log_avg_income_defl_in_race_id_gender_id_under_40_AP.rds") %>% 
  broom::tidy()
gc()
model.02.glance <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_log_avg_income_defl_in_race_id_gender_id_under_40_AP.rds") %>% 
  broom::glance()
gc()

# third model
model.03.tidy <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_log_avg_income_defl_in_race_id_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::tidy()
gc()
model.03.glance <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_log_avg_income_defl_in_race_id_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::glance()
gc()

# lists of tidy objects for modelsummaries
# model.01 <- list(
#   tidy = as.data.frame(model.01.tidy),
#   glance = as.data.frame(model.01.glance)
# )
model.02 <- list(
  tidy = as.data.frame(model.02.tidy),
  glance = as.data.frame(model.02.glance)
)
model.03 <- list(
  tidy = as.data.frame(model.03.tidy),
  glance = as.data.frame(model.03.glance)
)

# transforming objects into "modelsummary" class objects
# class(model.01) <- c("modelsummary_list", class(model.01))
class(model.02) <- c("modelsummary_list", class(model.02))
class(model.03) <- c("modelsummary_list", class(model.03))

# checking if estimates are in place
# modelsummary::get_estimates(model.01)
modelsummary::get_estimates(model.02)
modelsummary::get_estimates(model.03)

# notes to appear below the reg. table
notesAP <- "Notes: standard errors clustered by AP"

# image output
modelsummary::modelsummary(list(model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo|Log"), title = "Dependent variable: Log Average Income (deflated) -- first-stage estimation, AP level", notes = notesAP, coef_map = coef.order)

# txt "sinking"
sink("results/AP/dynamic/log_avg_income_1st_AP.txt")

modelsummary::modelsummary(list(model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo|Log"), title = "Dependent variable: Log Average Income (deflated) -- first-stage estimation, AP level", notes = notesAP, coef_map = coef.order, output = "latex")

sink()

rm(
  model.02,
  model.02.glance,
  model.02.tidy,
  model.03,
  model.03.glance,
  model.03.tidy,
  notesAP
)

gc()


### vinculo_ativo_3112 ----

# first model
# model.01.tidy <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_vinculo_ativo_3112_in_0_AP.rds") %>% 
#   broom::tidy()
# gc()
# model.01.glance <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_vinculo_ativo_3112_in_0_AP.rds") %>% 
#   broom::glance()
# gc()

# second model
model.02.tidy <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_vinculo_ativo_3112_in_race_id_gender_id_under_40_AP.rds") %>% 
  broom::tidy()
gc()
model.02.glance <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_vinculo_ativo_3112_in_race_id_gender_id_under_40_AP.rds") %>% 
  broom::glance()
gc()

# third model
model.03.tidy <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_vinculo_ativo_3112_in_race_id_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::tidy()
gc()
model.03.glance <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_vinculo_ativo_3112_in_race_id_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::glance()
gc()

# lists of tidy objects for modelsummaries
# model.01 <- list(
#   tidy = as.data.frame(model.01.tidy),
#   glance = as.data.frame(model.01.glance)
# )
model.02 <- list(
  tidy = as.data.frame(model.02.tidy),
  glance = as.data.frame(model.02.glance)
)
model.03 <- list(
  tidy = as.data.frame(model.03.tidy),
  glance = as.data.frame(model.03.glance)
)

# transforming objects into "modelsummary" class objects
# class(model.01) <- c("modelsummary_list", class(model.01))
class(model.02) <- c("modelsummary_list", class(model.02))
class(model.03) <- c("modelsummary_list", class(model.03))

# checking if estimates are in place
# modelsummary::get_estimates(model.01)
modelsummary::get_estimates(model.02)
modelsummary::get_estimates(model.03)

# notes to appear below the reg. table
notesAP <- "Notes: standard errors clustered by AP"

# image output
modelsummary::modelsummary(list(model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Still at work by end of the year (Y/n) -- first-stage estimation, AP level", notes = notesAP, coef_map = coef.order)

# txt "sinking"
sink("results/AP/dynamic/vinculo_ativo_3112_1st_AP.txt")

modelsummary::modelsummary(list(model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Still at work by end of the year (Y/n) -- first-stage estimation, AP level", notes = notesAP, coef_map = coef.order, output = "latex")

sink()

rm(
  model.02,
  model.02.glance,
  model.02.tidy,
  model.03,
  model.03.glance,
  model.03.tidy,
  notesAP
)

gc()


### indicador_simples ----

# first model
# model.01.tidy <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_indicador_simples_in_0_AP.rds") %>% 
#   broom::tidy()
# gc()
# model.01.glance <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_indicador_simples_in_0_AP.rds") %>% 
#   broom::glance()
# gc()

# second model
model.02.tidy <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_indicador_simples_in_race_id_gender_id_under_40_AP.rds") %>% 
  broom::tidy()
gc()
model.02.glance <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_indicador_simples_in_race_id_gender_id_under_40_AP.rds") %>% 
  broom::glance()
gc()

# third model
model.03.tidy <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_indicador_simples_in_race_id_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::tidy()
gc()
model.03.glance <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_indicador_simples_in_race_id_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::glance()
gc()

# lists of tidy objects for modelsummaries
# model.01 <- list(
#   tidy = as.data.frame(model.01.tidy),
#   glance = as.data.frame(model.01.glance)
# )
model.02 <- list(
  tidy = as.data.frame(model.02.tidy),
  glance = as.data.frame(model.02.glance)
)
model.03 <- list(
  tidy = as.data.frame(model.03.tidy),
  glance = as.data.frame(model.03.glance)
)

# transforming objects into "modelsummary" class objects
# class(model.01) <- c("modelsummary_list", class(model.01))
class(model.02) <- c("modelsummary_list", class(model.02))
class(model.03) <- c("modelsummary_list", class(model.03))

# checking if estimates are in place
# modelsummary::get_estimates(model.01)
modelsummary::get_estimates(model.02)
modelsummary::get_estimates(model.03)

# notes to appear below the reg. table
notesAP <- "Notes: standard errors clustered by AP"

# image output
modelsummary::modelsummary(list(model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Worker at \"Simples Nacional\" company (Y/n) -- first-stage estimation, AP level", notes = notesAP, coef_map = coef.order)

# txt "sinking"
sink("results/AP/dynamic/indicador_simples_1st_AP.txt")

modelsummary::modelsummary(list(model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Worker at \"Simples Nacional\" company (Y/n) -- first-stage estimation, AP level", notes = notesAP, coef_map = coef.order, output = "latex")

sink()

rm(
  model.02,
  model.02.glance,
  model.02.tidy,
  model.03,
  model.03.glance,
  model.03.tidy,
  notesAP
)

gc()


### q1_id ----

# first model
# model.01.tidy <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_q1_id_in_0_AP.rds") %>% 
#   broom::tidy()
# gc()
# model.01.glance <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_q1_id_in_0_AP.rds") %>% 
#   broom::glance()
# gc()

# second model
model.02.tidy <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_q1_id_in_race_id_gender_id_under_40_AP.rds") %>% 
  broom::tidy()
gc()
model.02.glance <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_q1_id_in_race_id_gender_id_under_40_AP.rds") %>% 
  broom::glance()
gc()

# third model
model.03.tidy <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_q1_id_in_race_id_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::tidy()
gc()
model.03.glance <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_q1_id_in_race_id_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::glance()
gc()

# lists of tidy objects for modelsummaries
# model.01 <- list(
#   tidy = as.data.frame(model.01.tidy),
#   glance = as.data.frame(model.01.glance)
# )
model.02 <- list(
  tidy = as.data.frame(model.02.tidy),
  glance = as.data.frame(model.02.glance)
)
model.03 <- list(
  tidy = as.data.frame(model.03.tidy),
  glance = as.data.frame(model.03.glance)
)

# transforming objects into "modelsummary" class objects
# class(model.01) <- c("modelsummary_list", class(model.01))
class(model.02) <- c("modelsummary_list", class(model.02))
class(model.03) <- c("modelsummary_list", class(model.03))

# checking if estimates are in place
# modelsummary::get_estimates(model.01)
modelsummary::get_estimates(model.02)
modelsummary::get_estimates(model.03)

# notes to appear below the reg. table
notesAP <- "Notes: standard errors clustered by AP"

# image output
modelsummary::modelsummary(list(model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Worker earning under 2 minimum wages/month (Y/n) -- first-stage estimation, AP level", notes = notesAP, coef_map = coef.order)

# txt "sinking"
sink("results/AP/dynamic/q1_id_1st_AP.txt")

modelsummary::modelsummary(list(model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Worker earning under 2 minimum wages/month (Y/n) -- first-stage estimation, AP level", notes = notesAP, coef_map = coef.order, output = "latex")

sink()

rm(
  model.02,
  model.02.glance,
  model.02.tidy,
  model.03,
  model.03.glance,
  model.03.tidy,
  notesAP
)

gc()


### gender_id ----

# first model
# model.01.tidy <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_gender_id_in_0_AP.rds") %>% 
#   broom::tidy()
# gc()
# model.01.glance <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_gender_id_in_0_AP.rds") %>% 
#   broom::glance()
# gc()

# second model
model.02.tidy <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_gender_id_in_race_id_under_40_AP.rds") %>%
  broom::tidy()
gc()
model.02.glance <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_gender_id_in_race_id_under_40_AP.rds") %>%
  broom::glance()
gc()

# third model
model.03.tidy <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_gender_id_in_race_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::tidy()
gc()
model.03.glance <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_gender_id_in_race_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::glance()
gc()

# lists of tidy objects for modelsummaries
# model.01 <- list(
#   tidy = as.data.frame(model.01.tidy),
#   glance = as.data.frame(model.01.glance)
# )
model.02 <- list(
  tidy = as.data.frame(model.02.tidy),
  glance = as.data.frame(model.02.glance)
)
model.03 <- list(
  tidy = as.data.frame(model.03.tidy),
  glance = as.data.frame(model.03.glance)
)

# transforming objects into "modelsummary" class objects
# class(model.01) <- c("modelsummary_list", class(model.01))
class(model.02) <- c("modelsummary_list", class(model.02))
class(model.03) <- c("modelsummary_list", class(model.03))

# checking if estimates are in place
# modelsummary::get_estimates(model.01)
modelsummary::get_estimates(model.02)
modelsummary::get_estimates(model.03)

# notes to appear below the reg. table
notesAP <- "Notes: standard errors clustered by AP"

# image output
modelsummary::modelsummary(list(model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Female worker (Y/n) -- first-stage estimation, AP level", notes = notesAP, coef_map = coef.order)

# txt "sinking"
sink("results/AP/dynamic/gender_id_1st_AP.txt")

modelsummary::modelsummary(list(model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Female worker (Y/n) -- first-stage estimation, AP level", notes = notesAP, coef_map = coef.order, output = "latex")

sink()

rm(
  model.02,
  model.02.glance,
  model.02.tidy,
  model.03,
  model.03.glance,
  model.03.tidy,
  notesAP
)

gc()


### race_id ----

# first model
# model.01.tidy <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_race_id_in_0_AP.rds") %>% 
#   broom::tidy()
# gc()
# model.01.glance <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_race_id_in_0_AP.rds") %>% 
#   broom::glance()
# gc()

# second model
model.02.tidy <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_race_id_in_gender_id_under_40_AP.rds") %>%
  broom::tidy()
gc()
model.02.glance <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_race_id_in_gender_id_under_40_AP.rds") %>%
  broom::glance()
gc()

# third model
model.03.tidy <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_race_id_in_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::tidy()
gc()
model.03.glance <- readRDS("outputs/AP/dynamic/1st/dynamic_1st_race_id_in_gender_id_under_40_tamanho_estabelecimento_educ_id_public_work_AP.rds") %>% 
  broom::glance()
gc()

# lists of tidy objects for modelsummaries
# model.01 <- list(
#   tidy = as.data.frame(model.01.tidy),
#   glance = as.data.frame(model.01.glance)
# )
model.02 <- list(
  tidy = as.data.frame(model.02.tidy),
  glance = as.data.frame(model.02.glance)
)
model.03 <- list(
  tidy = as.data.frame(model.03.tidy),
  glance = as.data.frame(model.03.glance)
)

# transforming objects into "modelsummary" class objects
# class(model.01) <- c("modelsummary_list", class(model.01))
class(model.02) <- c("modelsummary_list", class(model.02))
class(model.03) <- c("modelsummary_list", class(model.03))

# checking if estimates are in place
# modelsummary::get_estimates(model.01)
modelsummary::get_estimates(model.02)
modelsummary::get_estimates(model.03)

# notes to appear below the reg. table
notesAP <- "Notes: standard errors clustered by AP"

# image output
modelsummary::modelsummary(list(model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Non-white worker (Y/n) -- first-stage estimation, AP level", notes = notesAP, coef_map = coef.order)

# txt "sinking"
sink("results/AP/dynamic/race_id_1st_AP.txt")

modelsummary::modelsummary(list(model.02, model.03), stars = TRUE, gof_omit = c("IC|Pseudo"), title = "Dependent variable: Non-white worker (Y/n) -- first-stage estimation, AP level", notes = notesAP, coef_map = coef.order, output = "latex")

sink()

rm(
  model.02,
  model.02.glance,
  model.02.tidy,
  model.03,
  model.03.glance,
  model.03.tidy,
  notesAP
)

gc()


