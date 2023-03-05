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
  tictoc
)

#### beginning ----

if (!fs::dir_exists("outputs")) fs::dir_create("outputs")
if (!fs::dir_exists("outputs/RP")) fs::dir_create("outputs/RP")
if (!fs::dir_exists("outputs/RP/static")) fs::dir_create("outputs/RP/static")
if (!fs::dir_exists("outputs/RP/dynamic")) fs::dir_create("outputs/RP/dynamic")

### log_avg_income_defl ----

tictoc::tic()

models <- list(
  0,
  c("race_id", "gender_id"),
  c("race_id", "gender_id", "idade", "tamanho_estabelecimento", "educ_id", "public_work")
)

purrr::walk(
  models,
  function(i) {
    dep_vars <- c("avg_income_defl")
    
    fixed_vars <- c("ano", "RP", "treat_RP", "post", "idade")
    
    if (i[1] == 0) {
      selected_vars <- c(dep_vars, fixed_vars)
    } else {
      selected_vars <- c(i, dep_vars, fixed_vars)
    }
    
    cat("\n\n =============== Importing data...=============== \n\n")
    
    data <- fs::dir_ls("db/", regexp = "rais(.*)\\.parquet") %>%
      arrow::open_dataset() %>%
      dplyr::select(dplyr::all_of(selected_vars)) %>%
      dplyr::filter(ano >= 2001 & ano <= 2013) %>%
      dplyr::filter(idade >= 23 & idade <= 65) %>%
      dplyr::filter(avg_income_defl > 0) %>%
      dplyr::mutate(treat_RP_post = ifelse(treat_RP == 1 & post == 1, 1, 0)) %>%
      dplyr::mutate(log_avg_income_defl = log(avg_income_defl)) %>%
      dplyr::select(-c(avg_income_defl, treat_RP)) %>%
      dplyr::collect() %>%
      dplyr::group_by(ano) %>%
      # dplyr::slice_sample(n = 100) %>%
      data.table::as.data.table()
    
    untreat <- data[treat_RP_post == 0]
    
    dep_vars[1] <- "log_avg_income_defl"
    
    gc()
    
    purrr::walk(
      dep_vars,
      function(j) {
        if (length(i) == 1) {
          first_stage_fml <- glue::glue("{j} ~ {i} | {fixed_vars[1]} + {fixed_vars[2]}")
          
          name_file <- glue::glue("outputs/RP/static/static_{j}_in_{i}_RP.rds")
        } else {
          k <- stringr::str_flatten(i, collapse = " + ")
          
          first_stage_fml <- glue::glue("{j} ~ 0 + {k} | {fixed_vars[1]} + {fixed_vars[2]}")
          
          name_file <- glue::glue("outputs/RP/static/static_{j}_in_{janitor::make_clean_names(k)}_RP.rds")
        }
        
        cat(glue::glue("\n\n=============== MODEL: {first_stage_fml} ===============\n\n"))
        
        second_stage_fml <- glue::glue("{j} ~ 0 + fixest::i({fixed_vars[4]}, ref = FALSE)")
        
        cat("\n++++++++++ Running first stage model... ++++++++++\n")
        
        first_stage_reg <- fixest::feols(
          as.formula(first_stage_fml),
          data = untreat,
          warn = FALSE,
          notes = FALSE,
          nthreads = 8,
          cluster = "RP"
        )
        
        gc()
        
        cat("\n++++++++++ Setting errors as dependent variable... ++++++++++\n")
        
        first_stage_reg_u <- data[[j]] - stats::predict(first_stage_reg, newdata = data)
        
        data[[j]] <- first_stage_reg_u
        
        gc()
        
        cat("\n++++++++++ Running second stage model ...++++++++++\n")
        
        second_stage_reg <- fixest::feols(
          as.formula(second_stage_fml),
          data = data,
          warn = FALSE,
          notes = FALSE,
          nthreads = 8,
          cluster = "RP"
        )
        
        gc()
        
        cat("\n++++++++++ Exporting models... ++++++++++\n")
        
        models <- list(
          first_stage = first_stage_reg,
          second_stage = second_stage_reg
        )
        
        readr::write_rds(models, name_file)
        
        gc()
        
        cat("\n\n=============== DONE ===============\n\n")
      }
    )
    
    gc()
    
  }
)

gc()

tictoc::toc()

### vinculo_ativo_3112 ----

tictoc::tic()

models <- list(
  0,
  c("race_id", "gender_id"),
  c("race_id", "gender_id", "idade", "tamanho_estabelecimento", "educ_id", "public_work")
)

purrr::walk(
  models,
  function(i) {
    dep_vars <- c("vinculo_ativo_3112")
    
    fixed_vars <- c("ano", "RP", "treat_RP", "post", "idade")
    
    if (i[1] == 0) {
      selected_vars <- c(dep_vars, fixed_vars)
    } else {
      selected_vars <- c(i, dep_vars, fixed_vars)
    }
    
    cat("\n\n =============== Importing data...=============== \n\n")
    
    data <- fs::dir_ls("db/", regexp = "rais(.*)\\.parquet") %>%
      arrow::open_dataset() %>%
      dplyr::select(dplyr::all_of(selected_vars)) %>%
      dplyr::filter(ano >= 2001 & ano <= 2013) %>%
      dplyr::filter(idade >= 23 & idade <= 65) %>%
      # dplyr::filter(avg_income_defl > 0) %>%
      dplyr::mutate(treat_RP_post = ifelse(treat_RP == 1 & post == 1, 1, 0)) %>%
      # dplyr::mutate(log_avg_income_defl = log(avg_income_defl)) %>%
      dplyr::select(-c(treat_RP)) %>%
      dplyr::collect() %>%
      dplyr::group_by(ano) %>%
      # dplyr::slice_sample(n = 100) %>%
      data.table::as.data.table()
    
    untreat <- data[treat_RP_post == 0]
    
    # dep_vars[3] <- "log_avg_income_defl"
    
    gc()
    
    purrr::walk(
      dep_vars,
      function(j) {
        if (length(i) == 1) {
          first_stage_fml <- glue::glue("{j} ~ {i} | {fixed_vars[1]} + {fixed_vars[2]}")
          
          name_file <- glue::glue("outputs/RP/static/static_{j}_in_{i}_RP.rds")
        } else {
          k <- stringr::str_flatten(i, collapse = " + ")
          
          first_stage_fml <- glue::glue("{j} ~ 0 + {k} | {fixed_vars[1]} + {fixed_vars[2]}")
          
          name_file <- glue::glue("outputs/RP/static/static_{j}_in_{janitor::make_clean_names(k)}_RP.rds")
        }
        
        cat(glue::glue("\n\n=============== MODEL: {first_stage_fml} ===============\n\n"))
        
        second_stage_fml <- glue::glue("{j} ~ 0 + fixest::i({fixed_vars[4]}, ref = FALSE)")
        
        cat("\n++++++++++ Running first stage model... ++++++++++\n")
        
        first_stage_reg <- fixest::feols(
          as.formula(first_stage_fml),
          data = untreat,
          warn = FALSE,
          notes = FALSE,
          nthreads = 8,
          cluster = "RP"
        )
        
        gc()
        
        cat("\n++++++++++ Setting errors as dependent variable... ++++++++++\n")
        
        first_stage_reg_u <- data[[j]] - stats::predict(first_stage_reg, newdata = data)
        
        data[[j]] <- first_stage_reg_u
        
        gc()
        
        cat("\n++++++++++ Running second stage model ...++++++++++\n")
        
        second_stage_reg <- fixest::feols(
          as.formula(second_stage_fml),
          data = data,
          warn = FALSE,
          notes = FALSE,
          nthreads = 8,
          cluster = "RP"
        )
        
        gc()
        
        cat("\n++++++++++ Exporting models... ++++++++++\n")
        
        models <- list(
          first_stage = first_stage_reg,
          second_stage = second_stage_reg
        )
        
        readr::write_rds(models, name_file)
        
        gc()
        
        cat("\n\n=============== DONE ===============\n\n")
      }
    )
    
    gc()
    
  }
)

gc()

tictoc::toc()


### indicador_simples ----

tictoc::tic()

models <- list(
  0,
  c("race_id", "gender_id"),
  c("race_id", "gender_id", "idade", "tamanho_estabelecimento", "educ_id", "public_work")
)

purrr::walk(
  models,
  function(i) {
    dep_vars <- c("indicador_simples")
    
    fixed_vars <- c("ano", "RP", "treat_RP", "post", "idade")
    
    if (i[1] == 0) {
      selected_vars <- c(dep_vars, fixed_vars)
    } else {
      selected_vars <- c(i, dep_vars, fixed_vars)
    }
    
    cat("\n\n =============== Importing data...=============== \n\n")
    
    data <- fs::dir_ls("db/", regexp = "rais(.*)\\.parquet") %>%
      arrow::open_dataset() %>%
      dplyr::select(dplyr::all_of(selected_vars)) %>%
      dplyr::filter(ano >= 2001 & ano <= 2013) %>%
      dplyr::filter(idade >= 23 & idade <= 65) %>%
      # dplyr::filter(avg_income_defl > 0) %>%
      dplyr::mutate(treat_RP_post = ifelse(treat_RP == 1 & post == 1, 1, 0)) %>%
      # dplyr::mutate(log_avg_income_defl = log(avg_income_defl)) %>%
      dplyr::select(-c(treat_RP)) %>%
      dplyr::collect() %>%
      dplyr::group_by(ano) %>%
      # dplyr::slice_sample(n = 100) %>%
      data.table::as.data.table()
    
    untreat <- data[treat_RP_post == 0]
    
    # dep_vars[3] <- "log_avg_income_defl"
    
    gc()
    
    purrr::walk(
      dep_vars,
      function(j) {
        if (length(i) == 1) {
          first_stage_fml <- glue::glue("{j} ~ {i} | {fixed_vars[1]} + {fixed_vars[2]}")
          
          name_file <- glue::glue("outputs/RP/static/static_{j}_in_{i}_RP.rds")
        } else {
          k <- stringr::str_flatten(i, collapse = " + ")
          
          first_stage_fml <- glue::glue("{j} ~ 0 + {k} | {fixed_vars[1]} + {fixed_vars[2]}")
          
          name_file <- glue::glue("outputs/RP/static/static_{j}_in_{janitor::make_clean_names(k)}_RP.rds")
        }
        
        cat(glue::glue("\n\n=============== MODEL: {first_stage_fml} ===============\n\n"))
        
        second_stage_fml <- glue::glue("{j} ~ 0 + fixest::i({fixed_vars[4]}, ref = FALSE)")
        
        cat("\n++++++++++ Running first stage model... ++++++++++\n")
        
        first_stage_reg <- fixest::feols(
          as.formula(first_stage_fml),
          data = untreat,
          warn = FALSE,
          notes = FALSE,
          nthreads = 8,
          cluster = "RP"
        )
        
        gc()
        
        cat("\n++++++++++ Setting errors as dependent variable... ++++++++++\n")
        
        first_stage_reg_u <- data[[j]] - stats::predict(first_stage_reg, newdata = data)
        
        data[[j]] <- first_stage_reg_u
        
        gc()
        
        cat("\n++++++++++ Running second stage model ...++++++++++\n")
        
        second_stage_reg <- fixest::feols(
          as.formula(second_stage_fml),
          data = data,
          warn = FALSE,
          notes = FALSE,
          nthreads = 8,
          cluster = "RP"
        )
        
        gc()
        
        cat("\n++++++++++ Exporting models... ++++++++++\n")
        
        models <- list(
          first_stage = first_stage_reg,
          second_stage = second_stage_reg
        )
        
        readr::write_rds(models, name_file)
        
        gc()
        
        cat("\n\n=============== DONE ===============\n\n")
      }
    )
    
    gc()
    
  }
)

gc()

tictoc::toc()

### q1_id ----

tictoc::tic()

models <- list(
  0,
  c("race_id", "gender_id"),
  c("race_id", "gender_id", "idade", "tamanho_estabelecimento", "educ_id", "public_work")
)

purrr::walk(
  models,
  function(i) {
    dep_vars <- c("q1_id")
    
    fixed_vars <- c("ano", "RP", "treat_RP", "post", "idade")
    
    if (i[1] == 0) {
      selected_vars <- c(dep_vars, fixed_vars)
    } else {
      selected_vars <- c(i, dep_vars, fixed_vars)
    }
    
    cat("\n\n =============== Importing data...=============== \n\n")
    
    data <- fs::dir_ls("db/", regexp = "rais(.*)\\.parquet") %>%
      arrow::open_dataset() %>%
      dplyr::select(dplyr::all_of(selected_vars)) %>%
      dplyr::filter(ano >= 2001 & ano <= 2013) %>%
      dplyr::filter(idade >= 23 & idade <= 65) %>%
      # dplyr::filter(avg_income_defl > 0) %>%
      dplyr::mutate(treat_RP_post = ifelse(treat_RP == 1 & post == 1, 1, 0)) %>%
      # dplyr::mutate(log_avg_income_defl = log(avg_income_defl)) %>%
      dplyr::select(-c(treat_RP)) %>%
      dplyr::collect() %>%
      dplyr::group_by(ano) %>%
      # dplyr::slice_sample(n = 100) %>%
      data.table::as.data.table()
    
    untreat <- data[treat_RP_post == 0]
    
    # dep_vars[3] <- "log_avg_income_defl"
    
    gc()
    
    purrr::walk(
      dep_vars,
      function(j) {
        if (length(i) == 1) {
          first_stage_fml <- glue::glue("{j} ~ {i} | {fixed_vars[1]} + {fixed_vars[2]}")
          
          name_file <- glue::glue("outputs/RP/static/static_{j}_in_{i}_RP.rds")
        } else {
          k <- stringr::str_flatten(i, collapse = " + ")
          
          first_stage_fml <- glue::glue("{j} ~ 0 + {k} | {fixed_vars[1]} + {fixed_vars[2]}")
          
          name_file <- glue::glue("outputs/RP/static/static_{j}_in_{janitor::make_clean_names(k)}_RP.rds")
        }
        
        cat(glue::glue("\n\n=============== MODEL: {first_stage_fml} ===============\n\n"))
        
        second_stage_fml <- glue::glue("{j} ~ 0 + fixest::i({fixed_vars[4]}, ref = FALSE)")
        
        cat("\n++++++++++ Running first stage model... ++++++++++\n")
        
        first_stage_reg <- fixest::feols(
          as.formula(first_stage_fml),
          data = untreat,
          warn = FALSE,
          notes = FALSE,
          nthreads = 8,
          cluster = "RP"
        )
        
        gc()
        
        cat("\n++++++++++ Setting errors as dependent variable... ++++++++++\n")
        
        first_stage_reg_u <- data[[j]] - stats::predict(first_stage_reg, newdata = data)
        
        data[[j]] <- first_stage_reg_u
        
        gc()
        
        cat("\n++++++++++ Running second stage model ...++++++++++\n")
        
        second_stage_reg <- fixest::feols(
          as.formula(second_stage_fml),
          data = data,
          warn = FALSE,
          notes = FALSE,
          nthreads = 8,
          cluster = "RP"
        )
        
        gc()
        
        cat("\n++++++++++ Exporting models... ++++++++++\n")
        
        models <- list(
          first_stage = first_stage_reg,
          second_stage = second_stage_reg
        )
        
        readr::write_rds(models, name_file)
        
        gc()
        
        cat("\n\n=============== DONE ===============\n\n")
      }
    )
    
    gc()
    
  }
)

gc()

tictoc::toc()


### gender_id ----

tictoc::tic()

models <- list(
  0,
  # c("race_id", "gender_id"),
  c("race_id", "idade", "tamanho_estabelecimento", "educ_id", "public_work")
)

purrr::walk(
  models,
  function(i) {
    dep_vars <- c("gender_id")
    
    fixed_vars <- c("ano", "RP", "treat_RP", "post", "idade")
    
    if (i[1] == 0) {
      selected_vars <- c(dep_vars, fixed_vars)
    } else {
      selected_vars <- c(i, dep_vars, fixed_vars)
    }
    
    cat("\n\n =============== Importing data...=============== \n\n")
    
    data <- fs::dir_ls("db/", regexp = "rais(.*)\\.parquet") %>%
      arrow::open_dataset() %>%
      dplyr::select(dplyr::all_of(selected_vars)) %>%
      dplyr::filter(ano >= 2001 & ano <= 2013) %>%
      dplyr::filter(idade >= 23 & idade <= 65) %>%
      # dplyr::filter(avg_income_defl > 0) %>%
      dplyr::mutate(treat_RP_post = ifelse(treat_RP == 1 & post == 1, 1, 0)) %>%
      # dplyr::mutate(log_avg_income_defl = log(avg_income_defl)) %>%
      dplyr::select(-c(treat_RP)) %>%
      dplyr::collect() %>%
      dplyr::group_by(ano) %>%
      # dplyr::slice_sample(n = 100) %>%
      data.table::as.data.table()
    
    untreat <- data[treat_RP_post == 0]
    
    # dep_vars[3] <- "log_avg_income_defl"
    
    gc()
    
    purrr::walk(
      dep_vars,
      function(j) {
        if (length(i) == 1) {
          first_stage_fml <- glue::glue("{j} ~ {i} | {fixed_vars[1]} + {fixed_vars[2]}")
          
          name_file <- glue::glue("outputs/RP/static/static_{j}_in_{i}_RP.rds")
        } else {
          k <- stringr::str_flatten(i, collapse = " + ")
          
          first_stage_fml <- glue::glue("{j} ~ 0 + {k} | {fixed_vars[1]} + {fixed_vars[2]}")
          
          name_file <- glue::glue("outputs/RP/static/static_{j}_in_{janitor::make_clean_names(k)}_RP.rds")
        }
        
        cat(glue::glue("\n\n=============== MODEL: {first_stage_fml} ===============\n\n"))
        
        second_stage_fml <- glue::glue("{j} ~ 0 + fixest::i({fixed_vars[4]}, ref = FALSE)")
        
        cat("\n++++++++++ Running first stage model... ++++++++++\n")
        
        first_stage_reg <- fixest::feols(
          as.formula(first_stage_fml),
          data = untreat,
          warn = FALSE,
          notes = FALSE,
          nthreads = 8,
          cluster = "RP"
        )
        
        gc()
        
        cat("\n++++++++++ Setting errors as dependent variable... ++++++++++\n")
        
        first_stage_reg_u <- data[[j]] - stats::predict(first_stage_reg, newdata = data)
        
        data[[j]] <- first_stage_reg_u
        
        gc()
        
        cat("\n++++++++++ Running second stage model ...++++++++++\n")
        
        second_stage_reg <- fixest::feols(
          as.formula(second_stage_fml),
          data = data,
          warn = FALSE,
          notes = FALSE,
          nthreads = 8,
          cluster = "RP"
        )
        
        gc()
        
        cat("\n++++++++++ Exporting models... ++++++++++\n")
        
        models <- list(
          first_stage = first_stage_reg,
          second_stage = second_stage_reg
        )
        
        readr::write_rds(models, name_file)
        
        gc()
        
        cat("\n\n=============== DONE ===============\n\n")
      }
    )
    
    gc()
    
  }
)

gc()

tictoc::toc()

### race_id ----

tictoc::tic()

models <- list(
  0,
  # c("race_id", "gender_id"),
  c("gender_id", "idade", "tamanho_estabelecimento", "educ_id", "public_work")
)

purrr::walk(
  models,
  function(i) {
    dep_vars <- c("race_id")
    
    fixed_vars <- c("ano", "RP", "treat_RP", "post", "idade")
    
    if (i[1] == 0) {
      selected_vars <- c(dep_vars, fixed_vars)
    } else {
      selected_vars <- c(i, dep_vars, fixed_vars)
    }
    
    cat("\n\n =============== Importing data...=============== \n\n")
    
    data <- fs::dir_ls("db/", regexp = "rais(.*)\\.parquet") %>%
      arrow::open_dataset() %>%
      dplyr::select(dplyr::all_of(selected_vars)) %>%
      dplyr::filter(ano >= 2001 & ano <= 2013) %>%
      dplyr::filter(idade >= 23 & idade <= 65) %>%
      # dplyr::filter(avg_income_defl > 0) %>%
      dplyr::mutate(treat_RP_post = ifelse(treat_RP == 1 & post == 1, 1, 0)) %>%
      # dplyr::mutate(log_avg_income_defl = log(avg_income_defl)) %>%
      dplyr::select(-c(treat_RP)) %>%
      dplyr::collect() %>%
      dplyr::group_by(ano) %>%
      # dplyr::slice_sample(n = 100) %>%
      data.table::as.data.table()
    
    untreat <- data[treat_RP_post == 0]
    
    # dep_vars[3] <- "log_avg_income_defl"
    
    gc()
    
    purrr::walk(
      dep_vars,
      function(j) {
        if (length(i) == 1) {
          first_stage_fml <- glue::glue("{j} ~ {i} | {fixed_vars[1]} + {fixed_vars[2]}")
          
          name_file <- glue::glue("outputs/RP/static/static_{j}_in_{i}_RP.rds")
        } else {
          k <- stringr::str_flatten(i, collapse = " + ")
          
          first_stage_fml <- glue::glue("{j} ~ 0 + {k} | {fixed_vars[1]} + {fixed_vars[2]}")
          
          name_file <- glue::glue("outputs/RP/static/static_{j}_in_{janitor::make_clean_names(k)}_RP.rds")
        }
        
        cat(glue::glue("\n\n=============== MODEL: {first_stage_fml} ===============\n\n"))
        
        second_stage_fml <- glue::glue("{j} ~ 0 + fixest::i({fixed_vars[4]}, ref = FALSE)")
        
        cat("\n++++++++++ Running first stage model... ++++++++++\n")
        
        first_stage_reg <- fixest::feols(
          as.formula(first_stage_fml),
          data = untreat,
          warn = FALSE,
          notes = FALSE,
          nthreads = 8,
          cluster = "RP"
        )
        
        gc()
        
        cat("\n++++++++++ Setting errors as dependent variable... ++++++++++\n")
        
        first_stage_reg_u <- data[[j]] - stats::predict(first_stage_reg, newdata = data)
        
        data[[j]] <- first_stage_reg_u
        
        gc()
        
        cat("\n++++++++++ Running second stage model ...++++++++++\n")
        
        second_stage_reg <- fixest::feols(
          as.formula(second_stage_fml),
          data = data,
          warn = FALSE,
          notes = FALSE,
          nthreads = 8,
          cluster = "RP"
        )
        
        gc()
        
        cat("\n++++++++++ Exporting models... ++++++++++\n")
        
        models <- list(
          first_stage = first_stage_reg,
          second_stage = second_stage_reg
        )
        
        readr::write_rds(models, name_file)
        
        gc()
        
        cat("\n\n=============== DONE ===============\n\n")
      }
    )
    
    gc()
    
  }
)

gc()

tictoc::toc()

