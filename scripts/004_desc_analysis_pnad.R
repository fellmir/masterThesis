### beginning ----

source("scripts/001_libraries.R")

desc.base <- c("mean", "sd", "min", "q1", "med", "q3", "max", "n.valid", "pct.valid")

fs::dir_create("outputs/pnad")
fs::dir_create("outputs/pnad/freqs/doms")
fs::dir_create("outputs/pnad/freqs/indv")
fs::dir_create("outputs/pnad/cross-tabs/doms")
fs::dir_create("outputs/pnad/cross-tabs/indv")
fs::dir_create("outputs/pnad/desc-stats/doms")
fs::dir_create("outputs/pnad/desc-stats/indv")
fs::dir_create("outputs/pnad/plots/doms")
fs::dir_create("outputs/pnad/plots/indv")
fs::dir_create("outputs/pnad/comparisons/doms")
fs::dir_create("outputs/pnad/comparisons/indv")

### loading pnad dbs ----
# doms ----
pnad.doms.2001 <- readRDS("db/pnad/pnad_doms_2001.rds") %>%
  select(c(
    "renda_mensal_domiciliar_compativel_1992",
    "renda_mensal_domiciliar_compativel_1992_deflacionado",
    "total_pessoas", "cat_tipo_domicilio"
  ))

pnad.doms.2002 <- readRDS("db/pnad/pnad_doms_2002.rds") %>%
  select(c(
    "renda_mensal_domiciliar_compativel_1992",
    "renda_mensal_domiciliar_compativel_1992_deflacionado",
    "total_pessoas", "cat_tipo_domicilio"
  ))

pnad.doms.2003 <- readRDS("db/pnad/pnad_doms_2003.rds") %>%
  select(c(
    "renda_mensal_domiciliar_compativel_1992",
    "renda_mensal_domiciliar_compativel_1992_deflacionado",
    "total_pessoas", "cat_tipo_domicilio"
  ))

pnad.doms.2004 <- readRDS("db/pnad/pnad_doms_2004.rds") %>%
  select(c(
    "renda_mensal_domiciliar_compativel_1992",
    "renda_mensal_domiciliar_compativel_1992_deflacionado",
    "total_pessoas", "cat_tipo_domicilio"
  ))

pnad.doms.2005 <- readRDS("db/pnad/pnad_doms_2005.rds") %>%
  select(c(
    "renda_mensal_domiciliar_compativel_1992",
    "renda_mensal_domiciliar_compativel_1992_deflacionado",
    "total_pessoas", "cat_tipo_domicilio"
  ))

pnad.doms.2006 <- readRDS("db/pnad/pnad_doms_2006.rds") %>%
  select(c(
    "renda_mensal_domiciliar_compativel_1992",
    "renda_mensal_domiciliar_compativel_1992_deflacionado",
    "total_pessoas", "cat_tipo_domicilio"
  ))

pnad.doms.2007 <- readRDS("db/pnad/pnad_doms_2007.rds") %>%
  select(c(
    "renda_mensal_domiciliar_compativel_1992",
    "renda_mensal_domiciliar_compativel_1992_deflacionado",
    "total_pessoas", "cat_tipo_domicilio"
  ))

pnad.doms.2008 <- readRDS("db/pnad/pnad_doms_2008.rds") %>%
  select(c(
    "renda_mensal_domiciliar_compativel_1992",
    "renda_mensal_domiciliar_compativel_1992_deflacionado",
    "total_pessoas", "cat_tipo_domicilio"
  ))

pnad.doms.2009 <- readRDS("db/pnad/pnad_doms_2009.rds") %>%
  select(c(
    "renda_mensal_domiciliar_compativel_1992",
    "renda_mensal_domiciliar_compativel_1992_deflacionado",
    "total_pessoas", "cat_tipo_domicilio"
  ))

pnad.doms.2011 <- readRDS("db/pnad/pnad_doms_2011.rds") %>%
  select(c(
    "renda_mensal_domiciliar_compativel_1992",
    "renda_mensal_domiciliar_compativel_1992_deflacionado",
    "total_pessoas", "cat_tipo_domicilio"
  ))

pnad.doms.2012 <- readRDS("db/pnad/pnad_doms_2012.rds") %>%
  select(c(
    "renda_mensal_domiciliar_compativel_1992",
    "renda_mensal_domiciliar_compativel_1992_deflacionado",
    "total_pessoas", "cat_tipo_domicilio"
  ))

pnad.doms.2013 <- readRDS("db/pnad/pnad_doms_2003.rds") %>%
  select(c(
    "renda_mensal_domiciliar_compativel_1992",
    "renda_mensal_domiciliar_compativel_1992_deflacionado",
    "total_pessoas", "cat_tipo_domicilio"
  ))

pnad.doms.2014 <- readRDS("db/pnad/pnad_doms_2014.rds") %>%
  select(c(
    "renda_mensal_domiciliar_compativel_1992",
    "renda_mensal_domiciliar_compativel_1992_deflacionado",
    "total_pessoas", "cat_tipo_domicilio"
  ))

pnad.doms.2015 <- readRDS("db/pnad/pnad_doms_2015.rds") %>%
  select(c(
    "renda_mensal_domiciliar_compativel_1992",
    "renda_mensal_domiciliar_compativel_1992_deflacionado",
    "total_pessoas", "cat_tipo_domicilio"
  ))

# indv ----
pnad.indv.2001 <- readRDS("db/pnad/pnad_indv_2001.rds") %>%
  select(starts_with(c(
    "renda", "ano", "idade", "ultima", "trab",
    "tinha", "tomou", "possui", "contribui", "horas", "cat"
  ))) %>%
  select(-c("cat_atividade_ramo_negocio_semana", "ano_nascimento", "renda_mensal_familia"))

pnad.indv.2002 <- readRDS("db/pnad/pnad_indv_2002.rds") %>%
  select(starts_with(c(
    "renda", "ano", "idade", "ultima", "trab",
    "tinha", "tomou", "possui", "contribui", "horas", "cat"
  ))) %>%
  select(-c("cat_atividade_ramo_negocio_semana", "ano_nascimento", "renda_mensal_familia"))

pnad.indv.2003 <- readRDS("db/pnad/pnad_indv_2003.rds") %>%
  select(starts_with(c(
    "renda", "ano", "idade", "ultima", "trab",
    "tinha", "tomou", "possui", "contribui", "horas", "cat"
  ))) %>%
  select(-c("cat_atividade_ramo_negocio_semana", "ano_nascimento", "renda_mensal_familia"))

pnad.indv.2004 <- readRDS("db/pnad/pnad_indv_2004.rds") %>%
  select(starts_with(c(
    "renda", "ano", "idade", "ultima", "trab",
    "tinha", "tomou", "possui", "contribui", "horas", "cat"
  ))) %>%
  select(-c("cat_atividade_ramo_negocio_semana", "ano_nascimento", "renda_mensal_familia"))

pnad.indv.2005 <- readRDS("db/pnad/pnad_indv_2005.rds") %>%
  select(starts_with(c(
    "renda", "ano", "idade", "ultima", "trab",
    "tinha", "tomou", "possui", "contribui", "horas", "cat"
  ))) %>%
  select(-c("cat_atividade_ramo_negocio_semana", "ano_nascimento", "renda_mensal_familia"))

pnad.indv.2006 <- readRDS("db/pnad/pnad_indv_2006.rds") %>%
  select(starts_with(c(
    "renda", "ano", "idade", "ultima", "trab",
    "tinha", "tomou", "possui", "contribui", "horas", "cat"
  ))) %>%
  select(-c("cat_atividade_ramo_negocio_semana", "ano_nascimento", "renda_mensal_familia"))

pnad.indv.2007 <- readRDS("db/pnad/pnad_indv_2007.rds") %>%
  select(starts_with(c(
    "renda", "ano", "idade", "ultima", "trab",
    "tinha", "tomou", "possui", "contribui", "horas", "cat"
  ))) %>%
  select(-c("cat_atividade_ramo_negocio_semana", "ano_nascimento", "renda_mensal_familia"))

pnad.indv.2008 <- readRDS("db/pnad/pnad_indv_2008.rds") %>%
  select(starts_with(c(
    "renda", "ano", "idade", "ultima", "trab",
    "tinha", "tomou", "possui", "contribui", "horas", "cat"
  ))) %>%
  select(-c("cat_atividade_ramo_negocio_semana", "ano_nascimento", "renda_mensal_familia"))

pnad.indv.2009 <- readRDS("db/pnad/pnad_indv_2009.rds") %>%
  select(starts_with(c(
    "renda", "ano", "idade", "ultima", "trab",
    "tinha", "tomou", "possui", "contribui", "horas", "cat"
  ))) %>%
  select(-c("cat_atividade_ramo_negocio_semana", "ano_nascimento", "renda_mensal_familia"))

pnad.indv.2011 <- readRDS("db/pnad/pnad_indv_2011.rds") %>%
  select(starts_with(c(
    "renda", "ano", "idade", "ultima", "trab",
    "tinha", "tomou", "possui", "contribui", "horas", "cat"
  ))) %>%
  select(-c("cat_atividade_ramo_negocio_semana", "ano_nascimento", "renda_mensal_familia"))

pnad.indv.2012 <- readRDS("db/pnad/pnad_indv_2012.rds") %>%
  select(starts_with(c(
    "renda", "ano", "idade", "ultima", "trab",
    "tinha", "tomou", "possui", "contribui", "horas", "cat"
  ))) %>%
  select(-c("cat_atividade_ramo_negocio_semana", "ano_nascimento", "renda_mensal_familia"))

pnad.indv.2013 <- readRDS("db/pnad/pnad_indv_2013.rds") %>%
  select(starts_with(c(
    "renda", "ano", "idade", "ultima", "trab",
    "tinha", "tomou", "possui", "contribui", "horas", "cat"
  ))) %>%
  select(-c("cat_atividade_ramo_negocio_semana", "ano_nascimento", "renda_mensal_familia"))

pnad.indv.2014 <- readRDS("db/pnad/pnad_indv_2014.rds") %>%
  select(starts_with(c(
    "renda", "ano", "idade", "ultima", "trab",
    "tinha", "tomou", "possui", "contribui", "horas", "cat"
  ))) %>%
  select(-c("cat_atividade_ramo_negocio_semana", "ano_nascimento", "renda_mensal_familia"))

pnad.indv.2015 <- readRDS("db/pnad/pnad_indv_2015.rds") %>%
  select(starts_with(c(
    "renda", "ano", "idade", "ultima", "trab",
    "tinha", "tomou", "possui", "contribui", "horas", "cat"
  ))) %>%
  select(-c("cat_atividade_ramo_negocio_semana", "ano_nascimento", "renda_mensal_familia"))


### functions ----
# frequency table function for multiple dataframes
freq.loop <- function(df, gp_var = NA, par, ord = "default", db_name = NULL) {
  par <- as.symbol(par)

  if (is.na(gp_var)) {
    col <- df %>%
      dplyr::select({{ par }})

    if (!is.null(db_name)) {
      cat(glue::glue("\n\nDatabase = {db_name}\n\n"))

      print(
        summarytools::freq(
          col,
          report.nas = FALSE,
          order = ord,
          cumul = FALSE
          # headings = FALSE
        ),
        # caption = db_name # mudar?
      )
    } else {
      x <- summarytools::freq(
        col,
        report.nas = FALSE,
        order = ord,
        cumul = FALSE,
        # headings = FALSE
      )

      return(x)
    }
  } else {
    gp_var <- as.symbol(gp_var)

    col <- df %>%
      dplyr::group_by({{ gp_var }}) %>%
      dplyr::select({{ par }})

    if (!is.null(db_name)) {
      cat(glue::glue("\n\nDatabase = {db_name}\n\n"))

      print(
        summarytools::freq(
          col,
          report.nas = FALSE,
          order = ord,
          cumul = FALSE,
          # headings = FALSE
        ),
        # caption = db_name # mudar?
      )
    } else {
      x <- summarytools::freq(
        col,
        report.nas = FALSE,
        order = ord,
        cumul = FALSE,
        # headings = FALSE
      )

      return(x)
    }
  }
}

# cross-tabulation function for multiple dataframes
ctable.loop <- function(df, x_var, y_var, gp_var = NA, db_name = NA) {
  if (is.na(gp_var)) {
    cols <- df %>%
      dplyr::select(tidyselect::all_of(c(x_var, y_var)))

    cat(glue::glue("\n\nDatabase = {db_name}\n\n"))

    print(
      summarytools::ctable(
        x = cols[[1]],
        y = cols[[2]],
        prop = "r",
        useNA = "no",
        totals = FALSE,
        dnn = c(x_var, y_var)
        # headings = FALSE
      )
    )
  } else {
    gp_var_sym <- as.symbol(gp_var)

    cols <- df %>%
      dplyr::select(tidyselect::all_of(c(x_var, y_var)), {{ gp_var_sym }}) %>%
      dplyr::group_by({{ gp_var_sym }}) %>%
      tidyr::nest() %>%
      dplyr::arrange({{ gp_var_sym }})

    purrr::walk(
      seq_len(nrow(cols)),
      function(i) {
        cat(glue::glue("\n\nDatabase = {db_name}\nGroup = {cols[[1]][i]}\n\n"))

        print(
          summarytools::ctable(
            x = cols$data[[i]][[x_var]],
            y = cols$data[[i]][[y_var]],
            dnn = c(x_var, y_var),
            prop = "r",
            useNA = "no",
            totals = FALSE
          )
        )
      }
    )
  }
}

# bar plot show
geom.bar.fun <- function(df, var, gp_var = NA, name_plot) {
  var_sym <- as.symbol(var)

  if (is.na(gp_var)) {
    plot_var <- df %>%
      tidyr::drop_na({{ var_sym }}) %>%
      ggplot() +
      aes(x = {{ var_sym }}) %>%
      geom_bar()
  } else {
    gp_var_sym <- as.symbol(gp_var)

    plot_var <- df %>%
      tidyr::drop_na({{ var_sym }}) %>%
      ggplot() +
      aes(
        x = {{ var_sym }},
        fill = {{ gp_var_sym }}
      ) %>%
      geom_bar()
  }

  plot_var <- plot_var +
    ggtitle(glue::glue("{name_plot}_{var}"))
}

# bar plot saving
geom.bar.test.doms <- function(df, var, gp_var = NA, name_plot) {
  var_sym <- as.symbol(var)

  if (is.na(gp_var)) {
    plot_var <- df %>%
      tidyr::drop_na({{ var_sym }}) %>%
      ggplot() +
      aes(x = {{ var_sym }}) %>%
      geom_bar()
  } else {
    gp_var_sym <- as.symbol(gp_var)

    plot_var <- df %>%
      tidyr::drop_na({{ var_sym }}) %>%
      ggplot() +
      aes(
        x = {{ var_sym }},
        fill = {{ gp_var_sym }}
      ) %>%
      geom_bar()
  }

  plot_var <- plot_var +
    ggtitle(glue::glue("{name_plot}_{var}"))

  ggsave(
    plot = plot_var,
    # width = x,
    # height = y,
    # units = "px",
    type = "cairo",
    filename = glue::glue("outputs/pnad/plots/doms/{name_plot}_{var}.png")
  )
}

geom.bar.test.indv <- function(df, var, gp_var = NA, name_plot) {
  var_sym <- as.symbol(var)

  if (is.na(gp_var)) {
    plot_var <- df %>%
      tidyr::drop_na({{ var_sym }}) %>%
      ggplot() +
      aes(x = {{ var_sym }}) %>%
      geom_bar()
  } else {
    gp_var_sym <- as.symbol(gp_var)

    plot_var <- df %>%
      tidyr::drop_na({{ var_sym }}) %>%
      ggplot() +
      aes(
        x = {{ var_sym }},
        fill = {{ gp_var_sym }}
      ) %>%
      geom_bar()
  }

  plot_var <- plot_var +
    ggtitle(glue::glue("{name_plot}_{var}"))

  ggsave(
    plot = plot_var,
    # width = x,
    # height = y,
    # units = "px",
    type = "cairo",
    filename = glue::glue("outputs/pnad/plots/indv/{name_plot}_{var}.png")
  )
}

# frequency saving (in .md)
freq.test.doms <- function(df, gp_var = NA, par, ord = "default", db_name = NA) {
  par <- as.symbol(par)

  if (is.na(gp_var)) {
    col <- df %>%
      dplyr::select({{ par }})

    sink(glue::glue("outputs/pnad/freqs/doms/{db_name}_{par}.md"), append = FALSE)

    cat(glue::glue("\n\n## Database = {db_name}\n\n"))

    print(
      summarytools::freq(
        col,
        report.nas = FALSE,
        order = ord,
        cumul = FALSE,
        style = "rmarkdown"
        # headings = FALSE
      )
    )

    sink()
  } else {
    gp_var <- as.symbol(gp_var)

    cols <- df %>%
      dplyr::group_by({{ gp_var }}) %>%
      dplyr::select({{ par }})

    sink(glue::glue("outputs/pnad/freqs/doms/{db_name}_{par}_{gp_var}.md"), append = FALSE)

    cat(glue::glue("\n\n## Database = {db_name}\n\n"))

    print(
      summarytools::freq(
        col,
        report.nas = FALSE,
        order = ord,
        cumul = FALSE,
        style = "rmarkdown"
        # headings = FALSE
      )
    )

    sink()
  }
}

freq.test.indv <- function(df, gp_var = NA, par, ord = "default", db_name = NA) {
  par <- as.symbol(par)

  if (is.na(gp_var)) {
    col <- df %>%
      dplyr::select({{ par }})

    sink(glue::glue("outputs/pnad/freqs/indv/{db_name}_{par}.md"), append = FALSE)

    cat(glue::glue("\n\n## Database = {db_name}\n\n"))

    print(
      summarytools::freq(
        col,
        report.nas = FALSE,
        order = ord,
        cumul = FALSE,
        style = "rmarkdown"
        # headings = FALSE
      )
    )

    sink()
  } else {
    gp_var <- as.symbol(gp_var)

    cols <- df %>%
      dplyr::group_by({{ gp_var }}) %>%
      dplyr::select({{ par }})

    sink(glue::glue("outputs/pnad/freqs/indv/{db_name}_{par}_{gp_var}.md"), append = FALSE)

    cat(glue::glue("\n\n## Database = {db_name}\n\n"))

    print(
      summarytools::freq(
        col,
        report.nas = FALSE,
        order = ord,
        cumul = FALSE,
        style = "rmarkdown"
        # headings = FALSE
      )
    )

    sink()
  }
}

# cross-tabulation saving (in .md)
ctable.test.doms <- function(df, x_var, y_var, gp_var = NA, db_name = NA) {
  if (is.na(gp_var)) {
    cols <- df %>%
      dplyr::select(tidyselect::all_of(c(x_var, y_var)))

    sink(glue::glue("outputs/pnad/cross-tabs/doms/{db_name}_{x_var}_{y_var}.md"), append = FALSE)

    cat(glue::glue("\n\nDatabase = {db_name}\n\n"))

    print(
      summarytools::ctable(
        x = cols[[1]],
        y = cols[[2]],
        prop = "r",
        useNA = "no",
        totals = FALSE,
        dnn = c(x_var, y_var)
        # headings = FALSE
      )
    )

    sink()
  } else {
    gp_var_sym <- as.symbol(gp_var)

    cols <- df %>%
      dplyr::select(tidyselect::all_of(c(x_var, y_var)), {{ gp_var_sym }}) %>%
      dplyr::group_by({{ gp_var_sym }}) %>%
      tidyr::nest() %>%
      dplyr::arrange({{ gp_var_sym }})

    sink(glue::glue("outputs/pnad/cross-tabs/doms/{db_name}_{x_var}_{y_var}_{gp_var}.md"), append = FALSE)

    purrr::walk(
      seq_len(nrow(cols)),
      function(i) {
        cat(glue::glue("\n\nDatabase = {db_name}\nGroup = {cols[[1]][i]}\n\n"))

        print(
          summarytools::ctable(
            x = cols$data[[i]][[x_var]],
            y = cols$data[[i]][[y_var]],
            dnn = c(x_var, y_var),
            prop = "r",
            useNA = "no",
            totals = FALSE
          )
        )
      }
    )

    sink()
  }
}

ctable.test.indv <- function(df, x_var, y_var, gp_var = NA, db_name = NA) {
  if (is.na(gp_var)) {
    cols <- df %>%
      dplyr::select(tidyselect::all_of(c(x_var, y_var)))

    sink(glue::glue("outputs/pnad/cross-tabs/indv/{db_name}_{x_var}_{y_var}.md"), append = FALSE)

    cat(glue::glue("\n\nDatabase = {db_name}\n\n"))

    print(
      summarytools::ctable(
        x = cols[[1]],
        y = cols[[2]],
        prop = "r",
        useNA = "no",
        totals = FALSE,
        dnn = c(x_var, y_var)
        # headings = FALSE
      )
    )

    sink()
  } else {
    gp_var_sym <- as.symbol(gp_var)

    cols <- df %>%
      dplyr::select(tidyselect::all_of(c(x_var, y_var)), {{ gp_var_sym }}) %>%
      dplyr::group_by({{ gp_var_sym }}) %>%
      tidyr::nest() %>%
      dplyr::arrange({{ gp_var_sym }})

    sink(glue::glue("outputs/pnad/cross-tabs/indv/{db_name}_{x_var}_{y_var}_{gp_var}.md"), append = FALSE)

    purrr::walk(
      seq_len(nrow(cols)),
      function(i) {
        cat(glue::glue("\n\nDatabase = {db_name}\nGroup = {cols[[1]][i]}\n\n"))

        print(
          summarytools::ctable(
            x = cols$data[[i]][[x_var]],
            y = cols$data[[i]][[y_var]],
            dnn = c(x_var, y_var),
            prop = "r",
            useNA = "no",
            totals = FALSE
          )
        )
      }
    )

    sink()
  }
}

# function examples ----
# frequency table example (for multiple dataframes and multiple variables)
purrr::walk2(
  .x = list(pnad.doms.2001, pnad.doms.2002),
  .y = c("PNAD doms 2001", "PNAD doms 2002"),
  function(i, j) {
    purrr::walk(
      .x = c("cat_tipo_domicilio", "total_pessoas"),
      function(k) {
        freq.test(
          df = i,
          par = k,
          gp_var = NA,
          db_name = j
        )
      }
    )
  }
)

# frequency table example (for all possible combinations)
gp_vars <- NA

par_vars <- c("cat_tipo_domicilio", "range_total_rendimentos")

purrr::map(
  .x = list(
    pnad.doms.2001, pnad.doms.2002, pnad.doms.2003, pnad.doms.2004, pnad.doms.2005,
    pnad.doms.2006, pnad.doms.2007, pnad.doms.2008, pnad.doms.2009, pnad.doms.2011,
    pnad.doms.2012, pnad.doms.2013, pnad.doms.2014, pnad.doms.2015
  ),
  function(i) {
    purrr::map(
      .x = seq_len(length(gp_vars)),
      function(j) {
        purrr::map(
          .x = seq_len(length(par_vars)),
          function(k) {
            freq.loop(
              df = i,
              gp_var = gp_vars[j],
              par = par_vars[k]
            )
          }
        )
      }
    )
  }
)

# frequency table example (for all variables possible in dbs)
purrr::map(
  .x = list(
    pnad.doms.2001, pnad.doms.2002, pnad.doms.2003, pnad.doms.2004, pnad.doms.2005,
    pnad.doms.2006, pnad.doms.2007, pnad.doms.2008, pnad.doms.2009, pnad.doms.2011,
    pnad.doms.2012, pnad.doms.2013, pnad.doms.2014, pnad.doms.2015
  ),
  ~ .x %>%
    # group_by(area_planejamento) %>%
    freq()
)

# cross-tabulation example (for one db)
ctable.loop(
  df = pnad.doms.2001,
  x_var = "range_total_rendimentos",
  y_var = "cat_tipo_domicilio",
  # gp_var = "area_planejamento"
)

# cross-tabulation example (for multiple dbs)
purrr::walk2(
  .x = list(pnad.doms.2001, pnad.doms.2002),
  .y = c("PNAD doms 2001", "PNAD doms 2002"),
  ~ ctable.loop(
    df = .x,
    x_var = "range_total_rendimentos",
    y_var = "cat_tipo_domicilio",
    gp_var = NA,
    db_name = .y
  )
)

# descriptive statistics example (for multiple databases)
purrr::map(
  .x = list(pnad.doms.2001, pnad.doms.2002, pnad.doms.2003, pnad.doms.2004, pnad.doms.2005),
  ~ .x %>%
    # group_by(area_planejamento) %>%
    descr(stats = desc.base)
)

# bar plot example (for multiple databases)
purrr::map2(
  .x = list(pnad_doms_2001, pnad_doms_2002),
  .y = c("PNAD Doms 2001", "PNAD Doms 2002"),
  function(i, j) {
    purrr::map(
      .x = c("cat_tipo_domicilio", "total_pessoas"),
      ~ geom.bar.fun(
        df = i,
        title = j,
        var = .x,
        # gp_var = "sexo"
      )
    )
  }
)

# frequency comparison between multiple databases (w/o geo division)
purrr::map2(
  .x = list(pnad.doms.2001, pnad.doms.2002, pnad.doms.2003, pnad.doms.2004, pnad.doms.2005),
  .y = c(2001:2005),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "cat_tipo_domicilio",
      ord = "default"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "cat_tipo_domicilio")

# frequency comparison between multiple databases (with geo division)
# purrr::map2(
#   .x = list(census.2000.doms, census.2010.doms),
#   .y = c(2000, 2010),
#   function(i, j) {
#     x <- freq.loop(
#       df = i,
#       gp_var = "area_planejamento",
#       par = "cat_microcomputador"
#     ) %>%
#       summarytools::tb()
#     # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for big dataframes
#
#     names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))
#
#     return(x)
#   }
# ) %>%
#   purrr::reduce(dplyr::left_join, by = c("area_planejamento", "cat_microcomputador"))
# # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for small dataframes

### descriptive analysis - pnad doms ----

ls(pnad.doms.2001)
str(pnad.doms.2001)

# pnad doms frequencies ----
purrr::walk2(
  .x = list(
    pnad.doms.2001, pnad.doms.2002, pnad.doms.2003, pnad.doms.2004, pnad.doms.2005,
    pnad.doms.2006, pnad.doms.2007, pnad.doms.2008, pnad.doms.2009, pnad.doms.2011,
    pnad.doms.2012, pnad.doms.2013, pnad.doms.2014, pnad.doms.2015
  ),
  .y = c(
    "PNAD doms 2001", "PNAD doms 2002", "PNAD doms 2003", "PNAD doms 2004",
    "PNAD doms 2005", "PNAD doms 2006", "PNAD doms 2007", "PNAD doms 2008",
    "PNAD doms 2009", "PNAD doms 2011", "PNAD doms 2012", "PNAD doms 2013",
    "PNAD doms 2014", "PNAD doms 2015"
  ),
  ~ freq.test.doms(
    df = .x,
    par = "cat_tipo_domicilio",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.doms.2001, pnad.doms.2002, pnad.doms.2003, pnad.doms.2004, pnad.doms.2005,
    pnad.doms.2006, pnad.doms.2007, pnad.doms.2008, pnad.doms.2009, pnad.doms.2011,
    pnad.doms.2012, pnad.doms.2013, pnad.doms.2014, pnad.doms.2015
  ),
  .y = c(
    "PNAD doms 2001", "PNAD doms 2002", "PNAD doms 2003", "PNAD doms 2004",
    "PNAD doms 2005", "PNAD doms 2006", "PNAD doms 2007", "PNAD doms 2008",
    "PNAD doms 2009", "PNAD doms 2011", "PNAD doms 2012", "PNAD doms 2013",
    "PNAD doms 2014", "PNAD doms 2015"
  ),
  ~ freq.test.doms(
    df = .x,
    par = "total_pessoas",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.doms.2001, pnad.doms.2002, pnad.doms.2003, pnad.doms.2004, pnad.doms.2005,
    pnad.doms.2006, pnad.doms.2007, pnad.doms.2008, pnad.doms.2009, pnad.doms.2011,
    pnad.doms.2012, pnad.doms.2013, pnad.doms.2014, pnad.doms.2015
  ),
  .y = c(
    "PNAD doms 2001", "PNAD doms 2002", "PNAD doms 2003", "PNAD doms 2004",
    "PNAD doms 2005", "PNAD doms 2006", "PNAD doms 2007", "PNAD doms 2008",
    "PNAD doms 2009", "PNAD doms 2011", "PNAD doms 2012", "PNAD doms 2013",
    "PNAD doms 2014", "PNAD doms 2015"
  ),
  ~ freq.test.doms(
    df = .x,
    par = "range_total_rendimentos",
    gp_var = NA,
    db_name = .y
  )
)

# gp_vars <- NA
#
# par_vars <- c("cat_tipo_domicilio", "total_pessoas", "range_total_rendimentos")

# purrr::map(
#   .x = list(pnad.doms.2001, pnad.doms.2002, pnad.doms.2003, pnad.doms.2004, pnad.doms.2005,
#             pnad.doms.2006, pnad.doms.2007, pnad.doms.2008, pnad.doms.2009, pnad.doms.2011,
#             pnad.doms.2012, pnad.doms.2013, pnad.doms.2014, pnad.doms.2015),
#   function(i) {
#     purrr::map(
#       .x = seq_len(length(gp_vars)),
#       function(j) {
#         purrr::map(
#           .x = seq_len(length(par_vars)),
#           function(k) {
#             freq.loop(
#               df = i,
#               gp_var = gp_vars[j],
#               par = par_vars[k],
#               # ord = "default"
#             )
#           }
#         )
#       }
#     )
#   }
# )

# pnad doms cross-tabulations ----
# purrr::map(
#   .x = list(pnad.doms.2001, pnad.doms.2002, pnad.doms.2003, pnad.doms.2004, pnad.doms.2005,
#             pnad.doms.2006, pnad.doms.2007, pnad.doms.2008, pnad.doms.2009, pnad.doms.2011,
#             pnad.doms.2012, pnad.doms.2013, pnad.doms.2014, pnad.doms.2015),
#   function(i) {
#     purrr::map(
#       .x = seq_len(length(gp_vars)),
#       function(j) {
#         purrr::map(
#           .x = seq_len(length(par_vars)),
#           function(k) {
#             freq.loop(
#               df = i,
#               gp_var = gp_vars[j],
#               par = par_vars[k]
#             )
#           }
#         )
#       }
#     )
#   }
# )
#

# summarytools::ctable(
#   x = pnad.doms.2001$cat_tipo_domicilio,
#   y = pnad.doms.2001$renda_mensal_domiciliar_compativel_1992_deflacionado,
#   prop = "r",
#   useNA = "no",
#   totals = FALSE,
#   # headings = FALSE
# )

# pnad doms descriptive statistics ----
sink("outputs/pnad/desc-stats/doms/desc_stats.md", append = FALSE)

cat("PNAD doms")

purrr::map(
  .x = list(
    pnad.doms.2001, pnad.doms.2002, pnad.doms.2003, pnad.doms.2004, pnad.doms.2005,
    pnad.doms.2006, pnad.doms.2007, pnad.doms.2008, pnad.doms.2009, pnad.doms.2011,
    pnad.doms.2012, pnad.doms.2013, pnad.doms.2014, pnad.doms.2015
  ),
  ~ .x %>%
    # group_by(area_planejamento) %>%
    descr(stats = desc.base)
)

sink()

# data frame summary
sink("outputs/pnad/desc-stats/doms/df_summary.md", append = FALSE)

cat("PNAD doms")

purrr::map(
  .x = list(
    pnad.doms.2001, pnad.doms.2002, pnad.doms.2003, pnad.doms.2004, pnad.doms.2005,
    pnad.doms.2006, pnad.doms.2007, pnad.doms.2008, pnad.doms.2009, pnad.doms.2011,
    pnad.doms.2012, pnad.doms.2013, pnad.doms.2014, pnad.doms.2015
  ),
  ~ .x %>%
    # group_by(area_planejamento) %>%
    dfSummary()
)

sink()

# pnad doms plots ----
# barplot (for categorical variables)
purrr::walk2(
  .x = list(
    pnad.doms.2001, pnad.doms.2002, pnad.doms.2003, pnad.doms.2004, pnad.doms.2005,
    pnad.doms.2006, pnad.doms.2007, pnad.doms.2008, pnad.doms.2009
  ),
  .y = glue::glue("{2001:2009}_pnad_doms"),
  function(i, j) {
    purrr::walk(
      .x = c("cat_tipo_domicilio", "total_pessoas", "range_total_rendimentos"),
      ~ geom.bar.test.doms(
        df = i,
        var = .x,
        name_plot = j,
        # gp_var = "sexo"
      )
    )
  }
)

purrr::walk2(
  .x = list(
    pnad.doms.2011, pnad.doms.2012, pnad.doms.2013, pnad.doms.2014, pnad.doms.2015
  ),
  .y = glue::glue("{2011:2015}_pnad_doms"),
  function(i, j) {
    purrr::walk(
      .x = c("cat_tipo_domicilio", "total_pessoas", "range_total_rendimentos"),
      ~ geom.bar.test.doms(
        df = i,
        var = .x,
        name_plot = j,
        # gp_var = "sexo"
      )
    )
  }
)

# histogram (for quantitative/continuous variables)
# census.2000.doms %>%
#   ggplot() +
#   aes(x = total_rendimentos_domicilio_sm) +
#   geom_histogram(bins = 4000)

# boxplot (for quantitative/continuous variables)
# census.2000.doms %>%
#   ggplot() +
#   aes(x = total_rendimentos_domicilio_sm) +
#   geom_boxplot()

# scatterplot (for two quantitative variables (if applicable))
# census.2000.doms %>%
#   ggplot() +
#   aes(x = cat_microcomputador, y = total_rendimentos_domicilio_sm) +
#   geom_point()

# scatterplot by factor
# census.2000.doms %>%
#   ggplot() +
#   aes(x = cat_microcomputador, y = total_rendimentos_domicilio_sm, colour = area_planejamento) +
#   geom_point()

### descriptive analysis - pnad doms by year ----

# cat_tipo_domicilio ----
sink("outputs/pnad/comparisons/doms/cat_tipo_domicilio.md", append = FALSE)

cat("cat_tipo_domicilio")

purrr::map2(
  .x = list(
    pnad.doms.2001, pnad.doms.2002, pnad.doms.2003, pnad.doms.2004, pnad.doms.2005,
    pnad.doms.2006, pnad.doms.2007, pnad.doms.2008, pnad.doms.2009, pnad.doms.2011,
    pnad.doms.2012, pnad.doms.2013, pnad.doms.2014, pnad.doms.2015
  ),
  .y = c(2001:2009, 2011:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "cat_tipo_domicilio",
      ord = "freq"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "cat_tipo_domicilio") %>%
  knitr::kable()

sink()

# total_pessoas ----
sink("outputs/pnad/comparisons/doms/total_pessoas.md", append = FALSE)

cat("total_pessoas")

purrr::map2(
  .x = list(
    pnad.doms.2001, pnad.doms.2002, pnad.doms.2003, pnad.doms.2004, pnad.doms.2005,
    pnad.doms.2006, pnad.doms.2007, pnad.doms.2008, pnad.doms.2009, pnad.doms.2011,
    pnad.doms.2012, pnad.doms.2013, pnad.doms.2014, pnad.doms.2015
  ),
  .y = c(2001:2009, 2011:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "total_pessoas"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "total_pessoas") %>%
  knitr::kable()

sink()

# range_total_rendimentos ----
sink("outputs/pnad/comparisons/doms/range_total_rendimentos.md", append = FALSE)

cat("range_total_rendimentos")

purrr::map2(
  .x = list(
    pnad.doms.2001, pnad.doms.2002, pnad.doms.2003, pnad.doms.2004, pnad.doms.2005,
    pnad.doms.2006, pnad.doms.2007, pnad.doms.2008, pnad.doms.2009, pnad.doms.2011,
    pnad.doms.2012, pnad.doms.2013, pnad.doms.2014, pnad.doms.2015
  ),
  .y = c(2001:2009, 2011:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "range_total_rendimentos"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "range_total_rendimentos") %>%
  knitr::kable()

sink()

### descriptive analysis - pnad indv ----

ls(pnad.indv.2001)
str(pnad.indv.2001)

# pnad indv frequencies ----
purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ freq.test.indv(
    df = .x,
    par = "range_total_rendimentos",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ freq.test.indv(
    df = .x,
    par = "range_idade",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ freq.test.indv(
    df = .x,
    par = "anos_estudo",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ freq.test.indv(
    df = .x,
    par = "ultima_serie_frequentada",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ freq.test.indv(
    df = .x,
    par = "trabalhou_semana",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ freq.test.indv(
    df = .x,
    par = "possui_carteira_assinada",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ freq.test.indv(
    df = .x,
    par = "contribui_previdencia",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ freq.test.indv(
    df = .x,
    par = "cat_sexo",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ freq.test.indv(
    df = .x,
    par = "cat_raca_cor",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ freq.test.indv(
    df = .x,
    par = "cat_ultimo_grau_frequentado",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ freq.test.indv(
    df = .x,
    par = "cat_posicao_ocupacao",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ freq.test.indv(
    df = .x,
    par = "cat_condicao_domicilio",
    gp_var = NA,
    db_name = .y
  )
)

# gp_vars <- NA
#
# par_vars <- c("range_total_rendimentos", "range_idade", "anos_estudo", "ultima_serie_frequentada",
#               "trabalhou_semana", "possui_carteira_assinada", "contribui_previdencia",
#               "cat_sexo", "cat_raca_cor", "cat_ultimo_grau_frequentado", "cat_posicao_ocupacao",
#               "cat_condicao_domicilio")
#
# purrr::map(
#   .x = list(pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
#             pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
#             pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015),
#   function(i) {
#     purrr::map(
#       .x = seq_len(length(gp_vars)),
#       function(j) {
#         purrr::map(
#           .x = seq_len(length(par_vars)),
#           function(k) {
#             freq.loop(
#               df = i,
#               gp_var = gp_vars[j],
#               par = par_vars[k],
#               ord = "default"
#             )
#           }
#         )
#       }
#     )
#   }
# )

# pnad indv cross-tabulations ----
# trabalhou_semana
purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ ctable.test.indv(
    df = .x,
    x_var = "cat_ultimo_grau_frequentado",
    y_var = "trabalhou_semana",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ ctable.test.indv(
    df = .x,
    x_var = "range_idade",
    y_var = "trabalhou_semana",
    gp_var = NA,
    db_name = .y
  )
)

# possui_carteira_assinada
purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ ctable.test.indv(
    df = .x,
    x_var = "cat_ultimo_grau_frequentado",
    y_var = "possui_carteira_assinada",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ ctable.test.indv(
    df = .x,
    x_var = "range_idade",
    y_var = "possui_carteira_assinada",
    gp_var = NA,
    db_name = .y
  )
)

# range_horas_trabalhadas
purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ ctable.test.indv(
    df = .x,
    x_var = "cat_ultimo_grau_frequentado",
    y_var = "range_horas_trabalhadas",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ ctable.test.indv(
    df = .x,
    x_var = "range_idade",
    y_var = "range_horas_trabalhadas",
    gp_var = NA,
    db_name = .y
  )
)

# cat_sexo
purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ ctable.test.indv(
    df = .x,
    x_var = "cat_ultimo_grau_frequentado",
    y_var = "cat_sexo",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ ctable.test.indv(
    df = .x,
    x_var = "trabalhou_semana",
    y_var = "cat_sexo",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ ctable.test.indv(
    df = .x,
    x_var = "possui_carteira_assinada",
    y_var = "cat_sexo",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ ctable.test.indv(
    df = .x,
    x_var = "range_horas_trabalhadas",
    y_var = "cat_sexo",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ ctable.test.indv(
    df = .x,
    x_var = "range_total_rendimentos",
    y_var = "cat_sexo",
    gp_var = NA,
    db_name = .y
  )
)

# cat_raca_cor
purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ ctable.test.indv(
    df = .x,
    x_var = "cat_ultimo_grau_frequentado",
    y_var = "cat_raca_cor",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ ctable.test.indv(
    df = .x,
    x_var = "trabalhou_semana",
    y_var = "cat_raca_cor",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ ctable.test.indv(
    df = .x,
    x_var = "possui_carteira_assinada",
    y_var = "cat_raca_cor",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ ctable.test.indv(
    df = .x,
    x_var = "range_horas_trabalhadas",
    y_var = "cat_raca_cor",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(
    "PNAD indv 2001", "PNAD indv 2002", "PNAD indv 2003", "PNAD indv 2004",
    "PNAD indv 2005", "PNAD indv 2006", "PNAD indv 2007", "PNAD indv 2008",
    "PNAD indv 2009", "PNAD indv 2011", "PNAD indv 2012", "PNAD indv 2013",
    "PNAD indv 2014", "PNAD indv 2015"
  ),
  ~ ctable.test.indv(
    df = .x,
    x_var = "range_total_rendimentos",
    y_var = "cat_raca_cor",
    gp_var = NA,
    db_name = .y
  )
)

# pnad indv descriptive statistics ----
sink("outputs/pnad/desc-stats/indv/desc_stats_new.md", append = FALSE)

cat("PNAD indv")

purrr::map(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  ~ .x %>%
    # group_by(area_planejamento) %>%
    select(-c(
      "horas_trabalhadas_semana", "renda_aposentadoria", "renda_mensal_dinheiro",
      "renda_mensal_ocupacao_principal", "renda_pensao"
    )) %>%
    descr(stats = desc.base)
)

sink()

# data frame summary
sink("outputs/pnad/desc-stats/indv/df_summary.md", append = FALSE)

cat("PNAD indv")

purrr::map(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  ~ .x %>%
    # group_by(area_planejamento) %>%
    dfSummary()
)

sink()

# pnad indv plots ----
# barplot (for categorical variables)
purrr::walk2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009
  ),
  .y = glue::glue("{2001:2009}_pnad_indv"),
  function(i, j) {
    purrr::walk(
      .x = c(
        "cat_sexo", "cat_raca_cor", "range_total_rendimentos", "range_idade",
        "range_horas_trabalhadas", "ultima_serie_frequentada", "trabalhou_semana",
        "possui_carteira_assinada", "contribui_previdencia", "cat_ultimo_grau_frequentado",
        "cat_posicao_ocupacao", "cat_condicao_domicilio"
      ),
      ~ geom.bar.test.indv(
        df = i,
        var = .x,
        name_plot = j,
        # gp_var = "sexo"
      )
    )
  }
)

purrr::walk2(
  .x = list(
    pnad.indv.2011, pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = glue::glue("{2011:2015}_pnad_indv"),
  function(i, j) {
    purrr::walk(
      .x = c(
        "cat_sexo", "cat_raca_cor", "range_total_rendimentos", "range_idade",
        "range_horas_trabalhadas", "ultima_serie_frequentada", "trabalhou_semana",
        "possui_carteira_assinada", "contribui_previdencia", "cat_ultimo_grau_frequentado",
        "cat_posicao_ocupacao", "cat_condicao_domicilio"
      ),
      ~ geom.bar.test.indv(
        df = i,
        var = .x,
        name_plot = j,
        # gp_var = "sexo"
      )
    )
  }
)

# histogram (for quantitative/continuous variables)
# census.2000.doms %>%
#   ggplot() +
#   aes(x = total_rendimentos_domicilio_sm) +
#   geom_histogram(bins = 4000)

# boxplot (for quantitative/continuous variables)
# census.2000.doms %>%
#   ggplot() +
#   aes(x = total_rendimentos_domicilio_sm) +
#   geom_boxplot()

# scatterplot (for two quantitative variables (if applicable))
# census.2000.doms %>%
#   ggplot() +
#   aes(x = cat_microcomputador, y = total_rendimentos_domicilio_sm) +
#   geom_point()

# scatterplot by factor
# census.2000.doms %>%
#   ggplot() +
#   aes(x = cat_microcomputador, y = total_rendimentos_domicilio_sm, colour = area_planejamento) +
#   geom_point()

### descriptive analysis - pnad indv by year ----

# range_total_rendimentos ----
sink("outputs/pnad/comparisons/indv/cat_tipo_domicilio.md", append = FALSE)

cat("cat_tipo_domicilio")

purrr::map2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(2001:2009, 2011:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "range_total_rendimentos",
      ord = "default"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "range_total_rendimentos") %>%
  knitr::kable()

sink()

# range_idade ----
sink("outputs/pnad/comparisons/indv/range_idade.md", append = FALSE)

cat("range_idade")

purrr::map2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(2001:2009, 2011:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "range_idade",
      ord = "default"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "range_idade") %>%
  knitr::kable()

sink()

# range_horas_trabalhadas ----
sink("outputs/pnad/comparisons/indv/range_horas_trabalhadas.md", append = FALSE)

cat("range_horas_trabalhadas")

purrr::map2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(2001:2009, 2011:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "range_horas_trabalhadas",
      ord = "default"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "range_horas_trabalhadas") %>%
  knitr::kable()

sink()

# cat_sexo ----
sink("outputs/pnad/comparisons/indv/cat_sexo.md", append = FALSE)

cat("cat_sexo")

purrr::map2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(2001:2009, 2011:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "cat_sexo"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "cat_sexo") %>%
  knitr::kable()

sink()

# cat_raca_cor ----
sink("outputs/pnad/comparisons/indv/cat_raca_cor.md", append = FALSE)

cat("cat_raca_cor")

purrr::map2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(2001:2009, 2011:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "cat_raca_cor"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "cat_raca_cor") %>%
  knitr::kable()

sink()

# anos_estudo ----
sink("outputs/pnad/comparisons/indv/anos_estudo.md", append = FALSE)

cat("anos_estudo")

purrr::map2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(2001:2009, 2011:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "anos_estudo"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "anos_estudo") %>%
  knitr::kable()

sink()

# ultima_serie_frequentada ----
sink("outputs/pnad/comparisons/indv/ultima_serie_frequentada.md", append = FALSE)

cat("ultima_serie_frequentada")

purrr::map2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(2001:2009, 2011:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "ultima_serie_frequentada"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "ultima_serie_frequentada") %>%
  knitr::kable()

sink()

# trabalhou_semana ----
sink("outputs/pnad/comparisons/indv/trabalhou_semana.md", append = FALSE)

cat("trabalhou_semana")

purrr::map2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(2001:2009, 2011:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "trabalhou_semana"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "trabalhou_semana") %>%
  knitr::kable()

sink()

# possui_carteira_assinada ----
sink("outputs/pnad/comparisons/indv/possui_carteira_assinada.md", append = FALSE)

cat("possui_carteira_assinada")

purrr::map2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(2001:2009, 2011:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "possui_carteira_assinada"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "possui_carteira_assinada") %>%
  knitr::kable()

sink()

# contribui_previdencia ----
sink("outputs/pnad/comparisons/indv/contribui_previdencia.md", append = FALSE)

cat("contribui_previdencia")

purrr::map2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(2001:2009, 2011:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "contribui_previdencia"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "contribui_previdencia") %>%
  knitr::kable()

sink()

# cat_ultimo_grau_frequentado ----
sink("outputs/pnad/comparisons/indv/cat_ultimo_grau_frequentado.md", append = FALSE)

cat("cat_ultimo_grau_frequentado")

purrr::map2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(2001:2009, 2011:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "cat_ultimo_grau_frequentado"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "cat_ultimo_grau_frequentado") %>%
  knitr::kable()

sink()

# cat_posicao_ocupacao ----
sink("outputs/pnad/comparisons/indv/cat_posicao_ocupacao.md", append = FALSE)

cat("cat_posicao_ocupacao")

purrr::map2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(2001:2009, 2011:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "cat_posicao_ocupacao"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "cat_posicao_ocupacao") %>%
  knitr::kable()

sink()

# cat_condicao_domicilio ----
sink("outputs/pnad/comparisons/indv/cat_condicao_domicilio.md", append = FALSE)

cat("cat_condicao_domicilio")

purrr::map2(
  .x = list(
    pnad.indv.2001, pnad.indv.2002, pnad.indv.2003, pnad.indv.2004, pnad.indv.2005,
    pnad.indv.2006, pnad.indv.2007, pnad.indv.2008, pnad.indv.2009, pnad.indv.2011,
    pnad.indv.2012, pnad.indv.2013, pnad.indv.2014, pnad.indv.2015
  ),
  .y = c(2001:2009, 2011:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "cat_condicao_domicilio"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "cat_condicao_domicilio") %>%
  knitr::kable()

sink()
