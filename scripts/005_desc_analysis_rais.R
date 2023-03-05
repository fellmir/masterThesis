### beginning ----

source("scripts/001_libraries.R")

desc.base <- c("mean", "sd", "min", "q1", "med", "q3", "max", "n.valid", "pct.valid")

ano.ex <- as.character(c(seq(2003, 2004)))

ano.complete <- as.character(c(seq(2003, 2014)))

ano.raca_cor <- as.character(c(seq(2006, 2014)))

ano.1 <- as.character(c(seq(2003, 2005)))

ano.2 <- as.character(c(seq(2006, 2008)))

ano.3 <- as.character(c(seq(2009, 2011)))

ano.4 <- as.character(c(seq(2012, 2014)))

fs::dir_create("outputs/rais")
fs::dir_create("outputs/rais/freqs/estab")
fs::dir_create("outputs/rais/freqs/vinc")
fs::dir_create("outputs/rais/cross-tabs/estab")
fs::dir_create("outputs/rais/cross-tabs/vinc")
fs::dir_create("outputs/rais/desc-stats/estab")
fs::dir_create("outputs/rais/desc-stats/vinc")
fs::dir_create("outputs/rais/plots/estab")
fs::dir_create("outputs/rais/plots/vinc")
fs::dir_create("outputs/rais/comparisons/estab")
fs::dir_create("outputs/rais/comparisons/vinc")

### loading rais dbs ----

rais.estab.arrow <- fs::dir_ls("db/rais/estab/", regexp = "rais(.*)\\.parquet") %>%
  arrow::open_dataset()

rais.vinc.arrow <- fs::dir_ls("db/rais/vinc/", regexp = "rais(.*)\\.parquet") %>%
  arrow::open_dataset()

# estab ----
# rais.estab.2000 <- readRDS("db/rais/estab/rais_estab_2000.rds")
# rais.estab.2001 <- readRDS("db/rais/estab/rais_estab_2001.rds")
# rais.estab.2002 <- readRDS("db/rais/estab/rais_estab_2002.rds")
rais.estab.2003 <- readRDS("db/rais/estab/rais_estab_2003.rds")
rais.estab.2004 <- readRDS("db/rais/estab/rais_estab_2004.rds")
rais.estab.2005 <- readRDS("db/rais/estab/rais_estab_2005.rds")
rais.estab.2006 <- readRDS("db/rais/estab/rais_estab_2006.rds")
rais.estab.2007 <- readRDS("db/rais/estab/rais_estab_2007.rds")
rais.estab.2008 <- readRDS("db/rais/estab/rais_estab_2008.rds")
rais.estab.2009 <- readRDS("db/rais/estab/rais_estab_2009.rds")
rais.estab.2010 <- readRDS("db/rais/estab/rais_estab_2010.rds")
rais.estab.2011 <- readRDS("db/rais/estab/rais_estab_2011.rds")
rais.estab.2012 <- readRDS("db/rais/estab/rais_estab_2012.rds")
rais.estab.2013 <- readRDS("db/rais/estab/rais_estab_2013.rds")
rais.estab.2014 <- readRDS("db/rais/estab/rais_estab_2014.rds")
# rais.estab.2015 <- readRDS("db/rais/estab/rais_estab_2015.rds")

# vinc ----
# rais.vinc.2000 <- readRDS("db/rais/vinc/rais_vinc_2000.rds")
# rais.vinc.2001 <- readRDS("db/rais/vinc/rais_vinc_2001.rds")
# rais.vinc.2002 <- readRDS("db/rais/vinc/rais_vinc_2002.rds")
# rais.vinc.2003 <- readRDS("db/rais/vinc/rais_vinc_2003.rds")
# rais.vinc.2004 <- readRDS("db/rais/vinc/rais_vinc_2004.rds")
# rais.vinc.2005 <- readRDS("db/rais/vinc/rais_vinc_2005.rds")
# rais.vinc.2006 <- readRDS("db/rais/vinc/rais_vinc_2006.rds")
# rais.vinc.2007 <- readRDS("db/rais/vinc/rais_vinc_2007.rds")
# rais.vinc.2008 <- readRDS("db/rais/vinc/rais_vinc_2008.rds")
# rais.vinc.2009 <- readRDS("db/rais/vinc/rais_vinc_2009.rds")
# rais.vinc.2010 <- readRDS("db/rais/vinc/rais_vinc_2010.rds")
# rais.vinc.2011 <- readRDS("db/rais/vinc/rais_vinc_2011.rds")
# rais.vinc.2012 <- readRDS("db/rais/vinc/rais_vinc_2012.rds")
# rais.vinc.2013 <- readRDS("db/rais/vinc/rais_vinc_2013.rds")
# rais.vinc.2014 <- readRDS("db/rais/vinc/rais_vinc_2014.rds")
# rais.vinc.2015 <- readRDS("db/rais/vinc/rais_vinc_2015.rds")

### functions ----
# frequency table function for multiple dataframes
freq.loop <- function(df, gp_var = NA, par, ord = "default", db_name = NULL) {
  par <- as.symbol(par)

  if (is.na(gp_var)) {
    col <- df %>%
      dplyr::select({{ par }})

    if (!is.null(db_name)) {
      cat(glue::glue("\n\n## Database = {db_name}\n\n"))

      print(
        summarytools::freq(
          col,
          report.nas = FALSE,
          order = ord,
          cumul = FALSE,
          style = "rmarkdown",
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
        style = "rmarkdown",
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
      cat(glue::glue("\n\n## Database = {db_name}\n\n"))

      print(
        summarytools::freq(
          col,
          report.nas = FALSE,
          order = ord,
          cumul = FALSE,
          style = "rmarkdown",
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
        style = "rmarkdown",
        # headings = FALSE
      )

      return(x)
    }
  }
}

# cross-tabulation function for multiple dataframes
# ctable.loop <- function(df, x_var, y_var, gp_var = NA, db_name = NA) {
#   if (is.na(gp_var)) {
#     cols <- df %>%
#       dplyr::select(tidyselect::all_of(c(x_var, y_var)))
#
#     cat(glue::glue("\n\nDatabase = {db_name}\n\n"))
#
#     print(
#       summarytools::ctable(
#         x = cols[[1]],
#         y = cols[[2]],
#         prop = "r",
#         useNA = "no",
#         totals = FALSE,
#         dnn = c(x_var, y_var)
#         # headings = FALSE
#       )
#     )
#   } else {
#     gp_var_sym <- as.symbol(gp_var)
#
#     cols <- df %>%
#       dplyr::select(tidyselect::all_of(c(x_var, y_var)), {{ gp_var_sym }}) %>%
#       dplyr::group_by({{ gp_var_sym }}) %>%
#       tidyr::nest() %>%
#       dplyr::arrange({{ gp_var_sym }})
#
#     purrr::walk(
#       seq_len(nrow(cols)),
#       function(i) {
#         cat(glue::glue("\n\nDatabase = {db_name}\nGroup = {cols[[1]][i]}\n\n"))
#
#         print(
#           summarytools::ctable(
#             x = cols$data[[i]][[x_var]],
#             y = cols$data[[i]][[y_var]],
#             dnn = c(x_var, y_var),
#             prop = "r",
#             useNA = "no",
#             totals = FALSE
#           )
#         )
#       }
#     )
#   }
# }

# bar plot show
# geom.bar.fun <- function(df, var, gp_var = NA, name_plot) {
#   var_sym <- as.symbol(var)
#
#   if (is.na(gp_var)) {
#     plot_var <- df %>%
#       tidyr::drop_na({{ var_sym }}) %>%
#       ggplot() +
#       aes(x = {{ var_sym }}) %>%
#       geom_bar()
#   } else {
#     gp_var_sym <- as.symbol(gp_var)
#
#     plot_var <- df %>%
#       tidyr::drop_na({{ var_sym }}) %>%
#       ggplot() +
#       aes(
#         x = {{ var_sym }},
#         fill = {{ gp_var_sym }}
#       ) %>%
#       geom_bar()
#   }
#
#   plot_var <- plot_var +
#     ggtitle(glue::glue("{name_plot}_{var}"))
# }

# bar plot saving
geom.bar.test.estab <- function(df, var, gp_var = NA, name_plot) {
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
    width = 3840,
    height = 2160,
    units = "px",
    type = "cairo",
    filename = glue::glue("outputs/rais/plots/estab/{name_plot}_{var}.png")
  )
}

geom.bar.test.vinc <- function(df, var, gp_var = NA, name_plot) {
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
    width = 3840,
    height = 2160,
    units = "px",
    type = "cairo",
    filename = glue::glue("outputs/rais/plots/vinc/{name_plot}_{var}.png")
  )
}

# frequency saving (in .md)
freq.test.estab <- function(df, gp_var = NA, par, ord = "default", db_name = NA) {
  par <- as.symbol(par)

  if (is.na(gp_var)) {
    col <- df %>%
      dplyr::select({{ par }})

    sink(glue::glue("outputs/rais/freqs/estab/{db_name}_{par}.md"), append = FALSE)

    cat(glue::glue("\n\n## Database = {db_name}\n\n"))

    print(
      summarytools::freq(
        col,
        report.nas = FALSE,
        order = ord,
        cumul = FALSE,
        style = "rmarkdown",
        # headings = FALSE
      )
    )

    sink()
  } else {
    gp_var <- as.symbol(gp_var)

    cols <- df %>%
      dplyr::group_by({{ gp_var }}) %>%
      dplyr::select({{ par }})

    sink(glue::glue("outputs/rais/freqs/estab/{db_name}_{par}_{gp_var}.md"), append = FALSE)

    cat(glue::glue("\n\n## Database = {db_name}\n\n"))

    print(
      summarytools::freq(
        col,
        report.nas = FALSE,
        order = ord,
        cumul = FALSE,
        style = "rmarkdown",
        # headings = FALSE
      )
    )

    sink()
  }
}

freq.test.vinc <- function(df, gp_var = NA, par, ord = "default", db_name = NA) {
  par <- as.symbol(par)

  if (is.na(gp_var)) {
    col <- df %>%
      dplyr::select({{ par }})

    sink(glue::glue("outputs/rais/freqs/vinc/{db_name}_{par}.md"), append = FALSE)

    cat(glue::glue("\n\n## Database = {db_name}\n\n"))

    print(
      summarytools::freq(
        col,
        report.nas = FALSE,
        order = ord,
        cumul = FALSE,
        style = "rmarkdown",
        # headings = FALSE
      )
    )

    sink()
  } else {
    gp_var <- as.symbol(gp_var)

    cols <- df %>%
      dplyr::group_by({{ gp_var }}) %>%
      dplyr::select({{ par }})

    sink(glue::glue("outputs/rais/freqs/vinc/{db_name}_{par}_{gp_var}.md"), append = FALSE)

    cat(glue::glue("\n\n## Database = {db_name}\n\n"))

    print(
      summarytools::freq(
        col,
        report.nas = FALSE,
        order = ord,
        cumul = FALSE,
        style = "rmarkdown",
        # headings = FALSE
      )
    )

    sink()
  }
}

freq.loop.v2 <- function(df, gp_var = NA, par, ord = "default", db_name = NULL) {
  par <- as.symbol(par)

  if (is.na(gp_var)) {
    col <- df %>%
      dplyr::select({{ par }})

    if (!is.null(db_name)) {
      sink(glue::glue("outputs/rais/freqs/estab/{db_name}_{par}.md"), append = FALSE)

      cat(glue::glue("\n\n## Database = {db_name}\n\n"))

      print(
        summarytools::freq(
          col,
          report.nas = FALSE,
          order = ord,
          cumul = FALSE,
          style = "rmarkdown",
          # headings = FALSE
        ),
        # caption = db_name # mudar?
      )

      sink()
    } else {
      sink(glue::glue("outputs/rais/freqs/estab/{db_name}_{par}.md"), append = FALSE)

      x <- summarytools::freq(
        col,
        report.nas = FALSE,
        order = ord,
        cumul = FALSE,
        style = "rmarkdown",
        # headings = FALSE
      )

      sink()

      return(x)
    }
  } else {
    gp_var <- as.symbol(gp_var)

    col <- df %>%
      dplyr::group_by({{ gp_var }}) %>%
      dplyr::select({{ par }})

    if (!is.null(db_name)) {
      sink(glue::glue("outputs/rais/freqs/estab/{db_name}_{par}_{gp_var}.md"), append = FALSE)

      cat(glue::glue("\n\n## Database = {db_name}\n\n"))

      print(
        summarytools::freq(
          col,
          report.nas = FALSE,
          order = ord,
          cumul = FALSE,
          style = "rmarkdown",
          # headings = FALSE
        ),
        # caption = db_name # mudar?
      )

      sink()
    } else {
      sink(glue::glue("outputs/rais/freqs/estab/{db_name}_{par}_{gp_var}.md"), append = FALSE)

      x <- summarytools::freq(
        col,
        report.nas = FALSE,
        order = ord,
        cumul = FALSE,
        style = "rmarkdown",
        # headings = FALSE
      )

      sink()

      return(x)
    }
  }
}

freq.loop.v3 <- function(df, gp_var = NA, par, ord = "default", db_name = NULL) {
  par <- as.symbol(par)

  if (is.na(gp_var)) {
    col <- df %>%
      dplyr::select({{ par }})

    if (!is.null(db_name)) {
      sink(glue::glue("outputs/rais/freqs/vinc/{db_name}_{par}.md"), append = FALSE)

      cat(glue::glue("\n\n## Database = {db_name}\n\n"))

      print(
        summarytools::freq(
          col,
          report.nas = FALSE,
          order = ord,
          cumul = FALSE,
          style = "rmarkdown",
          # headings = FALSE
        ),
        # caption = db_name # mudar?
      )

      sink()
    } else {
      sink(glue::glue("outputs/rais/freqs/vinc/{db_name}_{par}.md"), append = FALSE)

      cat(glue::glue("\n\n## Database = {db_name}\n\n"))

      x <- summarytools::freq(
        col,
        report.nas = FALSE,
        order = ord,
        cumul = FALSE,
        style = "rmarkdown",
        # headings = FALSE
      )

      sink()

      return(x)
    }
  } else {
    gp_var <- as.symbol(gp_var)

    col <- df %>%
      dplyr::group_by({{ gp_var }}) %>%
      dplyr::select({{ par }})

    if (!is.null(db_name)) {
      sink(glue::glue("outputs/rais/freqs/vinc/{db_name}_{par}_{gp_var}.md"), append = FALSE)

      cat(glue::glue("\n\n## Database = {db_name}\n\n"))

      print(
        summarytools::freq(
          col,
          report.nas = FALSE,
          order = ord,
          cumul = FALSE,
          style = "rmarkdown",
          # headings = FALSE
        ),
        # caption = db_name # mudar?
      )

      sink()
    } else {
      sink(glue::glue("outputs/rais/freqs/vinc/{db_name}_{par}_{gp_var}.md"), append = FALSE)

      cat(glue::glue("\n\n## Database = {db_name}\n\n"))

      x <- summarytools::freq(
        col,
        report.nas = FALSE,
        order = ord,
        cumul = FALSE,
        style = "rmarkdown",
        # headings = FALSE
      )

      sink()

      return(x)
    }
  }
}

# cross-tabulation saving (in .md)
ctable.test.estab <- function(df, x_var, y_var, gp_var = NA, db_name = NA) {
  if (is.na(gp_var)) {
    cols <- df %>%
      dplyr::select(tidyselect::all_of(c(x_var, y_var)))

    sink(glue::glue("outputs/rais/cross-tabs/estab/{db_name}_{x_var}_{y_var}.md"), append = FALSE)

    cat(glue::glue("\n\n## Database = {db_name}\n\n"))

    print(
      summarytools::ctable(
        x = cols[[1]],
        y = cols[[2]],
        prop = "r",
        useNA = "no",
        style = "rmarkdown",
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

    sink(glue::glue("outputs/rais/cross-tabs/estab/{db_name}_{x_var}_{y_var}_{gp_var}.md"), append = FALSE)

    purrr::walk(
      seq_len(nrow(cols)),
      function(i) {
        cat(glue::glue("\n\n## Database = {db_name}\nGroup = {cols[[1]][i]}\n\n"))

        print(
          summarytools::ctable(
            x = cols$data[[i]][[x_var]],
            y = cols$data[[i]][[y_var]],
            dnn = c(x_var, y_var),
            prop = "r",
            useNA = "no",
            style = "rmarkdown",
            totals = FALSE
          )
        )
      }
    )

    sink()
  }
}

ctable.test.vinc <- function(df, x_var, y_var, gp_var = NA, db_name = NA) {
  if (is.na(gp_var)) {
    cols <- df %>%
      dplyr::select(tidyselect::all_of(c(x_var, y_var)))

    sink(glue::glue("outputs/rais/cross-tabs/vinc/{db_name}_{x_var}_{y_var}.md"), append = FALSE)

    cat(glue::glue("\n\n## Database = {db_name}\n\n"))

    print(
      summarytools::ctable(
        x = cols[[1]],
        y = cols[[2]],
        prop = "r",
        useNA = "no",
        style = "rmarkdown",
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

    sink(glue::glue("outputs/rais/cross-tabs/vinc/{db_name}_{x_var}_{y_var}_{gp_var}.md"), append = FALSE)

    purrr::walk(
      seq_len(nrow(cols)),
      function(i) {
        cat(glue::glue("\n\n## Database = {db_name}\nGroup = {cols[[1]][i]}\n\n"))

        print(
          summarytools::ctable(
            x = cols$data[[i]][[x_var]],
            y = cols$data[[i]][[y_var]],
            dnn = c(x_var, y_var),
            prop = "r",
            useNA = "no",
            style = "rmarkdown",
            totals = FALSE
          )
        )
      }
    )

    sink()
  }
}

# function examples ----
# parquet use
rais.estab.arrow %>%
  dplyr::select(area_planejamento, cat_natureza_juridica, cat_tamanho, ano) %>%
  dplyr::filter(ano %in% ano.ex) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c("RAIS estab 2003", "RAIS estab 2004"),
    ~ ctable.test.estab(
      df = .x,
      x_var = "cat_natureza_juridica",
      y_var = "cat_tamanho",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

# testing new freq function
gp_vars <- c("area_planejamento")

par_vars <- c(
  "range_qtd_vinc_ativos", "range_qtd_vinc_clt", "range_qtd_vinc_estat",
  "indicador_cei_vinculado", "indicador_simples", "indicador_rais_negativa",
  "cat_tamanho", "cat_natureza_juridica", "cat_tipo"
)

purrr::walk2(
  .x = list(rais.estab.2003, rais.estab.2004),
  .y = c("RAIS estab 2003", "RAIS estab 2004"),
  function(i, j) {
    purrr::walk(
      .x = seq_len(length(gp_vars)),
      function(k) {
        purrr::map(
          .x = seq_len(length(par_vars)),
          function(l) {
            freq.loop.v2(
              df = i,
              db_name = j,
              gp_var = gp_vars[k],
              par = par_vars[l],
              ord = "default"
            )
          }
        )
      }
    )
  }
)

# testing above w/ "arrow" framework
gp_vars <- NA

par_vars <- c(
  "ano", "range_qtd_vinc_ativos", "range_qtd_vinc_clt", "range_qtd_vinc_estat",
  "indicador_cei_vinculado", "indicador_simples", "indicador_rais_negativa",
  "cat_tamanho", "cat_natureza_juridica", "cat_tipo"
)

rais.estab.arrow %>%
  dplyr::select(all_of(par_vars)) %>%
  dplyr::filter(ano %in% ano.ex) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c("RAIS estab 2003", "RAIS estab 2004"),
    function(i, j) {
      purrr::walk(
        .x = seq_len(length(gp_vars)),
        function(k) {
          purrr::map(
            .x = seq_len(length(par_vars)),
            function(l) {
              freq.loop.v2(
                df = i,
                db_name = j,
                gp_var = gp_vars[k],
                par = par_vars[l],
                ord = "default"
              )
            }
          )
        }
      )
    }
  )

# freq function w/ geo division
gp_vars <- "area_planejamento"

par_vars <- c(
  "ano", "indicador_cei_vinculado", "indicador_simples",
  "indicador_rais_negativa", "cat_tamanho", "cat_natureza_juridica",
  "cat_tipo", "area_planejamento"
)

rais.estab.arrow %>%
  dplyr::select(all_of(par_vars)) %>%
  dplyr::filter(ano %in% c("2003", "2004")) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c("RAIS estab 2003", "RAIS estab 2004"),
    function(i, j) {
      purrr::walk(
        .x = gp_vars,
        function(k) {
          purrr::map(
            par_vars[!(par_vars %in% gp_vars)],
            function(l) {
              freq.loop.v2(
                df = i,
                db_name = j,
                gp_var = k,
                par = l
              )
            }
          )
        }
      )
    }
  )

# cross-tab w/ "arrow" framework
rais.vinc.arrow %>%
  dplyr::select(cat_sexo, cat_nacionalidade, ano) %>%
  dplyr::filter(ano %in% ano.ex) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_nacionalidade",
      y_var = "cat_sexo",
      gp_var = NA,
      db_name = .y
    )
  )

# frequency table example (for multiple dataframes and ONE variable)
purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004
  ),
  .y = c("RAIS estab 2003", "RAIS estab 2004"),
  ~ freq.test.estab(
    df = .x,
    par = "indicador_cei_vinculado",
    gp_var = NA,
    db_name = .y
  )
)

# frequency table example (for all possible combinations)
gp_vars <- NA

par_vars <- c("indicador_cei_vinculado", "range_total_rendimentos")

purrr::map(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
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
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  ~ .x %>%
    # group_by(area_planejamento) %>%
    freq()
)

# cross-tabulation example (for one db and two variables)
ctable.loop(
  df = rais.estab.2003,
  x_var = "indicador_cei_vinculado",
  y_var = "cat_tipo_domicilio",
  # gp_var = "area_planejamento"
)


purrr::walk2(
  .x = list(rais.estab.2003, rais.estab.2004),
  .y = c("RAIS estab 2001", "RAIS estab 2002"),
  ~ ctable.loop(
    df = .x,
    x_var = "indicador_cei_vinculado",
    y_var = "cat_tipo_domicilio",
    # gp_var = "area_planejamento",
    db_name = .y
  )
)

# descriptive statistics example (for multiple databases)
purrr::map(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  ~ .x %>%
    # group_by(area_planejamento) %>%
    descr(stats = desc.base)
)

# bar plot example (for multiple databases)
purrr::map(
  list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  function(i) {
    purrr::map(
      c("indicador_cei_vinculado", "range_total_rendimentos"),
      ~ geom.bar.fun(
        df = i,
        var = .x,
        # gp_var = "sexo"
      )
    )
  }
)

# frequency comparison between multiple databases (w/o geo division)
purrr::map2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
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
purrr::map2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(2000, 2010),
  function(i, j) {
    x <- freq.loop(
      df = i,
      gp_var = "area_planejamento",
      par = "cat_microcomputador"
    ) %>%
      summarytools::tb()
    # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for big dataframes

    names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = c("area_planejamento", "cat_microcomputador"))
# %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for small dataframes

### descriptive analysis - rais estab ----

ls(rais.estab.2003)
str(rais.estab.2003)

# rais estab frequencies ----
purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ freq.test.estab(
    df = .x,
    par = "range_qtd_vinc_ativos",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ freq.test.estab(
    df = .x,
    par = "range_qtd_vinc_clt",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ freq.test.estab(
    df = .x,
    par = "range_qtd_vinc_estat",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ freq.test.estab(
    df = .x,
    par = "indicador_cei_vinculado",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ freq.test.estab(
    df = .x,
    par = "indicador_simples",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ freq.test.estab(
    df = .x,
    par = "indicador_rais_negativa",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ freq.test.estab(
    df = .x,
    par = "cat_tamanho",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ freq.test.estab(
    df = .x,
    par = "cat_natureza_juridica",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ freq.test.estab(
    df = .x,
    par = "cat_tipo",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ freq.test.estab(
    df = .x,
    par = "area_planejamento",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ freq.test.estab(
    df = .x,
    par = "regiao_planejamento",
    gp_var = NA,
    db_name = .y
  )
)

# gp_vars <- NA
#
# par_vars <- c("range_qtd_vinc_ativos", "range_qtd_vinc_clt", "range_qtd_vinc_estat",
#               "indicador_cei_vinculado", "indicador_simples", "indicador_rais_negativa",
#               "cat_tamanho", "cat_natureza_juridica", "cat_tipo", "area_planejamento",
#               "regiao_planejamento")
#
# purrr::map(
#   .x = list(rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
#             rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
#             rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014),
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

# by area_planejamento ----
gp_vars <- c("area_planejamento")

par_vars <- c(
  "range_qtd_vinc_ativos", "range_qtd_vinc_clt", "range_qtd_vinc_estat",
  "indicador_cei_vinculado", "indicador_simples", "indicador_rais_negativa",
  "cat_tamanho", "cat_natureza_juridica", "cat_tipo"
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  function(i, j) {
    purrr::walk(
      .x = seq_len(length(gp_vars)),
      function(k) {
        purrr::map(
          .x = seq_len(length(par_vars)),
          function(l) {
            freq.loop.v2(
              df = i,
              db_name = j,
              gp_var = gp_vars[k],
              par = par_vars[l],
              ord = "default"
            )
          }
        )
      }
    )
  }
)


# by regiao_planejamento ----
gp_vars <- c("regiao_planejamento")

par_vars <- c(
  "range_qtd_vinc_ativos", "range_qtd_vinc_clt", "range_qtd_vinc_estat",
  "indicador_cei_vinculado", "indicador_simples", "indicador_rais_negativa",
  "cat_tamanho", "cat_natureza_juridica", "cat_tipo"
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  function(i, j) {
    purrr::walk(
      .x = seq_len(length(gp_vars)),
      function(k) {
        purrr::map(
          .x = seq_len(length(par_vars)),
          function(l) {
            freq.loop.v2(
              df = i,
              db_name = j,
              gp_var = gp_vars[k],
              par = par_vars[l],
              ord = "default"
            )
          }
        )
      }
    )
  }
)

# rais estab cross-tabulations ----
# cat_tamanho
purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ ctable.test.estab(
    df = .x,
    x_var = "cat_natureza_juridica",
    y_var = "cat_tamanho",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ ctable.test.estab(
    df = .x,
    x_var = "cat_tipo",
    y_var = "cat_tamanho",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ ctable.test.estab(
    df = .x,
    x_var = "area_planejamento",
    y_var = "cat_tamanho",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ ctable.test.estab(
    df = .x,
    x_var = "regiao_planejamento",
    y_var = "cat_tamanho",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ ctable.test.estab(
    df = .x,
    x_var = "range_qtd_vinc_ativos",
    y_var = "cat_tamanho",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ ctable.test.estab(
    df = .x,
    x_var = "range_qtd_vinc_clt",
    y_var = "cat_tamanho",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ ctable.test.estab(
    df = .x,
    x_var = "range_qtd_vinc_estat",
    y_var = "cat_tamanho",
    gp_var = NA,
    db_name = .y
  )
)

# cat_natureza_juridica
purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ ctable.test.estab(
    df = .x,
    x_var = "area_planejamento",
    y_var = "cat_natureza_juridica",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ ctable.test.estab(
    df = .x,
    x_var = "regiao_planejamento",
    y_var = "cat_natureza_juridica",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ ctable.test.estab(
    df = .x,
    x_var = "range_qtd_vinc_ativos",
    y_var = "cat_natureza_juridica",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ ctable.test.estab(
    df = .x,
    x_var = "range_qtd_vinc_clt",
    y_var = "cat_natureza_juridica",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ ctable.test.estab(
    df = .x,
    x_var = "range_qtd_vinc_estat",
    y_var = "cat_natureza_juridica",
    gp_var = NA,
    db_name = .y
  )
)

# area_planejamento
purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ ctable.test.estab(
    df = .x,
    x_var = "range_qtd_vinc_ativos",
    y_var = "area_planejamento",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ ctable.test.estab(
    df = .x,
    x_var = "range_qtd_vinc_clt",
    y_var = "area_planejamento",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ ctable.test.estab(
    df = .x,
    x_var = "range_qtd_vinc_estat",
    y_var = "area_planejamento",
    gp_var = NA,
    db_name = .y
  )
)

# regiao_planejamento
purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ ctable.test.estab(
    df = .x,
    x_var = "range_qtd_vinc_ativos",
    y_var = "regiao_planejamento",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ ctable.test.estab(
    df = .x,
    x_var = "range_qtd_vinc_clt",
    y_var = "regiao_planejamento",
    gp_var = NA,
    db_name = .y
  )
)

purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(
    "RAIS estab 2003", "RAIS estab 2004", "RAIS estab 2005", "RAIS estab 2006",
    "RAIS estab 2007", "RAIS estab 2008", "RAIS estab 2009", "RAIS estab 2010",
    "RAIS estab 2011", "RAIS estab 2012", "RAIS estab 2013", "RAIS estab 2014"
  ),
  ~ ctable.test.estab(
    df = .x,
    x_var = "range_qtd_vinc_estat",
    y_var = "regiao_planejamento",
    gp_var = NA,
    db_name = .y
  )
)

# rais estab descriptive statistics ----
sink("outputs/rais/desc-stats/estab/desc_stats.md", append = FALSE)

cat("## RAIS estab")

purrr::map(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  ~ .x %>%
    # group_by(area_planejamento) %>%
    select(-c(
      "AP", "indicador_atividade_ano", "indicador_cei_vinculado",
      "indicador_rais_negativa", "indicador_simples", "RP"
    )) %>%
    descr(
      stats = desc.base,
      style = "rmarkdown"
    )
)

sink()

# by area_planejamento
sink("outputs/rais/desc-stats/estab/desc_stats-AP.md", append = FALSE)

cat("## RAIS estab by AP")

purrr::map(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  ~ .x %>%
    group_by(area_planejamento) %>%
    select(-c(
      "AP", "indicador_atividade_ano", "indicador_cei_vinculado",
      "indicador_rais_negativa", "indicador_simples", "RP"
    )) %>%
    descr(
      stats = desc.base,
      style = "rmarkdown"
    )
)

sink()

# by regiao_planejamento
sink("outputs/rais/desc-stats/estab/desc_stats-RP.md", append = FALSE)

cat("## RAIS estab by RP")

purrr::map(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  ~ .x %>%
    group_by(regiao_planejamento) %>%
    select(-c(
      "AP", "indicador_atividade_ano", "indicador_cei_vinculado",
      "indicador_rais_negativa", "indicador_simples", "RP"
    )) %>%
    descr(
      stats = desc.base,
      style = "rmarkdown"
    )
)

sink()

# data frame summary
sink("outputs/rais/desc-stats/estab/df_summary.md", append = FALSE)

cat("## RAIS estab")

purrr::map(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  ~ .x %>%
    # group_by(area_planejamento) %>%
    dfSummary()
)

sink()

# by area_planejamento
sink("outputs/rais/desc-stats/estab/df_summary-AP.md", append = FALSE)

cat("## RAIS estab by AP")

purrr::map(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  ~ .x %>%
    group_by(area_planejamento) %>%
    dfSummary()
)

sink()

# by regiao_planejamento
sink("outputs/rais/desc-stats/estab/df_summary-RP.md", append = FALSE)

cat("## RAIS estab by RP")

purrr::map(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  ~ .x %>%
    group_by(regiao_planejamento) %>%
    dfSummary()
)

sink()

# rais estab plots ----
# barplot (for categorical variables)
purrr::walk2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = glue::glue("{2003:2014}_rais_estab"),
  function(i, j) {
    purrr::walk(
      .x = c(
        "indicador_cei_vinculado", "indicador_simples", "indicador_rais_negativa",
        "indicador_atividade_ano", "cat_tamanho", "cat_natureza_juridica", "cat_tipo",
        "area_planejamento", "regiao_planejamento", "range_qtd_vinc_ativos",
        "range_qtd_vinc_clt", "range_qtd_vinc_estat"
      ),
      ~ geom.bar.test.estab(
        df = i,
        var = .x,
        name_plot = j
        # gp_var = "sexo"
      )
    )
  }
)

# histogram (for quantitative/continuous variables)
# census.2000.estab %>%
#   ggplot() +
#   aes(x = total_rendimentos_domicilio_sm) +
#   geom_histogram(bins = 4000)

# boxplot (for quantitative/continuous variables)
# census.2000.estab %>%
#   ggplot() +
#   aes(x = total_rendimentos_domicilio_sm) +
#   geom_boxplot()

# scatterplot (for two quantitative variables (if applicable))
# census.2000.estab %>%
#   ggplot() +
#   aes(x = cat_microcomputador, y = total_rendimentos_domicilio_sm) +
#   geom_point()

# scatterplot by factor
# census.2000.estab %>%
#   ggplot() +
#   aes(x = cat_microcomputador, y = total_rendimentos_domicilio_sm, colour = area_planejamento) +
#   geom_point()

### descriptive analysis - rais estab by year ----

# range_qtd_vinc_ativos ----
sink("outputs/rais/comparisons/estab/range_qtd_vinc_ativos.md", append = FALSE)

cat("range_qtd_vinc_ativos")

purrr::map2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(2003:2014),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "range_qtd_vinc_ativos",
      ord = "default"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "range_qtd_vinc_ativos") %>%
  knitr::kable()

sink()

# range_qtd_vinc_clt ----
sink("outputs/rais/comparisons/estab/range_qtd_vinc_clt.md", append = FALSE)

cat("range_qtd_vinc_clt")

purrr::map2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(2003:2014),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "range_qtd_vinc_clt",
      ord = "default"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "range_qtd_vinc_clt") %>%
  knitr::kable()

sink()

# range_qtd_vinc_estat ----
sink("outputs/rais/comparisons/estab/range_qtd_vinc_estat.md", append = FALSE)

cat("range_qtd_vinc_estat")

purrr::map2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(2003:2014),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "range_qtd_vinc_estat",
      ord = "default"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "range_qtd_vinc_estat") %>%
  knitr::kable()

sink()

# indicador_cei_vinculado ----
sink("outputs/rais/comparisons/estab/indicador_cei_vinculado.md", append = FALSE)

cat("indicador_cei_vinculado")

purrr::map2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(2003:2014),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "indicador_cei_vinculado",
      ord = "default"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "indicador_cei_vinculado") %>%
  knitr::kable()

sink()

# indicador_simples ----
sink("outputs/rais/comparisons/estab/indicador_simples.md", append = FALSE)

cat("indicador_simples")

purrr::map2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(2003:2014),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "indicador_simples",
      ord = "default"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "indicador_simples") %>%
  knitr::kable()

sink()

# indicador_rais_negativa ----
sink("outputs/rais/comparisons/estab/indicador_rais_negativa.md", append = FALSE)

cat("indicador_rais_negativa")

purrr::map2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(2003:2014),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "indicador_rais_negativa",
      ord = "default"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "indicador_rais_negativa") %>%
  knitr::kable()

sink()

# indicador_atividade_ano ----
sink("outputs/rais/comparisons/estab/indicador_atividade_ano.md", append = FALSE)

cat("indicador_atividade_ano")

purrr::map2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(2003:2014),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "indicador_atividade_ano",
      ord = "default"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "indicador_atividade_ano") %>%
  knitr::kable()

sink()

# cat_tamanho ----
sink("outputs/rais/comparisons/estab/cat_tamanho.md", append = FALSE)

cat("cat_tamanho")

purrr::map2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(2003:2014),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "cat_tamanho",
      ord = "default"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "cat_tamanho") %>%
  knitr::kable()

sink()

# cat_natureza_juridica ----
sink("outputs/rais/comparisons/estab/cat_natureza_juridica.md", append = FALSE)

cat("cat_natureza_juridica")

purrr::map2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(2003:2014),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "cat_natureza_juridica",
      ord = "default"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "cat_natureza_juridica") %>%
  knitr::kable()

sink()

# cat_tipo ----
sink("outputs/rais/comparisons/estab/cat_tipo.md", append = FALSE)

cat("cat_tipo")

purrr::map2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(2003:2014),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "cat_tipo",
      ord = "default"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "cat_tipo") %>%
  knitr::kable()

sink()

# area_planejamento ----
sink("outputs/rais/comparisons/estab/area_planejamento.md", append = FALSE)

cat("area_planejamento")

purrr::map2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(2003:2014),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "area_planejamento",
      ord = "default"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "area_planejamento") %>%
  knitr::kable()

sink()

# regiao_planejamento ----
sink("outputs/rais/comparisons/estab/regiao_planejamento.md", append = FALSE)

cat("regiao_planejamento")

purrr::map2(
  .x = list(
    rais.estab.2003, rais.estab.2004, rais.estab.2005, rais.estab.2006,
    rais.estab.2007, rais.estab.2008, rais.estab.2009, rais.estab.2010,
    rais.estab.2011, rais.estab.2012, rais.estab.2013, rais.estab.2014
  ),
  .y = c(2003:2014),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "regiao_planejamento",
      ord = "default"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "regiao_planejamento") %>%
  knitr::kable()

sink()

### descriptive analysis - rais vinc ----


# rais vinc frequencies ----
gp_vars <- NA

par_vars <- c(
  "indicador_cei_vinculado",
  "mes_admissao",
  "vinculo_ativo_3112",
  "mes_desligamento",
  "cat_faixa_remuneracao_media_sm",
  "cat_faixa_remuneracao_dezembro_sm",
  "cat_tipo_admissao",
  "cat_motivo_desligamento",
  "cat_faixa_tempo_emprego",
  "cat_faixa_horas_contratadas",
  "cat_sexo",
  "cat_nacionalidade",
  "cat_faixa_etaria",
  "cat_tipo_vinculo",
  "cat_tamanho_estabelecimento",
  "cat_tipo_estabelecimento",
  "cat_natureza_juridica",
  "area_planejamento",
  "regiao_planejamento",
  "ano"
)

rais.vinc.arrow %>%
  dplyr::select(all_of(par_vars)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    function(i, j) {
      purrr::walk(
        .x = seq_len(length(gp_vars)),
        function(k) {
          purrr::map(
            .x = seq_len(length(par_vars)),
            function(l) {
              freq.loop.v3(
                df = i,
                db_name = j,
                gp_var = gp_vars[k],
                par = par_vars[l],
                ord = "default"
              )
            }
          )
        }
      )
    }
  )

# cat_raca_cor
par_vars <- c(
  # "indicador_trabalho_parcial", # from 2017
  # "indicador_trabalho_intermitente", # from 2017
  # "cat_tipo_salario", # from 2018
  "cat_raca_cor",
  # "cat_subsetor_ibge", # from 2015
  "ano"
)

rais.vinc.arrow %>%
  dplyr::select(all_of(par_vars)) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    function(i, j) {
      purrr::walk(
        .x = seq_len(length(gp_vars)),
        function(k) {
          purrr::map(
            .x = seq_len(length(par_vars)),
            function(l) {
              freq.loop.v3(
                df = i,
                db_name = j,
                gp_var = gp_vars[k],
                par = par_vars[l],
                ord = "default"
              )
            }
          )
        }
      )
    }
  )

# cat_grau_instrucao_1985_2005
par_vars <- c(
  "cat_grau_instrucao_1985_2005",
  "ano"
)

rais.vinc.arrow %>%
  dplyr::select(all_of(par_vars)) %>%
  dplyr::filter(ano %in% ano.1) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c("RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"),
    function(i, j) {
      purrr::walk(
        .x = seq_len(length(gp_vars)),
        function(k) {
          purrr::map(
            .x = seq_len(length(par_vars)),
            function(l) {
              freq.loop.v3(
                df = i,
                db_name = j,
                gp_var = gp_vars[k],
                par = par_vars[l],
                ord = "default"
              )
            }
          )
        }
      )
    }
  )

# cat_grau_instrucao_apos_2005
par_vars <- c(
  "grau_instrucao_apos_2005",
  "ano"
)

rais.vinc.arrow %>%
  dplyr::select(all_of(par_vars)) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    function(i, j) {
      purrr::walk(
        .x = seq_len(length(gp_vars)),
        function(k) {
          purrr::map(
            .x = seq_len(length(par_vars)),
            function(l) {
              freq.loop.v3(
                df = i,
                db_name = j,
                gp_var = gp_vars[k],
                par = par_vars[l],
                ord = "default"
              )
            }
          )
        }
      )
    }
  )

# by area_planejamento
gp_vars <- "area_planejamento"

par_vars <- c(
  "indicador_cei_vinculado",
  "mes_admissao",
  "vinculo_ativo_3112",
  "mes_desligamento",
  "cat_faixa_remuneracao_media_sm",
  "cat_faixa_remuneracao_dezembro_sm",
  "cat_tipo_admissao",
  "cat_motivo_desligamento",
  "cat_faixa_tempo_emprego",
  "cat_faixa_horas_contratadas",
  "cat_sexo",
  "cat_nacionalidade",
  "cat_faixa_etaria",
  "cat_tipo_vinculo",
  "cat_tamanho_estabelecimento",
  "cat_tipo_estabelecimento",
  "cat_natureza_juridica",
  "area_planejamento",
  "ano"
)

rais.vinc.arrow %>%
  dplyr::select(all_of(par_vars)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    function(i, j) {
      purrr::walk(
        .x = gp_vars,
        function(k) {
          purrr::map(
            par_vars[!(par_vars %in% gp_vars)],
            function(l) {
              freq.loop.v3(
                df = i,
                db_name = j,
                gp_var = k,
                par = l
              )
            }
          )
        }
      )
    }
  )

# cat_raca_cor
par_vars <- c(
  "cat_raca_cor",
  "area_planejamento",
  "ano"
)

rais.vinc.arrow %>%
  dplyr::select(all_of(par_vars)) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    function(i, j) {
      purrr::walk(
        .x = gp_vars,
        function(k) {
          purrr::map(
            par_vars[!(par_vars %in% gp_vars)],
            function(l) {
              freq.loop.v3(
                df = i,
                db_name = j,
                gp_var = k,
                par = l
              )
            }
          )
        }
      )
    }
  )

# cat_grau_instrucao_1985_2005
par_vars <- c(
  "cat_grau_instrucao_1985_2005",
  "area_planejamento",
  "ano"
)

rais.vinc.arrow %>%
  dplyr::select(all_of(par_vars)) %>%
  dplyr::filter(ano %in% ano.1) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c("RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"),
    function(i, j) {
      purrr::walk(
        .x = gp_vars,
        function(k) {
          purrr::map(
            par_vars[!(par_vars %in% gp_vars)],
            function(l) {
              freq.loop.v3(
                df = i,
                db_name = j,
                gp_var = k,
                par = l
              )
            }
          )
        }
      )
    }
  )

# cat_grau_instrucao_apos_2005
par_vars <- c(
  "grau_instrucao_apos_2005",
  "area_planejamento",
  "ano"
)

rais.vinc.arrow %>%
  dplyr::select(all_of(par_vars)) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    function(i, j) {
      purrr::walk(
        .x = gp_vars,
        function(k) {
          purrr::map(
            par_vars[!(par_vars %in% gp_vars)],
            function(l) {
              freq.loop.v3(
                df = i,
                db_name = j,
                gp_var = k,
                par = l
              )
            }
          )
        }
      )
    }
  )


# by regiao_planejamento
gp_vars <- "regiao_planejamento"

par_vars <- c(
  "indicador_cei_vinculado",
  "mes_admissao",
  "vinculo_ativo_3112",
  "mes_desligamento",
  "cat_faixa_remuneracao_media_sm",
  "cat_faixa_remuneracao_dezembro_sm",
  "cat_tipo_admissao",
  "cat_motivo_desligamento",
  "cat_faixa_tempo_emprego",
  "cat_faixa_horas_contratadas",
  "cat_sexo",
  "cat_nacionalidade",
  "cat_faixa_etaria",
  "cat_tipo_vinculo",
  "cat_tamanho_estabelecimento",
  "cat_tipo_estabelecimento",
  "cat_natureza_juridica",
  "regiao_planejamento",
  "ano"
)

rais.vinc.arrow %>%
  dplyr::select(all_of(par_vars)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    function(i, j) {
      purrr::walk(
        .x = gp_vars,
        function(k) {
          purrr::map(
            par_vars[!(par_vars %in% gp_vars)],
            function(l) {
              freq.loop.v3(
                df = i,
                db_name = j,
                gp_var = k,
                par = l
              )
            }
          )
        }
      )
    }
  )

# cat_raca_cor
par_vars <- c(
  "cat_raca_cor",
  "regiao_planejamento",
  "ano"
)

rais.vinc.arrow %>%
  dplyr::select(all_of(par_vars)) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    function(i, j) {
      purrr::walk(
        .x = gp_vars,
        function(k) {
          purrr::map(
            par_vars[!(par_vars %in% gp_vars)],
            function(l) {
              freq.loop.v3(
                df = i,
                db_name = j,
                gp_var = k,
                par = l
              )
            }
          )
        }
      )
    }
  )

# cat_grau_instrucao_1985_2005
par_vars <- c(
  "cat_grau_instrucao_1985_2005",
  "regiao_planejamento",
  "ano"
)

rais.vinc.arrow %>%
  dplyr::select(all_of(par_vars)) %>%
  dplyr::filter(ano %in% ano.1) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c("RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"),
    function(i, j) {
      purrr::walk(
        .x = gp_vars,
        function(k) {
          purrr::map(
            par_vars[!(par_vars %in% gp_vars)],
            function(l) {
              freq.loop.v3(
                df = i,
                db_name = j,
                gp_var = k,
                par = l
              )
            }
          )
        }
      )
    }
  )

# cat_grau_instrucao_apos_2005
par_vars <- c(
  "grau_instrucao_apos_2005",
  "regiao_planejamento",
  "ano"
)

rais.vinc.arrow %>%
  dplyr::select(all_of(par_vars)) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    function(i, j) {
      purrr::walk(
        .x = gp_vars,
        function(k) {
          purrr::map(
            par_vars[!(par_vars %in% gp_vars)],
            function(l) {
              freq.loop.v3(
                df = i,
                db_name = j,
                gp_var = k,
                par = l
              )
            }
          )
        }
      )
    }
  )

# rais vinc cross-tabulations ----

# cat_sexo ----
rais.vinc.arrow %>%
  dplyr::select(cat_sexo, indicador_cei_vinculado, ano) %>%
  dplyr::mutate(indicador_cei_vinculado = as.character(indicador_cei_vinculado)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "indicador_cei_vinculado",
      y_var = "cat_sexo",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_sexo, vinculo_ativo_3112, ano) %>%
  dplyr::mutate(vinculo_ativo_3112 = as.character(vinculo_ativo_3112)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "vinculo_ativo_3112",
      y_var = "cat_sexo",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_sexo, cat_faixa_remuneracao_media_sm, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_sexo",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_sexo, cat_faixa_remuneracao_dezembro_sm, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_sexo",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_sexo, cat_tipo_admissao, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_admissao",
      y_var = "cat_sexo",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_sexo, cat_motivo_desligamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_motivo_desligamento",
      y_var = "cat_sexo",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_sexo, cat_faixa_tempo_emprego, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_tempo_emprego",
      y_var = "cat_sexo",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_sexo, cat_faixa_horas_contratadas, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_horas_contratadas",
      y_var = "cat_sexo",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_sexo, cat_tipo_vinculo, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_sexo",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_sexo, cat_tamanho_estabelecimento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tamanho_estabelecimento",
      y_var = "cat_sexo",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_sexo, cat_tipo_estabelecimento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_estabelecimento",
      y_var = "cat_sexo",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_sexo, cat_natureza_juridica, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_natureza_juridica",
      y_var = "cat_sexo",
      gp_var = NA,
      db_name = .y
    )
  )

# cat_nacionalidade ----
rais.vinc.arrow %>%
  dplyr::select(cat_nacionalidade, indicador_cei_vinculado, ano) %>%
  dplyr::mutate(indicador_cei_vinculado = as.character(indicador_cei_vinculado)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "indicador_cei_vinculado",
      y_var = "cat_nacionalidade",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_nacionalidade, vinculo_ativo_3112, ano) %>%
  dplyr::mutate(vinculo_ativo_3112 = as.character(vinculo_ativo_3112)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "vinculo_ativo_3112",
      y_var = "cat_nacionalidade",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_nacionalidade, cat_faixa_remuneracao_media_sm, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_nacionalidade",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_nacionalidade, cat_faixa_remuneracao_dezembro_sm, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_nacionalidade",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_nacionalidade, cat_tipo_admissao, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_admissao",
      y_var = "cat_nacionalidade",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_nacionalidade, cat_motivo_desligamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_motivo_desligamento",
      y_var = "cat_nacionalidade",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_nacionalidade, cat_faixa_tempo_emprego, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_tempo_emprego",
      y_var = "cat_nacionalidade",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_nacionalidade, cat_faixa_horas_contratadas, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_horas_contratadas",
      y_var = "cat_nacionalidade",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_nacionalidade, cat_tipo_vinculo, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_nacionalidade",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_nacionalidade, cat_tamanho_estabelecimento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tamanho_estabelecimento",
      y_var = "cat_nacionalidade",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_nacionalidade, cat_tipo_estabelecimento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_estabelecimento",
      y_var = "cat_nacionalidade",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_nacionalidade, cat_natureza_juridica, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_natureza_juridica",
      y_var = "cat_nacionalidade",
      gp_var = NA,
      db_name = .y
    )
  )

# cat_faixa_etaria ----
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_etaria, indicador_cei_vinculado, ano) %>%
  dplyr::mutate(indicador_cei_vinculado = as.character(indicador_cei_vinculado)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "indicador_cei_vinculado",
      y_var = "cat_faixa_etaria",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_etaria, vinculo_ativo_3112, ano) %>%
  dplyr::mutate(vinculo_ativo_3112 = as.character(vinculo_ativo_3112)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "vinculo_ativo_3112",
      y_var = "cat_faixa_etaria",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_etaria, cat_faixa_remuneracao_media_sm, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_faixa_etaria",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_etaria, cat_faixa_remuneracao_dezembro_sm, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_faixa_etaria",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_etaria, cat_tipo_admissao, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_admissao",
      y_var = "cat_faixa_etaria",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_etaria, cat_motivo_desligamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_motivo_desligamento",
      y_var = "cat_faixa_etaria",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_etaria, cat_faixa_tempo_emprego, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_tempo_emprego",
      y_var = "cat_faixa_etaria",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_etaria, cat_faixa_horas_contratadas, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_horas_contratadas",
      y_var = "cat_faixa_etaria",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_etaria, cat_tipo_vinculo, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_faixa_etaria",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_etaria, cat_tamanho_estabelecimento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tamanho_estabelecimento",
      y_var = "cat_faixa_etaria",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_etaria, cat_tipo_estabelecimento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_estabelecimento",
      y_var = "cat_faixa_etaria",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_etaria, cat_natureza_juridica, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_natureza_juridica",
      y_var = "cat_faixa_etaria",
      gp_var = NA,
      db_name = .y
    )
  )

# area_planejamento ----
rais.vinc.arrow %>%
  dplyr::select(area_planejamento, indicador_cei_vinculado, ano) %>%
  dplyr::mutate(indicador_cei_vinculado = as.character(indicador_cei_vinculado)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "indicador_cei_vinculado",
      y_var = "area_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(area_planejamento, vinculo_ativo_3112, ano) %>%
  dplyr::mutate(vinculo_ativo_3112 = as.character(vinculo_ativo_3112)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "vinculo_ativo_3112",
      y_var = "area_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(area_planejamento, cat_faixa_remuneracao_media_sm, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "area_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(area_planejamento, cat_faixa_remuneracao_dezembro_sm, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "area_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(area_planejamento, cat_tipo_admissao, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_admissao",
      y_var = "area_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(area_planejamento, cat_motivo_desligamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_motivo_desligamento",
      y_var = "area_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(area_planejamento, cat_faixa_tempo_emprego, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_tempo_emprego",
      y_var = "area_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(area_planejamento, cat_faixa_horas_contratadas, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_horas_contratadas",
      y_var = "area_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(area_planejamento, cat_tipo_vinculo, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "area_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(area_planejamento, cat_tamanho_estabelecimento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tamanho_estabelecimento",
      y_var = "area_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(area_planejamento, cat_tipo_estabelecimento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_estabelecimento",
      y_var = "area_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(area_planejamento, cat_natureza_juridica, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_natureza_juridica",
      y_var = "area_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

# regiao_planejamento ----
rais.vinc.arrow %>%
  dplyr::select(regiao_planejamento, indicador_cei_vinculado, ano) %>%
  dplyr::mutate(indicador_cei_vinculado = as.character(indicador_cei_vinculado)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "indicador_cei_vinculado",
      y_var = "regiao_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(regiao_planejamento, vinculo_ativo_3112, ano) %>%
  dplyr::mutate(vinculo_ativo_3112 = as.character(vinculo_ativo_3112)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "vinculo_ativo_3112",
      y_var = "regiao_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(regiao_planejamento, cat_faixa_remuneracao_media_sm, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "regiao_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(regiao_planejamento, cat_faixa_remuneracao_dezembro_sm, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "regiao_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(regiao_planejamento, cat_tipo_admissao, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_admissao",
      y_var = "regiao_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(regiao_planejamento, cat_motivo_desligamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_motivo_desligamento",
      y_var = "regiao_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(regiao_planejamento, cat_faixa_tempo_emprego, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_tempo_emprego",
      y_var = "regiao_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(regiao_planejamento, cat_faixa_horas_contratadas, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_horas_contratadas",
      y_var = "regiao_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(regiao_planejamento, cat_tipo_vinculo, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "regiao_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(regiao_planejamento, cat_tamanho_estabelecimento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tamanho_estabelecimento",
      y_var = "regiao_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(regiao_planejamento, cat_tipo_estabelecimento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_estabelecimento",
      y_var = "regiao_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(regiao_planejamento, cat_natureza_juridica, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_natureza_juridica",
      y_var = "regiao_planejamento",
      gp_var = NA,
      db_name = .y
    )
  )

# cat_grau_instrucao_1985_2005 (2003 ~ 2005) ----
rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, indicador_cei_vinculado, ano) %>%
  dplyr::mutate(indicador_cei_vinculado = as.character(indicador_cei_vinculado)) %>%
  dplyr::filter(ano %in% ano.grau_inst_ate_2005) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "indicador_cei_vinculado",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, vinculo_ativo_3112, ano) %>%
  dplyr::mutate(vinculo_ativo_3112 = as.character(vinculo_ativo_3112)) %>%
  dplyr::filter(ano %in% ano.grau_inst_ate_2005) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "vinculo_ativo_3112",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, cat_faixa_remuneracao_media_sm, ano) %>%
  dplyr::filter(ano %in% ano.grau_inst_ate_2005) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, cat_faixa_remuneracao_dezembro_sm, ano) %>%
  dplyr::filter(ano %in% ano.grau_inst_ate_2005) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, cat_tipo_admissao, ano) %>%
  dplyr::filter(ano %in% ano.grau_inst_ate_2005) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_admissao",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, cat_motivo_desligamento, ano) %>%
  dplyr::filter(ano %in% ano.grau_inst_ate_2005) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_motivo_desligamento",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, cat_faixa_tempo_emprego, ano) %>%
  dplyr::filter(ano %in% ano.grau_inst_ate_2005) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_tempo_emprego",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, cat_faixa_horas_contratadas, ano) %>%
  dplyr::filter(ano %in% ano.grau_inst_ate_2005) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_horas_contratadas",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, cat_tipo_vinculo, ano) %>%
  dplyr::filter(ano %in% ano.grau_inst_ate_2005) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, cat_tamanho_estabelecimento, ano) %>%
  dplyr::filter(ano %in% ano.grau_inst_ate_2005) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tamanho_estabelecimento",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, cat_tipo_estabelecimento, ano) %>%
  dplyr::filter(ano %in% ano.grau_inst_ate_2005) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_estabelecimento",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, cat_natureza_juridica, ano) %>%
  dplyr::filter(ano %in% ano.grau_inst_ate_2005) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_natureza_juridica",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = NA,
      db_name = .y
    )
  )

# personal characteristics
rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, cat_sexo, ano) %>%
  dplyr::filter(ano %in% ano.grau_inst_ate_2005) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_sexo",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, cat_nacionalidade, ano) %>%
  dplyr::filter(ano %in% ano.grau_inst_ate_2005) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_nacionalidade",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, cat_faixa_etaria, ano) %>%
  dplyr::filter(ano %in% ano.grau_inst_ate_2005) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_etaria",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.grau_inst_ate_2005) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "area_planejamento",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.grau_inst_ate_2005) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "regiao_planejamento",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = NA,
      db_name = .y
    )
  )

# cat_grau_instrucao_apos_2005 (2006 ~ 2014) ----
rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, indicador_cei_vinculado, ano) %>%
  dplyr::mutate(indicador_cei_vinculado = as.character(indicador_cei_vinculado)) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "indicador_cei_vinculado",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, vinculo_ativo_3112, ano) %>%
  dplyr::mutate(vinculo_ativo_3112 = as.character(vinculo_ativo_3112)) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "vinculo_ativo_3112",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, cat_faixa_remuneracao_media_sm, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, cat_faixa_remuneracao_dezembro_sm, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, cat_tipo_admissao, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_admissao",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, cat_motivo_desligamento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_motivo_desligamento",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, cat_faixa_tempo_emprego, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_tempo_emprego",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, cat_faixa_horas_contratadas, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_horas_contratadas",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, cat_tipo_vinculo, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, cat_tamanho_estabelecimento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tamanho_estabelecimento",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, cat_tipo_estabelecimento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_estabelecimento",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, cat_natureza_juridica, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_natureza_juridica",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = NA,
      db_name = .y
    )
  )

# personal characteristics
rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, cat_sexo, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_sexo",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, cat_nacionalidade, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_nacionalidade",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, cat_faixa_etaria, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_etaria",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "area_planejamento",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "regiao_planejamento",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = NA,
      db_name = .y
    )
  )

# cat_raca_cor (2006 ~ 2014) ----
rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, indicador_cei_vinculado, ano) %>%
  dplyr::mutate(indicador_cei_vinculado = as.character(indicador_cei_vinculado)) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "indicador_cei_vinculado",
      y_var = "cat_raca_cor",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, vinculo_ativo_3112, ano) %>%
  dplyr::mutate(vinculo_ativo_3112 = as.character(vinculo_ativo_3112)) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "vinculo_ativo_3112",
      y_var = "cat_raca_cor",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, cat_faixa_remuneracao_media_sm, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_raca_cor",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, cat_faixa_remuneracao_dezembro_sm, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_raca_cor",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, cat_tipo_admissao, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_admissao",
      y_var = "cat_raca_cor",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, cat_motivo_desligamento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_motivo_desligamento",
      y_var = "cat_raca_cor",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, cat_faixa_tempo_emprego, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_tempo_emprego",
      y_var = "cat_raca_cor",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, cat_faixa_horas_contratadas, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_horas_contratadas",
      y_var = "cat_raca_cor",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, cat_tipo_vinculo, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_raca_cor",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, cat_tamanho_estabelecimento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tamanho_estabelecimento",
      y_var = "cat_raca_cor",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, cat_tipo_estabelecimento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_estabelecimento",
      y_var = "cat_raca_cor",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, cat_natureza_juridica, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_natureza_juridica",
      y_var = "cat_raca_cor",
      gp_var = NA,
      db_name = .y
    )
  )

# personal characteristics
rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, cat_sexo, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_sexo",
      y_var = "cat_raca_cor",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, cat_nacionalidade, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_nacionalidade",
      y_var = "cat_raca_cor",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, cat_faixa_etaria, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_etaria",
      y_var = "cat_raca_cor",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, cat_grau_instrucao_apos_2005, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_grau_instrucao_apos_2005",
      y_var = "cat_raca_cor",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "area_planejamento",
      y_var = "cat_raca_cor",
      gp_var = NA,
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "regiao_planejamento",
      y_var = "cat_raca_cor",
      gp_var = NA,
      db_name = .y
    )
  )

# cat_tipo_vinculo (AP/RP) ----
# mes_admissao (as.character)
rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, mes_admissao, area_planejamento, ano) %>%
  dplyr::mutate(mes_admissao = as.character(mes_admissao)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "mes_admissao",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, mes_admissao, regiao_planejamento, ano) %>%
  dplyr::mutate(mes_admissao = as.character(mes_admissao)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "mes_admissao",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# vinculo_ativo_3112 (as.character)
rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, vinculo_ativo_3112, area_planejamento, ano) %>%
  dplyr::mutate(vinculo_ativo_3112 = as.character(vinculo_ativo_3112)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "vinculo_ativo_3112",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, vinculo_ativo_3112, regiao_planejamento, ano) %>%
  dplyr::mutate(vinculo_ativo_3112 = as.character(vinculo_ativo_3112)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "vinculo_ativo_3112",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# mes_desligamento (as.character)
rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, mes_desligamento, area_planejamento, ano) %>%
  dplyr::mutate(mes_desligamento = as.character(mes_desligamento)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "mes_desligamento",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, mes_desligamento, regiao_planejamento, ano) %>%
  dplyr::mutate(mes_desligamento = as.character(mes_desligamento)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "mes_desligamento",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# indicador_portador_deficiencia (as.character)
rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, indicador_portador_deficiencia, area_planejamento, ano) %>%
  dplyr::mutate(indicador_portador_deficiencia = as.character(indicador_portador_deficiencia)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "indicador_portador_deficiencia",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, indicador_portador_deficiencia, regiao_planejamento, ano) %>%
  dplyr::mutate(indicador_portador_deficiencia = as.character(indicador_portador_deficiencia)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "indicador_portador_deficiencia",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_tipo_admissao
rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_tipo_admissao, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_tipo_admissao",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_tipo_admissao, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_tipo_admissao",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_motivo_desligamento
rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_motivo_desligamento, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_motivo_desligamento",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_motivo_desligamento, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_motivo_desligamento",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_faixa_tempo_emprego
rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_faixa_tempo_emprego, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_faixa_tempo_emprego",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_faixa_tempo_emprego, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_faixa_tempo_emprego",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_faixa_horas_contratadas
rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_faixa_horas_contratadas, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_faixa_horas_contratadas",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_faixa_horas_contratadas, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_faixa_horas_contratadas",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_sexo
rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_sexo, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_sexo",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_sexo, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_sexo",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_nacionalidade
rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_nacionalidade, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_nacionalidade",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_nacionalidade, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_nacionalidade",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_faixa_etaria
rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_faixa_etaria, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_faixa_etaria",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_faixa_etaria, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_faixa_etaria",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_tamanho_estabelecimento
rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_tamanho_estabelecimento, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_tamanho_estabelecimento",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_tamanho_estabelecimento, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_tamanho_estabelecimento",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_tipo_estabelecimento
rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_tipo_estabelecimento, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_tipo_estabelecimento",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_tipo_estabelecimento, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_tipo_estabelecimento",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_natureza_juridica
rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_natureza_juridica, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_natureza_juridica",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_natureza_juridica, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_natureza_juridica",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_grau_instrucao_1985_2005 (2003-2005)
rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_grau_instrucao_1985_2005, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.1) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_grau_instrucao_1985_2005, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.1) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_raca_cor (from 2006)
rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_raca_cor, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_raca_cor",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_raca_cor, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_raca_cor",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_grau_instrucao_apos_2005 (from 2006)
rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_grau_instrucao_apos_2005, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, cat_grau_instrucao_apos_2005, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_tipo_vinculo",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_faixa_remuneracao_media_sm (AP/RP) ----
# mes_admissao (as.character)
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, mes_admissao, area_planejamento, ano) %>%
  dplyr::mutate(mes_admissao = as.character(mes_admissao)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "mes_admissao",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, mes_admissao, regiao_planejamento, ano) %>%
  dplyr::mutate(mes_admissao = as.character(mes_admissao)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "mes_admissao",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# vinculo_ativo_3112 (as.character)
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, vinculo_ativo_3112, area_planejamento, ano) %>%
  dplyr::mutate(vinculo_ativo_3112 = as.character(vinculo_ativo_3112)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "vinculo_ativo_3112",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, vinculo_ativo_3112, regiao_planejamento, ano) %>%
  dplyr::mutate(vinculo_ativo_3112 = as.character(vinculo_ativo_3112)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "vinculo_ativo_3112",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# mes_desligamento (as.character)
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, mes_desligamento, area_planejamento, ano) %>%
  dplyr::mutate(mes_desligamento = as.character(mes_desligamento)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "mes_desligamento",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, mes_desligamento, regiao_planejamento, ano) %>%
  dplyr::mutate(mes_desligamento = as.character(mes_desligamento)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "mes_desligamento",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# indicador_portador_deficiencia (as.character)
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, indicador_portador_deficiencia, area_planejamento, ano) %>%
  dplyr::mutate(indicador_portador_deficiencia = as.character(indicador_portador_deficiencia)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "indicador_portador_deficiencia",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, indicador_portador_deficiencia, regiao_planejamento, ano) %>%
  dplyr::mutate(indicador_portador_deficiencia = as.character(indicador_portador_deficiencia)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "indicador_portador_deficiencia",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_tipo_admissao
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_tipo_admissao, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_tipo_admissao",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_tipo_admissao, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_tipo_admissao",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_motivo_desligamento
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_motivo_desligamento, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_motivo_desligamento",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_motivo_desligamento, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_motivo_desligamento",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_faixa_tempo_emprego
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_faixa_tempo_emprego, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_faixa_tempo_emprego",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_faixa_tempo_emprego, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_faixa_tempo_emprego",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_faixa_horas_contratadas
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_faixa_horas_contratadas, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_faixa_horas_contratadas",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_faixa_horas_contratadas, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_faixa_horas_contratadas",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_sexo
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_sexo, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_sexo",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_sexo, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_sexo",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_nacionalidade
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_nacionalidade, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_nacionalidade",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_nacionalidade, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_nacionalidade",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_faixa_etaria
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_faixa_etaria, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_faixa_etaria",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_faixa_etaria, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_faixa_etaria",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_tamanho_estabelecimento
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_tamanho_estabelecimento, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_tamanho_estabelecimento",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_tamanho_estabelecimento, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_tamanho_estabelecimento",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_tipo_estabelecimento
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_tipo_estabelecimento, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_tipo_estabelecimento",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_tipo_estabelecimento, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_tipo_estabelecimento",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_natureza_juridica
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_natureza_juridica, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_natureza_juridica",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_natureza_juridica, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_natureza_juridica",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_grau_instrucao_1985_2005 (2003-2005)
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_grau_instrucao_1985_2005, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.1) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_grau_instrucao_1985_2005, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.1) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_raca_cor (from 2006)
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_raca_cor, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_raca_cor",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_raca_cor, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_raca_cor",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_grau_instrucao_apos_2005 (from 2006)
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_grau_instrucao_apos_2005, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, cat_grau_instrucao_apos_2005, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_media_sm",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_faixa_remuneracao_dezembro_sm (AP/RP) ----
# mes_admissao (as.character)
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, mes_admissao, area_planejamento, ano) %>%
  dplyr::mutate(mes_admissao = as.character(mes_admissao)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "mes_admissao",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, mes_admissao, regiao_planejamento, ano) %>%
  dplyr::mutate(mes_admissao = as.character(mes_admissao)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "mes_admissao",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# vinculo_ativo_3112 (as.character)
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, vinculo_ativo_3112, area_planejamento, ano) %>%
  dplyr::mutate(vinculo_ativo_3112 = as.character(vinculo_ativo_3112)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "vinculo_ativo_3112",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, vinculo_ativo_3112, regiao_planejamento, ano) %>%
  dplyr::mutate(vinculo_ativo_3112 = as.character(vinculo_ativo_3112)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "vinculo_ativo_3112",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# mes_desligamento (as.character)
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, mes_desligamento, area_planejamento, ano) %>%
  dplyr::mutate(mes_desligamento = as.character(mes_desligamento)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "mes_desligamento",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, mes_desligamento, regiao_planejamento, ano) %>%
  dplyr::mutate(mes_desligamento = as.character(mes_desligamento)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "mes_desligamento",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# indicador_portador_deficiencia (as.character)
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, indicador_portador_deficiencia, area_planejamento, ano) %>%
  dplyr::mutate(indicador_portador_deficiencia = as.character(indicador_portador_deficiencia)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "indicador_portador_deficiencia",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, indicador_portador_deficiencia, regiao_planejamento, ano) %>%
  dplyr::mutate(indicador_portador_deficiencia = as.character(indicador_portador_deficiencia)) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "indicador_portador_deficiencia",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_tipo_admissao
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_tipo_admissao, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_tipo_admissao",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_tipo_admissao, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_tipo_admissao",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_motivo_desligamento
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_motivo_desligamento, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_motivo_desligamento",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_motivo_desligamento, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_motivo_desligamento",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_faixa_tempo_emprego
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_faixa_tempo_emprego, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_faixa_tempo_emprego",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_faixa_tempo_emprego, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_faixa_tempo_emprego",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_faixa_horas_contratadas
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_faixa_horas_contratadas, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_faixa_horas_contratadas",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_faixa_horas_contratadas, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_faixa_horas_contratadas",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_sexo
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_sexo, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_sexo",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_sexo, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_sexo",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_nacionalidade
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_nacionalidade, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_nacionalidade",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_nacionalidade, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_nacionalidade",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_faixa_etaria
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_faixa_etaria, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_faixa_etaria",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_faixa_etaria, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_faixa_etaria",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_tamanho_estabelecimento
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_tamanho_estabelecimento, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_tamanho_estabelecimento",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_tamanho_estabelecimento, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_tamanho_estabelecimento",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_tipo_estabelecimento
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_tipo_estabelecimento, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_tipo_estabelecimento",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_tipo_estabelecimento, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_tipo_estabelecimento",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_natureza_juridica
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_natureza_juridica, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_natureza_juridica",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_natureza_juridica, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005", "RAIS vinc 2006",
      "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009", "RAIS vinc 2010",
      "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013", "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_natureza_juridica",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_grau_instrucao_1985_2005 (2003-2005)
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_grau_instrucao_1985_2005, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.1) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_grau_instrucao_1985_2005, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.1) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2003", "RAIS vinc 2004", "RAIS vinc 2005"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_grau_instrucao_1985_2005",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_raca_cor (from 2006)
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_raca_cor, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_raca_cor",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_raca_cor, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_raca_cor",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# cat_grau_instrucao_apos_2005 (from 2006)
rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_grau_instrucao_apos_2005, area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = "area_planejamento",
      db_name = .y
    )
  )

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, cat_grau_instrucao_apos_2005, regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = c(
      "RAIS vinc 2006", "RAIS vinc 2007", "RAIS vinc 2008", "RAIS vinc 2009",
      "RAIS vinc 2010", "RAIS vinc 2011", "RAIS vinc 2012", "RAIS vinc 2013",
      "RAIS vinc 2014"
    ),
    ~ ctable.test.vinc(
      df = .x,
      x_var = "cat_faixa_remuneracao_dezembro_sm",
      y_var = "cat_grau_instrucao_apos_2005",
      gp_var = "regiao_planejamento",
      db_name = .y
    )
  )

# rais vinc descriptive statistics ----
sink("outputs/rais/desc-stats/vinc/desc_stats.md", append = FALSE)

cat("## RAIS vinc 2003-2005")

rais.vinc.arrow %>%
  dplyr::select(
    idade,
    mes_admissao,
    mes_desligamento,
    quantidade_horas_contratadas,
    tempo_emprego,
    valor_remuneracao_dezembro,
    valor_remuneracao_dezembro_sm,
    valor_remuneracao_media,
    valor_remuneracao_media_sm,
    ano
  ) %>%
  dplyr::filter(ano %in% ano.1) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      # group_by(area_planejamento) %>%
      descr(
        stats = desc.base,
        style = "rmarkdown"
      )
  )

cat("## RAIS vinc 2006-2008")

rais.vinc.arrow %>%
  dplyr::select(
    idade,
    mes_admissao,
    mes_desligamento,
    quantidade_horas_contratadas,
    tempo_emprego,
    valor_remuneracao_dezembro,
    valor_remuneracao_dezembro_sm,
    valor_remuneracao_media,
    valor_remuneracao_media_sm,
    ano
  ) %>%
  dplyr::filter(ano %in% ano.2) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      # group_by(area_planejamento) %>%
      descr(
        stats = desc.base,
        style = "rmarkdown"
      )
  )

cat("## RAIS vinc 2009-2011")

rais.vinc.arrow %>%
  dplyr::select(
    idade,
    mes_admissao,
    mes_desligamento,
    quantidade_horas_contratadas,
    tempo_emprego,
    valor_remuneracao_dezembro,
    valor_remuneracao_dezembro_sm,
    valor_remuneracao_media,
    valor_remuneracao_media_sm,
    ano
  ) %>%
  dplyr::filter(ano %in% ano.3) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      # group_by(area_planejamento) %>%
      descr(
        stats = desc.base,
        style = "rmarkdown"
      )
  )

cat("## RAIS vinc 2012-2014")

rais.vinc.arrow %>%
  dplyr::select(
    idade,
    mes_admissao,
    mes_desligamento,
    quantidade_horas_contratadas,
    tempo_emprego,
    valor_remuneracao_dezembro,
    valor_remuneracao_dezembro_sm,
    valor_remuneracao_media,
    valor_remuneracao_media_sm,
    ano
  ) %>%
  dplyr::filter(ano %in% ano.4) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      # group_by(area_planejamento) %>%
      descr(
        stats = desc.base,
        style = "rmarkdown"
      )
  )

sink()

# by area_planejamento
sink("outputs/rais/desc-stats/vinc/desc_stats-AP.md", append = FALSE)

cat("## RAIS vinc by AP 2003-2005")

rais.vinc.arrow %>%
  dplyr::select(
    idade,
    mes_admissao,
    mes_desligamento,
    quantidade_horas_contratadas,
    tempo_emprego,
    valor_remuneracao_dezembro,
    valor_remuneracao_dezembro_sm,
    valor_remuneracao_media,
    valor_remuneracao_media_sm,
    ano,
    area_planejamento
  ) %>%
  dplyr::filter(ano %in% ano.1) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      group_by(area_planejamento) %>%
      descr(
        stats = desc.base,
        style = "rmarkdown"
      )
  )

cat("## RAIS vinc by AP 2006-2008")

rais.vinc.arrow %>%
  dplyr::select(
    idade,
    mes_admissao,
    mes_desligamento,
    quantidade_horas_contratadas,
    tempo_emprego,
    valor_remuneracao_dezembro,
    valor_remuneracao_dezembro_sm,
    valor_remuneracao_media,
    valor_remuneracao_media_sm,
    ano,
    area_planejamento
  ) %>%
  dplyr::filter(ano %in% ano.2) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      group_by(area_planejamento) %>%
      descr(
        stats = desc.base,
        style = "rmarkdown"
      )
  )

cat("## RAIS vinc by AP 2009-2011")

rais.vinc.arrow %>%
  dplyr::select(
    idade,
    mes_admissao,
    mes_desligamento,
    quantidade_horas_contratadas,
    tempo_emprego,
    valor_remuneracao_dezembro,
    valor_remuneracao_dezembro_sm,
    valor_remuneracao_media,
    valor_remuneracao_media_sm,
    ano,
    area_planejamento
  ) %>%
  dplyr::filter(ano %in% ano.3) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      group_by(area_planejamento) %>%
      descr(
        stats = desc.base,
        style = "rmarkdown"
      )
  )

cat("## RAIS vinc by AP 2012-2014")

rais.vinc.arrow %>%
  dplyr::select(
    idade,
    mes_admissao,
    mes_desligamento,
    quantidade_horas_contratadas,
    tempo_emprego,
    valor_remuneracao_dezembro,
    valor_remuneracao_dezembro_sm,
    valor_remuneracao_media,
    valor_remuneracao_media_sm,
    ano,
    area_planejamento
  ) %>%
  dplyr::filter(ano %in% ano.4) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      group_by(area_planejamento) %>%
      descr(
        stats = desc.base,
        style = "rmarkdown"
      )
  )

sink()

# by regiao_planejamento
sink("outputs/rais/desc-stats/vinc/desc_stats-RP.md", append = FALSE)

cat("## RAIS vinc by RP 2003-2005")

rais.vinc.arrow %>%
  dplyr::select(
    idade,
    mes_admissao,
    mes_desligamento,
    quantidade_horas_contratadas,
    tempo_emprego,
    valor_remuneracao_dezembro,
    valor_remuneracao_dezembro_sm,
    valor_remuneracao_media,
    valor_remuneracao_media_sm,
    ano,
    regiao_planejamento
  ) %>%
  dplyr::filter(ano %in% ano.1) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      group_by(regiao_planejamento) %>%
      descr(
        stats = desc.base,
        style = "rmarkdown"
      )
  )

cat("## RAIS vinc by RP 2006-2008")

rais.vinc.arrow %>%
  dplyr::select(
    idade,
    mes_admissao,
    mes_desligamento,
    quantidade_horas_contratadas,
    tempo_emprego,
    valor_remuneracao_dezembro,
    valor_remuneracao_dezembro_sm,
    valor_remuneracao_media,
    valor_remuneracao_media_sm,
    ano,
    regiao_planejamento
  ) %>%
  dplyr::filter(ano %in% ano.2) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      group_by(regiao_planejamento) %>%
      descr(
        stats = desc.base,
        style = "rmarkdown"
      )
  )

cat("## RAIS vinc by RP 2009-2011")

rais.vinc.arrow %>%
  dplyr::select(
    idade,
    mes_admissao,
    mes_desligamento,
    quantidade_horas_contratadas,
    tempo_emprego,
    valor_remuneracao_dezembro,
    valor_remuneracao_dezembro_sm,
    valor_remuneracao_media,
    valor_remuneracao_media_sm,
    ano,
    regiao_planejamento
  ) %>%
  dplyr::filter(ano %in% ano.3) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      group_by(regiao_planejamento) %>%
      descr(
        stats = desc.base,
        style = "rmarkdown"
      )
  )

cat("## RAIS vinc by RP 2012-2014")

rais.vinc.arrow %>%
  dplyr::select(
    idade,
    mes_admissao,
    mes_desligamento,
    quantidade_horas_contratadas,
    tempo_emprego,
    valor_remuneracao_dezembro,
    valor_remuneracao_dezembro_sm,
    valor_remuneracao_media,
    valor_remuneracao_media_sm,
    ano,
    regiao_planejamento
  ) %>%
  dplyr::filter(ano %in% ano.4) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      group_by(regiao_planejamento) %>%
      descr(
        stats = desc.base,
        style = "rmarkdown"
      )
  )

sink()

# data frame summary
sink("outputs/rais/desc-stats/vinc/df_summary.md", append = FALSE)

cat("## RAIS vinc 2003-2005")

rais.vinc.arrow %>%
  dplyr::filter(ano %in% ano.1) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      # group_by(area_planejamento) %>%
      dfSummary()
  )

cat("## RAIS vinc 2006-2008")

rais.vinc.arrow %>%
  dplyr::filter(ano %in% ano.2) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      # group_by(area_planejamento) %>%
      dfSummary()
  )

cat("## RAIS vinc 2009-2011")

rais.vinc.arrow %>%
  dplyr::filter(ano %in% ano.3) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      # group_by(area_planejamento) %>%
      dfSummary()
  )

cat("## RAIS vinc 2012-2014")

rais.vinc.arrow %>%
  dplyr::filter(ano %in% ano.4) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      # group_by(area_planejamento) %>%
      dfSummary()
  )

sink()

# by area_planejamento
sink("outputs/rais/desc-stats/vinc/df_summary-AP.md", append = FALSE)

cat("## RAIS vinc by AP 2003-2005")

rais.vinc.arrow %>%
  dplyr::filter(ano %in% ano.1) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      group_by(area_planejamento) %>%
      dfSummary()
  )

cat("## RAIS vinc by AP 2006-2008")

rais.vinc.arrow %>%
  dplyr::filter(ano %in% ano.2) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      group_by(area_planejamento) %>%
      dfSummary()
  )

cat("## RAIS vinc by AP 2009-2011")

rais.vinc.arrow %>%
  dplyr::filter(ano %in% ano.3) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      group_by(area_planejamento) %>%
      dfSummary()
  )

cat("## RAIS vinc by AP 2012-2014")

rais.vinc.arrow %>%
  dplyr::filter(ano %in% ano.4) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      group_by(area_planejamento) %>%
      dfSummary()
  )

sink()

# by regiao_planejamento
sink("outputs/rais/desc-stats/vinc/df_summary-RP.md", append = FALSE)

cat("## RAIS vinc by RP 2003-2005")

rais.vinc.arrow %>%
  dplyr::filter(ano %in% ano.1) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      group_by(regiao_planejamento) %>%
      dfSummary()
  )

cat("## RAIS vinc by RP 2006-2008")

rais.vinc.arrow %>%
  dplyr::filter(ano %in% ano.2) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      group_by(regiao_planejamento) %>%
      dfSummary()
  )

cat("## RAIS vinc by RP 2009-2011")

rais.vinc.arrow %>%
  dplyr::filter(ano %in% ano.3) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      group_by(regiao_planejamento) %>%
      dfSummary()
  )

cat("## RAIS vinc by RP 2012-2014")

rais.vinc.arrow %>%
  dplyr::filter(ano %in% ano.4) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map(
    ~ .x %>%
      group_by(regiao_planejamento) %>%
      dfSummary()
  )

sink()

# rais vinc plots ----

# bar plots (for categorical variables)
rais.vinc.arrow %>%
  dplyr::filter(ano %in% ano.1) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = glue::glue("{2003:2005}_rais_vinc"),
    function(i, j) {
      purrr::walk(
        .x = c(
          "indicador_cei_vinculado",
          "mes_admissao",
          "vinculo_ativo_3112",
          "mes_desligamento",
          "cat_faixa_remuneracao_media_sm",
          "cat_faixa_remuneracao_dezembro_sm",
          "cat_tipo_admissao",
          "cat_motivo_desligamento",
          "cat_faixa_tempo_emprego",
          "cat_faixa_horas_contratadas",
          "cat_sexo",
          "cat_nacionalidade",
          "cat_faixa_etaria",
          "cat_grau_instrucao_1985_2005",
          "cat_tipo_vinculo",
          "cat_tamanho_estabelecimento",
          "cat_tipo_estabelecimento",
          "cat_natureza_juridica",
          "area_planejamento",
          "regiao_planejamento"
        ),
        ~ geom.bar.test.vinc(
          df = i,
          var = .x,
          name_plot = j
          # gp_var = "sexo"
        )
      )
    }
  )

rais.vinc.arrow %>%
  dplyr::filter(ano %in% ano.2) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = glue::glue("{2006:2008}_rais_vinc"),
    function(i, j) {
      purrr::walk(
        .x = c(
          "cat_raca_cor",
          "cat_grau_instrucao_apos_2005",
          "indicador_cei_vinculado",
          "mes_admissao",
          "vinculo_ativo_3112",
          "mes_desligamento",
          "cat_faixa_remuneracao_media_sm",
          "cat_faixa_remuneracao_dezembro_sm",
          "cat_tipo_admissao",
          "cat_motivo_desligamento",
          "cat_faixa_tempo_emprego",
          "cat_faixa_horas_contratadas",
          "cat_sexo",
          "cat_nacionalidade",
          "cat_faixa_etaria",
          "cat_tipo_vinculo",
          "cat_tamanho_estabelecimento",
          "cat_tipo_estabelecimento",
          "cat_natureza_juridica",
          "area_planejamento",
          "regiao_planejamento"
        ),
        ~ geom.bar.test.vinc(
          df = i,
          var = .x,
          name_plot = j
          # gp_var = "sexo"
        )
      )
    }
  )

rais.vinc.arrow %>%
  dplyr::filter(ano %in% ano.3) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = glue::glue("{2009:2011}_rais_vinc"),
    function(i, j) {
      purrr::walk(
        .x = c(
          "cat_raca_cor",
          "cat_grau_instrucao_apos_2005",
          "indicador_cei_vinculado",
          "mes_admissao",
          "vinculo_ativo_3112",
          "mes_desligamento",
          "cat_faixa_remuneracao_media_sm",
          "cat_faixa_remuneracao_dezembro_sm",
          "cat_tipo_admissao",
          "cat_motivo_desligamento",
          "cat_faixa_tempo_emprego",
          "cat_faixa_horas_contratadas",
          "cat_sexo",
          "cat_nacionalidade",
          "cat_faixa_etaria",
          "cat_tipo_vinculo",
          "cat_tamanho_estabelecimento",
          "cat_tipo_estabelecimento",
          "cat_natureza_juridica",
          "area_planejamento",
          "regiao_planejamento"
        ),
        ~ geom.bar.test.vinc(
          df = i,
          var = .x,
          name_plot = j
          # gp_var = "sexo"
        )
      )
    }
  )

rais.vinc.arrow %>%
  dplyr::filter(ano %in% ano.4) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::walk2(
    .y = glue::glue("{2012:2014}_rais_vinc"),
    function(i, j) {
      purrr::walk(
        .x = c(
          "cat_raca_cor",
          "cat_grau_instrucao_apos_2005",
          "indicador_cei_vinculado",
          "mes_admissao",
          "vinculo_ativo_3112",
          "mes_desligamento",
          "cat_faixa_remuneracao_media_sm",
          "cat_faixa_remuneracao_dezembro_sm",
          "cat_tipo_admissao",
          "cat_motivo_desligamento",
          "cat_faixa_tempo_emprego",
          "cat_faixa_horas_contratadas",
          "cat_sexo",
          "cat_nacionalidade",
          "cat_faixa_etaria",
          "cat_tipo_vinculo",
          "cat_tamanho_estabelecimento",
          "cat_tipo_estabelecimento",
          "cat_natureza_juridica",
          "area_planejamento",
          "regiao_planejamento"
        ),
        ~ geom.bar.test.vinc(
          df = i,
          var = .x,
          name_plot = j
          # gp_var = "sexo"
        )
      )
    }
  )

# histogram (for quantitative/continuous variables)


# boxplot (for quantitative/continuous variables)


# scatterplot (for two quantitative variables (if applicable))


# scatterplot by factor


### descriptive analysis - rais vinc by year ----

# indicador_cei_vinculado ----
sink("outputs/rais/comparisons/vinc/indicador_cei_vinculado.md", append = FALSE)

cat("indicador_cei_vinculado")

rais.vinc.arrow %>%
  dplyr::select(indicador_cei_vinculado, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "indicador_cei_vinculado",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "indicador_cei_vinculado") %>%
  knitr::kable()

sink()

# mes_admissao ----
sink("outputs/rais/comparisons/vinc/mes_admissao.md", append = FALSE)

cat("mes_admissao")

rais.vinc.arrow %>%
  dplyr::select(mes_admissao, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "mes_admissao",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "mes_admissao") %>%
  knitr::kable()

sink()

# vinculo_ativo_3112 ----
sink("outputs/rais/comparisons/vinc/vinculo_ativo_3112.md", append = FALSE)

cat("vinculo_ativo_3112")

rais.vinc.arrow %>%
  dplyr::select(vinculo_ativo_3112, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "vinculo_ativo_3112",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "vinculo_ativo_3112") %>%
  knitr::kable()

sink()

sink("outputs/rais/comparisons/vinc/vinculo_ativo_3112-AP.md", append = FALSE)

cat("vinculo_ativo_3112")

rais.vinc.arrow %>%
  dplyr::select(vinculo_ativo_3112, ano, area_planejamento) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        gp_var = "area_planejamento",
        par = "vinculo_ativo_3112",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = c("area_planejamento", "vinculo_ativo_3112")) %>%
  knitr::kable()

sink()

# mes_desligamento ----
sink("outputs/rais/comparisons/vinc/mes_desligamento.md", append = FALSE)

cat("mes_desligamento")

rais.vinc.arrow %>%
  dplyr::select(mes_desligamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "mes_desligamento",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "mes_desligamento") %>%
  knitr::kable()

sink()

# cat_faixa_remuneracao_media_sm ----
sink("outputs/rais/comparisons/vinc/cat_faixa_remuneracao_media_sm.md", append = FALSE)

cat("cat_faixa_remuneracao_media_sm")

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "cat_faixa_remuneracao_media_sm",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "cat_faixa_remuneracao_media_sm") %>%
  knitr::kable()

sink()

sink("outputs/rais/comparisons/vinc/cat_faixa_remuneracao_media_sm-AP.md", append = FALSE)

cat("cat_faixa_remuneracao_media_sm")

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_media_sm, ano, area_planejamento) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        gp_var = "area_planejamento",
        par = "cat_faixa_remuneracao_media_sm",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = c("area_planejamento", "cat_faixa_remuneracao_media_sm")) %>%
  knitr::kable()

sink()

# cat_faixa_remuneracao_dezembro_sm ----
sink("outputs/rais/comparisons/vinc/cat_faixa_remuneracao_dezembro_sm.md", append = FALSE)

cat("cat_faixa_remuneracao_dezembro_sm")

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "cat_faixa_remuneracao_dezembro_sm",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "cat_faixa_remuneracao_dezembro_sm") %>%
  knitr::kable()

sink()

sink("outputs/rais/comparisons/vinc/cat_faixa_remuneracao_dezembro_sm-AP.md", append = FALSE)

cat("cat_faixa_remuneracao_dezembro_sm")

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_remuneracao_dezembro_sm, ano, area_planejamento) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        gp_var = "area_planejamento",
        par = "cat_faixa_remuneracao_dezembro_sm",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = c("area_planejamento", "cat_faixa_remuneracao_dezembro_sm")) %>%
  knitr::kable()

sink()

# cat_tipo_admissao ----
sink("outputs/rais/comparisons/vinc/cat_tipo_admissao.md", append = FALSE)

cat("cat_tipo_admissao")

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_admissao, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "cat_tipo_admissao",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "cat_tipo_admissao") %>%
  knitr::kable()

sink()

sink("outputs/rais/comparisons/vinc/cat_tipo_admissao-AP.md", append = FALSE)

cat("cat_tipo_admissao")

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_admissao, ano, area_planejamento) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        gp_var = "area_planejamento",
        par = "cat_tipo_admissao",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = c("area_planejamento", "cat_tipo_admissao")) %>%
  knitr::kable()

sink()

# cat_motivo_desligamento ----
sink("outputs/rais/comparisons/vinc/cat_motivo_desligamento.md", append = FALSE)

cat("cat_motivo_desligamento")

rais.vinc.arrow %>%
  dplyr::select(cat_motivo_desligamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "cat_motivo_desligamento",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "cat_motivo_desligamento") %>%
  knitr::kable()

sink()

# cat_faixa_tempo_emprego ----
sink("outputs/rais/comparisons/vinc/cat_faixa_tempo_emprego.md", append = FALSE)

cat("cat_faixa_tempo_emprego")

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_tempo_emprego, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "cat_faixa_tempo_emprego",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "cat_faixa_tempo_emprego") %>%
  knitr::kable()

sink()

# cat_faixa_horas_contratadas ----
sink("outputs/rais/comparisons/vinc/cat_faixa_horas_contratadas.md", append = FALSE)

cat("cat_faixa_horas_contratadas")

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_horas_contratadas, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "cat_faixa_horas_contratadas",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "cat_faixa_horas_contratadas") %>%
  knitr::kable()

sink()

sink("outputs/rais/comparisons/vinc/cat_faixa_horas_contratadas-AP.md", append = FALSE)

cat("cat_faixa_horas_contratadas")

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_horas_contratadas, ano, area_planejamento) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        gp_var = "area_planejamento",
        par = "cat_faixa_horas_contratadas",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = c("area_planejamento", "cat_faixa_horas_contratadas")) %>%
  knitr::kable()

sink()

# cat_sexo ----
sink("outputs/rais/comparisons/vinc/cat_sexo.md", append = FALSE)

cat("cat_sexo")

rais.vinc.arrow %>%
  dplyr::select(cat_sexo, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "cat_sexo",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "cat_sexo") %>%
  knitr::kable()

sink()

sink("outputs/rais/comparisons/vinc/cat_sexo-AP.md", append = FALSE)

cat("cat_sexo")

rais.vinc.arrow %>%
  dplyr::select(cat_sexo, ano, area_planejamento) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        gp_var = "area_planejamento",
        par = "cat_sexo",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = c("area_planejamento", "cat_sexo")) %>%
  knitr::kable()

sink()

# cat_nacionalidade ----
sink("outputs/rais/comparisons/vinc/cat_nacionalidade.md", append = FALSE)

cat("cat_nacionalidade")

rais.vinc.arrow %>%
  dplyr::select(cat_nacionalidade, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "cat_nacionalidade",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "cat_nacionalidade") %>%
  knitr::kable()

sink()

# cat_faixa_etaria ----
sink("outputs/rais/comparisons/vinc/cat_faixa_etaria.md", append = FALSE)

cat("cat_faixa_etaria")

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_etaria, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "cat_faixa_etaria",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "cat_faixa_etaria") %>%
  knitr::kable()

sink()

sink("outputs/rais/comparisons/vinc/cat_faixa_etaria-AP.md", append = FALSE)

cat("cat_faixa_etaria")

rais.vinc.arrow %>%
  dplyr::select(cat_faixa_etaria, ano, area_planejamento) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        gp_var = "area_planejamento",
        par = "cat_faixa_etaria",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = c("area_planejamento", "cat_faixa_etaria")) %>%
  knitr::kable()

sink()

# cat_tipo_vinculo ----
sink("outputs/rais/comparisons/vinc/cat_tipo_vinculo.md", append = FALSE)

cat("cat_tipo_vinculo")

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "cat_tipo_vinculo",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "cat_tipo_vinculo") %>%
  knitr::kable()

sink()

sink("outputs/rais/comparisons/vinc/cat_tipo_vinculo-AP.md", append = FALSE)

cat("cat_tipo_vinculo")

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_vinculo, ano, area_planejamento) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        gp_var = "area_planejamento",
        par = "cat_tipo_vinculo",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = c("area_planejamento", "cat_tipo_vinculo")) %>%
  knitr::kable()

sink()

# cat_tamanho_estabelecimento ----
sink("outputs/rais/comparisons/vinc/cat_tamanho_estabelecimento.md", append = FALSE)

cat("cat_tamanho_estabelecimento")

rais.vinc.arrow %>%
  dplyr::select(cat_tamanho_estabelecimento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "cat_tamanho_estabelecimento",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "cat_tamanho_estabelecimento") %>%
  knitr::kable()

sink()

# cat_tipo_estabelecimento ----
sink("outputs/rais/comparisons/vinc/cat_tipo_estabelecimento.md", append = FALSE)

cat("cat_tipo_estabelecimento")

rais.vinc.arrow %>%
  dplyr::select(cat_tipo_estabelecimento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "cat_tipo_estabelecimento",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "cat_tipo_estabelecimento") %>%
  knitr::kable()

sink()

# cat_natureza_juridica ----
sink("outputs/rais/comparisons/vinc/cat_natureza_juridica.md", append = FALSE)

cat("cat_natureza_juridica")

rais.vinc.arrow %>%
  dplyr::select(cat_natureza_juridica, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "cat_natureza_juridica",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "cat_natureza_juridica") %>%
  knitr::kable()

sink()

# area_planejamento ----
sink("outputs/rais/comparisons/vinc/area_planejamento.md", append = FALSE)

cat("area_planejamento")

rais.vinc.arrow %>%
  dplyr::select(area_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "area_planejamento",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "area_planejamento") %>%
  knitr::kable()

sink()

# regiao_planejamento ----
sink("outputs/rais/comparisons/vinc/regiao_planejamento.md", append = FALSE)

cat("regiao_planejamento")

rais.vinc.arrow %>%
  dplyr::select(regiao_planejamento, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "regiao_planejamento",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "regiao_planejamento") %>%
  knitr::kable()

sink()

# cat_grau_instrucao_1985_2005 ----
sink("outputs/rais/comparisons/vinc/cat_grau_instrucao_1985_2005.md", append = FALSE)

cat("cat_grau_instrucao_1985_2005")

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, ano) %>%
  dplyr::filter(ano %in% ano.1) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2005),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "cat_grau_instrucao_1985_2005",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "cat_grau_instrucao_1985_2005") %>%
  knitr::kable()

sink()

sink("outputs/rais/comparisons/vinc/cat_grau_instrucao_1985_2005-AP.md", append = FALSE)

cat("cat_grau_instrucao_1985_2005")

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_1985_2005, ano, area_planejamento) %>%
  dplyr::filter(ano %in% ano.1) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2003:2005),
    function(i, j) {
      x <- freq.loop(
        df = i,
        gp_var = "area_planejamento",
        par = "cat_grau_instrucao_1985_2005",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = c("area_planejamento", "cat_grau_instrucao_1985_2005")) %>%
  knitr::kable()

sink()

# cat_raca_cor ----
sink("outputs/rais/comparisons/vinc/cat_raca_cor.md", append = FALSE)

cat("cat_raca_cor")

rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2006:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "cat_raca_cor",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "cat_raca_cor") %>%
  knitr::kable()

sink()

sink("outputs/rais/comparisons/vinc/cat_raca_cor-AP.md", append = FALSE)

cat("cat_raca_cor")

rais.vinc.arrow %>%
  dplyr::select(cat_raca_cor, ano, area_planejamento) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2006:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        gp_var = "area_planejamento",
        par = "cat_raca_cor",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = c("area_planejamento", "cat_raca_cor")) %>%
  knitr::kable()

sink()

# cat_grau_instrucao_apos_2005 ----
sink("outputs/rais/comparisons/vinc/cat_grau_instrucao_apos_2005.md", append = FALSE)

cat("cat_grau_instrucao_apos_2005")

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, ano) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2006:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "cat_grau_instrucao_apos_2005",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "cat_grau_instrucao_apos_2005") %>%
  knitr::kable()

sink()

sink("outputs/rais/comparisons/vinc/cat_grau_instrucao_apos_2005-AP.md", append = FALSE)

cat("cat_grau_instrucao_apos_2005")

rais.vinc.arrow %>%
  dplyr::select(cat_grau_instrucao_apos_2005, ano, area_planejamento) %>%
  dplyr::filter(ano %in% ano.raca_cor) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2006:2014),
    function(i, j) {
      x <- freq.loop(
        df = i,
        gp_var = "area_planejamento",
        par = "cat_grau_instrucao_apos_2005",
        ord = "default"
      ) %>%
        summarytools::tb()

      names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = c("area_planejamento", "cat_grau_instrucao_apos_2005")) %>%
  knitr::kable()

sink()
