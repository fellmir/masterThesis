### beginning ----

source("scripts/001_libraries.R")

desc.base <- c("mean", "sd", "min", "q1", "med", "q3", "max", "n.valid", "pct.valid")

fs::dir_create("outputs/census")
fs::dir_create("outputs/census/freqs/doms")
fs::dir_create("outputs/census/freqs/indv")
fs::dir_create("outputs/census/cross-tabs/doms")
fs::dir_create("outputs/census/cross-tabs/indv")
fs::dir_create("outputs/census/desc-stats/doms")
fs::dir_create("outputs/census/desc-stats/indv")
fs::dir_create("outputs/census/plots/doms")
fs::dir_create("outputs/census/plots/indv")
fs::dir_create("outputs/census/comparisons/doms")
fs::dir_create("outputs/census/comparisons/indv")

### loading census dbs ----

census.2000.doms <- readRDS("db/census/census_2000_doms.rds") %>%
  select(starts_with(c("total", "cat")) | ends_with("planejamento"))

census.2010.doms <- readRDS("db/census/census_2010_doms.rds") %>%
  select(starts_with(c("total", "cat")) | ends_with("planejamento"))

census.2000.indv <- readRDS("db/census/census_2000_indv.rds") %>%
  select(starts_with(c("rendimento", "total", "idade", "horas", "cat")) | ends_with("planejamento"))

census.2010.indv <- readRDS("db/census/census_2010_indv.rds") %>%
  select(starts_with(c("rendimento", "total", "idade", "horas", "cat")) | ends_with("planejamento"))

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
geom.bar.test <- function(df, var, gp_var = NA, name_plot) {
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
    filename = glue::glue("{name_plot}_{var}.png")
  )
}

# frequency saving (in .md)
freq.test <- function(df, gp_var = NA, par, ord = "default", db_name = NA) {
  par <- as.symbol(par)

  if (is.na(gp_var)) {
    col <- df %>%
      dplyr::select({{ par }})

    sink(glue::glue("{db_name}_{par}.md"), append = FALSE)

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

    sink(glue::glue("{db_name}_{par}_{gp_var}.md"), append = FALSE)

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
ctable.test <- function(df, x_var, y_var, gp_var = NA, db_name = NA) {
  if (is.na(gp_var)) {
    cols <- df %>%
      dplyr::select(tidyselect::all_of(c(x_var, y_var)))

    sink(glue::glue("{db_name}_{x_var}_{y_var}.md"), append = FALSE)

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

    sink(glue::glue("{db_name}_{x_var}_{y_var}_{gp_var}.md"), append = FALSE)

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
# frequency table example (for multiple dataframes)
gp_vars <- c("area_planejamento", "regiao_planejamento")

par_vars <- c("range_total_rendimentos_sm", "cat_especie_domicilio")

purrr::map(
  list(census.2000.doms, census.2010.doms),
  function(i) {
    purrr::map(
      .x = seq_len(length(gp_vars)),
      ~ freq.loop(
        df = i,
        gp_var = gp_vars[.x],
        par = par_vars[.x]
      )
    )
  }
)

# frequency table example (for all possible combinations)
purrr::map(
  .x = list(census.2000.doms, census.2010.doms),
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

purrr::map(
  .x = list(census.2000.doms, census.2010.doms),
  ~ .x %>%
    group_by(area_planejamento) %>%
    freq()
)

# cross-tabulation example
ctable.loop(
  df = census.2000.doms,
  x_var = "cat_tipo_domicilio",
  y_var = "cat_microcomputador",
  # gp_var = "area_planejamento"
)

purrr::walk2(
  .x = list(census.2000.doms, census.2010.doms),
  .y = c("Censo 2000", "Censo 2010"),
  ~ ctable.loop(
    df = .x,
    x_var = "cat_tipo_domicilio",
    y_var = "cat_microcomputador",
    gp_var = "area_planejamento",
    db_name = .y
  )
)

# descriptive statistics example (for multiple databases)
purrr::map(
  .x = list(census.2000.doms, census.2010.doms),
  ~ .x %>%
    group_by(area_planejamento) %>%
    descr(stats = desc.base)
)

# bar plot example

plots <- purrr::map(
  list(pnad_indv_2013, pnad_indv_2014),
  function(i) {
    purrr::map(
      c("cat_posicao_ocupacao", "cat_condicao_domicilio"),
      ~ geom_bar_fun(
        df = i,
        var = .x,
        gp_var = "sexo"
      )
    )
  }
)

# frequency comparison between multiple databases (w/o geo division)
purrr::map2(
  .x = list(census.2000.doms, census.2010.doms),
  .y = c(2000, 2010),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "cat_microcomputador"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "cat_microcomputador")

# frequency comparison between multiple databases (with geo division)
purrr::map2(
  .x = list(census.2000.doms, census.2010.doms),
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

### descriptive analysis - census 2000 doms ----

ls(census.2000.doms)
str(census.2000.doms)

# census 2000 doms frequencies ----
view(summarytools::freq(
  census.2000.doms$range_total_rendimentos_sm,
  report.nas = FALSE,
  cumul = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/freqs/doms/2000_range_total_rend_sm.md"
)

view(summarytools::freq(
  census.2000.doms,
  report.nas = FALSE,
  order = "freq",
  cumul = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/freqs/doms/2000_freqs.md"
)

# frequencies by area_planejamento
view(census.2000.doms %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    range_total_rendimentos_sm,
    report.nas = FALSE,
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/doms/2000_range_total_rend_sm_AP.md"
)

view(census.2000.doms %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_especie_domicilio,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/doms/2000_esp_dom_AP.md"
)

view(census.2000.doms %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_tipo_domicilio,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/doms/2000_tipo_dom_AP.md"
)

view(census.2000.doms %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_microcomputador,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/doms/2000_PC_AP.md"
)

# frequencies by regiao_planejamento
view(census.2000.doms %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    range_total_rendimentos_sm,
    report.nas = FALSE,
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/doms/2000_range_total_rend_sm_RP.md"
)

view(census.2000.doms %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_especie_domicilio,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/doms/2000_esp_dom_RP.md"
)

view(census.2000.doms %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_tipo_domicilio,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/doms/2000_tipo_dom_RP.md"
)

view(census.2000.doms %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    census.2000.doms$cat_microcomputador,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/doms/2000_PC_RP.md"
)

# census 2000 doms cross-tabulations ----
# cross-tabulations for cat_microcomputador by area_planejamento
view(summarytools::ctable(
  x = census.2000.doms$area_planejamento,
  y = census.2000.doms$cat_microcomputador,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/doms/2000_AP-PC.md"
)

# cross-tabulations for cat_microcomputador by regiao_planejamento
view(summarytools::ctable(
  x = census.2000.doms$regiao_planejamento,
  y = census.2000.doms$cat_microcomputador,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/doms/2000_RP-PC.md"
)

# cross-tabulations for cat_tipo_domicilio and cat_microcomputador per geographical division
view(stby(
  list(
    x = census.2000.doms$cat_tipo_domicilio,
    y = census.2000.doms$cat_microcomputador
  ),
  INDICES = census.2000.doms$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/doms/2000_tipo_dom-PC_AP.md"
)

view(stby(
  list(
    x = census.2000.doms$cat_tipo_domicilio,
    y = census.2000.doms$cat_microcomputador
  ),
  INDICES = census.2000.doms$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/doms/2000_tipo_dom-PC_RP.md"
)

# cross-tabulations for cat_microcomputador and range_total_rendimentos_sm per geographical division
view(stby(
  list(
    x = census.2000.doms$range_total_rendimentos_sm,
    y = census.2000.doms$cat_microcomputador
  ),
  INDICES = census.2000.doms$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/doms/2000_range_total_rend_sm-PC_AP.md"
)

view(stby(
  list(
    x = census.2000.doms$range_total_rendimentos_sm,
    y = census.2000.doms$cat_microcomputador
  ),
  INDICES = census.2000.doms$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/doms/2000_range_total_rend_sm-PC_RP.md"
)

# cross-tabulations for cat_tipo_domicilio and range_total_rendimentos_sm per geographical division
view(stby(
  list(
    x = census.2000.doms$range_total_rendimentos_sm,
    y = census.2000.doms$cat_tipo_domicilio
  ),
  INDICES = census.2000.doms$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/doms/2000_range_total_rend_sm-tipo_dom_AP.md"
)

view(stby(
  list(
    x = census.2000.doms$range_total_rendimentos_sm,
    y = census.2000.doms$cat_tipo_domicilio
  ),
  INDICES = census.2000.doms$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/doms/2000_range_total_rend_sm-tipo_dom_RP.md"
)

# census 2000 doms descriptive statistics ----
# descriptive statistics of numerical variables
view(summarytools::descr(
  census.2000.doms,
  # headings = FALSE,
  stats = desc.base,
  # transpose = TRUE
),
file =
  "outputs/census/desc-stats/doms/2000_desc_stats.md"
)

# descriptive statistics by geographical grouping
view(stby(
  data = census.2000.doms,
  INDICES = census.2000.doms$area_planejamento,
  FUN = descr,
  stats = desc.base
),
file =
  "outputs/census/desc-stats/doms/2000_desc_stats_AP.md"
)

view(stby(
  data = census.2000.doms,
  INDICES = census.2000.doms$regiao_planejamento,
  FUN = descr,
  stats = desc.base
),
file =
  "outputs/census/desc-stats/doms/2000_desc_stats_RP.md"
)

# OR:
# census.2000.doms %>%
#   group_by(area_planejamento) %>%
#   descr(stats = desc.base)
#
# census.2000.doms %>%
#   group_by(regiao_planejamento) %>%
#   descr(stats = desc.base)

# data frame summary
view(summarytools::dfSummary(census.2000.doms),
  file =
    "outputs/census/desc-stats/doms/2000_df_summary.md"
)

# census 2000 doms plots ----
# barplot (for categorical variables)
census.2000.doms %>%
  tidyr::drop_na(range_total_rendimentos_sm) %>%
  ggplot() +
  aes(x = range_total_rendimentos_sm) +
  geom_bar() +
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  ggtitle("census.2000.doms$range_total_rendimentos_sm") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/doms/2000_range_total_rendimentos_sm.png"
  )

census.2000.doms %>%
  tidyr::drop_na(cat_especie_domicilio) %>%
  ggplot() +
  aes(x = cat_especie_domicilio) +
  geom_bar() +
  theme_bw() +
  ggtitle("census.2000.doms$cat_especie_domicilio") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/doms/2000_cat_especie_domicilio.png"
  )

census.2000.doms %>%
  tidyr::drop_na(cat_tipo_domicilio) %>%
  ggplot() +
  aes(x = cat_tipo_domicilio) +
  geom_bar() +
  theme_bw() +
  ggtitle("census.2000.doms$cat_tipo_domicilio") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/doms/2000_cat_tipo_domicilio.png"
  )

census.2000.doms %>%
  tidyr::drop_na(cat_microcomputador) %>%
  ggplot() +
  aes(x = cat_microcomputador) +
  geom_bar() +
  theme_bw() +
  ggtitle("census.2000.doms$cat_microcomputador") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/doms/2000_cat_microcomputador.png"
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

### descriptive analysis - census 2010 doms ----

ls(census.2010.doms)
str(census.2010.doms)

# census 2010 doms frequencies ----
view(summarytools::freq(
  census.2010.doms$range_total_rendimentos_sm,
  report.nas = FALSE,
  cumul = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/freqs/doms/2010_range_total_rend_sm.md"
)

view(summarytools::freq(
  census.2010.doms,
  report.nas = FALSE,
  order = "freq",
  cumul = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/freqs/doms/2010_freqs.md"
)

# frequencies by area_planejamento
view(census.2010.doms %>%
  group_by(area_planejamento) %>%
  summarytools::freq(range_total_rendimentos_sm,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/doms/2010_range_total_rend_sm_AP.md"
)

view(census.2010.doms %>%
  group_by(area_planejamento) %>%
  summarytools::freq(cat_especie_domicilio,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/doms/2010_esp_dom_AP.md"
)

view(census.2010.doms %>%
  group_by(area_planejamento) %>%
  summarytools::freq(cat_tipo_domicilio,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/doms/2010_tipo_dom_AP.md"
)

view(census.2010.doms %>%
  group_by(area_planejamento) %>%
  summarytools::freq(cat_microcomputador,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/doms/2010_PC_AP.md"
)

# frequencies by regiao_planejamento
view(census.2010.doms %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(range_total_rendimentos_sm,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/doms/2010_esp_dom_RP.md"
)

view(census.2010.doms %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(cat_especie_domicilio,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/doms/2010_esp_dom_RP.md"
)

view(census.2010.doms %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(cat_tipo_domicilio,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/doms/2010_tipo_dom_RP.md"
)

view(census.2010.doms %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(cat_microcomputador,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/doms/2010_PC_RP.md"
)

# census 2010 doms cross-tabulations ----
# cross-tabulations for cat_microcomputador by area_planejamento
view(summarytools::ctable(
  x = census.2010.doms$area_planejamento,
  y = census.2010.doms$cat_microcomputador,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/doms/2010_AP-PC.md"
)

# cross-tabulations for cat_microcomputador by area_planejamento
view(summarytools::ctable(
  x = census.2010.doms$regiao_planejamento,
  y = census.2010.doms$cat_microcomputador,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/doms/2010_RP-PC.md"
)

# cross-tabulations for cat_tipo_domicilio and cat_microcomputador per geographical division
view(stby(
  list(
    x = census.2010.doms$cat_tipo_domicilio,
    y = census.2010.doms$cat_microcomputador
  ),
  INDICES = census.2010.doms$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/doms/2010_tipo_dom-PC_AP.md"
)

view(stby(
  list(
    x = census.2010.doms$cat_tipo_domicilio,
    y = census.2010.doms$cat_microcomputador
  ),
  INDICES = census.2010.doms$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/doms/2010_tipo_dom-PC_RP.md"
)

# cross-tabulations for cat_microcomputador and range_total_rendimentos_sm per geographical division
view(stby(
  list(
    x = census.2010.doms$range_total_rendimentos_sm,
    y = census.2010.doms$cat_microcomputador
  ),
  INDICES = census.2010.doms$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/doms/2010_range_total_rend_sm-PC_AP.md"
)

view(stby(
  list(
    x = census.2010.doms$range_total_rendimentos_sm,
    y = census.2010.doms$cat_microcomputador
  ),
  INDICES = census.2010.doms$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/doms/2010_range_total_rend_sm-PC_RP.md"
)

# cross-tabulations for cat_tipo_domicilio and range_total_rendimentos_sm per geographical division
view(stby(
  list(
    x = census.2010.doms$range_total_rendimentos_sm,
    y = census.2010.doms$cat_tipo_domicilio
  ),
  INDICES = census.2010.doms$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/doms/2010_range_total_rend_sm-tipo_dom_AP.md"
)

view(stby(
  list(
    x = census.2010.doms$range_total_rendimentos_sm,
    y = census.2010.doms$cat_tipo_domicilio
  ),
  INDICES = census.2010.doms$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/doms/2010_range_total_rend_sm-tipo_dom_RP.md"
)

# census 2010 doms descriptive statistics ----
# descriptive statistics of numerical variables
view(summarytools::descr(
  census.2010.doms,
  # headings = FALSE,
  stats = desc.base,
  # transpose = TRUE
),
file =
  "outputs/census/desc-stats/doms/2010_desc_stats.md"
)

# descriptive statistics by geographical grouping
view(stby(
  data = census.2010.doms,
  INDICES = census.2010.doms$area_planejamento,
  FUN = descr,
  stats = desc.base
),
file =
  "outputs/census/desc-stats/doms/2010_desc_stats_AP.md"
)

view(stby(
  data = census.2010.doms,
  INDICES = census.2010.doms$regiao_planejamento,
  FUN = descr,
  stats = desc.base
),
file =
  "outputs/census/desc-stats/doms/2010_desc_stats_RP.md"
)

# OR:
# census.2010.doms %>%
#   group_by(area_planejamento) %>%
#   descr(stats = desc.base)
#
# census.2010.doms %>%
#   group_by(regiao_planejamento) %>%
#   descr(stats = desc.base)

# data frame summary
view(summarytools::dfSummary(census.2010.doms),
  file =
    "outputs/census/desc-stats/doms/2010_df_summary.md"
)

# census 2010 doms plots ----
# barplot (for categorical variables)
census.2010.doms %>%
  tidyr::drop_na(range_total_rendimentos_sm) %>%
  ggplot() +
  aes(x = range_total_rendimentos_sm) +
  geom_bar() +
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  ggtitle("census.2010.doms$range_total_rendimentos_sm") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/doms/2010_range_total_rendimentos_sm.png"
  )

census.2010.doms %>%
  tidyr::drop_na(cat_especie_domicilio) %>%
  ggplot() +
  aes(x = cat_especie_domicilio) +
  geom_bar() +
  theme_bw() +
  ggtitle("census.2010.doms$cat_especie_domicilio") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/doms/2010_cat_especie_domicilio.png"
  )

census.2010.doms %>%
  tidyr::drop_na(cat_tipo_domicilio) %>%
  ggplot() +
  aes(x = cat_tipo_domicilio) +
  geom_bar() +
  theme_bw() +
  ggtitle("census.2010.doms$cat_tipo_domicilio") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/doms/2010_cat_tipo_domicilio.png"
  )

census.2010.doms %>%
  tidyr::drop_na(cat_microcomputador) %>%
  ggplot() +
  aes(x = cat_microcomputador) +
  geom_bar() +
  theme_bw() +
  ggtitle("census.2010.doms$cat_microcomputador") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/doms/2010_cat_microcomputador.png"
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

### descriptive analysis - census 2000 doms x census 2010 doms ----

# cat_microcomputador ----
# WITHOUT geo division
sink("outputs/census/comparisons/doms/cat_microcomputador.md", append = FALSE)

cat("cat_microcomputador")

purrr::map2(
  .x = list(census.2000.doms, census.2010.doms),
  .y = c(2000, 2010),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "cat_microcomputador"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "cat_microcomputador") %>%
  knitr::kable()

sink()

# WITH geo division
sink("outputs/census/comparisons/doms/cat_microcomputador-AP.md", append = FALSE)

cat("cat_microcomputador-AP")

purrr::map2(
  .x = list(census.2000.doms, census.2010.doms),
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
  purrr::reduce(dplyr::left_join, by = c("area_planejamento", "cat_microcomputador")) %>%
  # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for small dataframes
  knitr::kable()

sink()

sink("outputs/census/comparisons/doms/cat_microcomputador-RP.md", append = FALSE)

cat("cat_microcomputador-RP")

purrr::map2(
  .x = list(census.2000.doms, census.2010.doms),
  .y = c(2000, 2010),
  function(i, j) {
    x <- freq.loop(
      df = i,
      gp_var = "regiao_planejamento",
      par = "cat_microcomputador"
    ) %>%
      summarytools::tb()
    # %>% dplyr::filter(regiao_planejamento == "Baixada de Jacarepaguá") for big dataframes

    names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = c("regiao_planejamento", "cat_microcomputador")) %>%
  # %>% dplyr::filter(regiao_planejamento == "Baixada de Jacarepaguá") for small dataframes
  knitr::kable()

sink()

# range_total_rendimentos_sm ----
# WITHOUT geo division
sink("outputs/census/comparisons/doms/range_total_rendimentos_sm.md", append = FALSE)

cat("range_total_rendimentos_sm")

purrr::map2(
  .x = list(census.2000.doms, census.2010.doms),
  .y = c(2000, 2010),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "range_total_rendimentos_sm",
      ord = "default"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "range_total_rendimentos_sm") %>%
  knitr::kable()

sink()

# WITH geo division
sink("outputs/census/comparisons/doms/range_total_rendimentos_sm-AP.md", append = FALSE)

cat("range_total_rendimentos_sm-AP")

purrr::map2(
  .x = list(census.2000.doms, census.2010.doms),
  .y = c(2000, 2010),
  function(i, j) {
    x <- freq.loop(
      df = i,
      gp_var = "area_planejamento",
      par = "range_total_rendimentos_sm",
      ord = "default"
    ) %>%
      summarytools::tb()
    # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for big dataframes

    names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = c("area_planejamento", "range_total_rendimentos_sm")) %>%
  # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for small dataframes
  knitr::kable()

sink()

sink("outputs/census/comparisons/doms/range_total_rendimentos_sm-RP.md", append = FALSE)

cat("range_total_rendimentos_sm-RP")

purrr::map2(
  .x = list(census.2000.doms, census.2010.doms),
  .y = c(2000, 2010),
  function(i, j) {
    x <- freq.loop(
      df = i,
      gp_var = "regiao_planejamento",
      par = "range_total_rendimentos_sm",
      ord = "default"
    ) %>%
      summarytools::tb()
    # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for big dataframes

    names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = c("regiao_planejamento", "range_total_rendimentos_sm")) %>%
  # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for small dataframes
  knitr::kable()

sink()

### descriptive analysis - census 2000 indv ----

ls(census.2000.indv)
str(census.2000.indv)

# census 2000 indv frequencies ----
view(summarytools::freq(
  census.2000.indv$range_total_rendimentos_sm,
  report.nas = FALSE,
  cumul = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/freqs/indv/2000_range_total_rend_sm.md"
)

view(summarytools::freq(
  census.2000.indv$range_idade,
  report.nas = FALSE,
  cumul = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/freqs/indv/2000_range_idade.md"
)

view(summarytools::freq(
  census.2000.indv$total_horas_trabalhadas,
  report.nas = FALSE,
  order = "freq",
  cumul = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/freqs/indv/2000_total_hrs_trab.md"
)

view(summarytools::freq(
  census.2000.indv$idade_anos_completos,
  report.nas = FALSE,
  order = "freq",
  cumul = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/freqs/indv/2000_idade_comp.md"
)

view(summarytools::freq(
  census.2000.indv,
  report.nas = FALSE,
  order = "freq",
  cumul = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/freqs/indv/2000_freqs.md"
)

# frequencies by area_planejamento ----
view(census.2000.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    range_total_rendimentos_sm,
    report.nas = FALSE,
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_range_total_rend_sm_AP.md"
)

view(census.2000.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    range_idade,
    report.nas = FALSE,
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_range_idade_AP.md"
)

view(census.2000.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    total_horas_trabalhadas,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_total_hrs_trab_AP.md"
)

view(census.2000.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    idade_anos_completos,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_idade_comp_AP.md"
)

view(census.2000.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_anos_estudo,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_anos_estd_AP.md"
)

view(census.2000.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_cod_antigo_atividade,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_cod_ant_ativ_AP.md"
)

view(census.2000.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_cod_antigo_ocupacao,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_cod_ant_ocup_AP.md"
)

view(census.2000.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_concluiu_curso,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_conc_curso_AP.md"
)

view(census.2000.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_contribuicao_inss,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_INSS_AP.md"
)

view(census.2000.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_cor_raca,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_cor_raca_AP.md"
)

view(census.2000.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_qtd_trabalhos_julho_2000,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_qtd_trab_AP.md"
)


view(census.2000.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_serie_aprovacao,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_serie_aprov_AP.md"
)

view(census.2000.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_sexo,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_sexo_AP.md"
)

view(census.2000.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_trabalhou,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_trabalhou_AP.md"
)

# frequencies by regiao_planejamento ----
view(census.2000.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    range_total_rendimentos_sm,
    report.nas = FALSE,
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_range_total_rend_sm_RP.md"
)

view(census.2000.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    range_idade,
    report.nas = FALSE,
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_range_idade_RP.md"
)

view(census.2000.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    total_horas_trabalhadas,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_total_hrs_trab_RP.md"
)

view(census.2000.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    idade_anos_completos,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_idade_comp_RP.md"
)

view(census.2000.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_anos_estudo,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_anos_estd_RP.md"
)

view(census.2000.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_cod_antigo_atividade,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_cod_ant_ativ_RP.md"
)

view(census.2000.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_cod_antigo_ocupacao,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_cod_ant_ocup_RP.md"
)

view(census.2000.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_concluiu_curso,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_conc_curso_RP.md"
)

view(census.2000.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_contribuicao_inss,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_INSS_RP.md"
)

view(census.2000.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_cor_raca,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_cor_raca_RP.md"
)

view(census.2000.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_qtd_trabalhos_julho_2000,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_qtd_trab_RP.md"
)


view(census.2000.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_serie_aprovacao,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_serie_aprov_RP.md"
)

view(census.2000.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_sexo,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_sexo_RP.md"
)

view(census.2000.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_trabalhou,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2000_trabalhou_RP.md"
)

# census 2000 indv cross-tabulations ----
# cross-tabulations by range_total_rendimentos_sm
view(summarytools::ctable(
  x = census.2000.indv$range_total_rendimentos_sm,
  y = census.2000.indv$cat_anos_estudo,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2000_range_total_rend-anos_estd.md"
)

view(summarytools::ctable(
  x = census.2000.indv$range_total_rendimentos_sm,
  y = census.2000.indv$cat_contribuicao_inss,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2000_range_total_rend-INSS.md"
)

view(summarytools::ctable(
  x = census.2000.indv$range_total_rendimentos_sm,
  y = census.2000.indv$cat_cor_raca,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2000_range_total_rend-raca.md"
)

view(summarytools::ctable(
  x = census.2000.indv$range_total_rendimentos_sm,
  y = census.2000.indv$cat_sexo,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2000_range_total_rend-sexo.md"
)

view(summarytools::ctable(
  x = census.2000.indv$range_total_rendimentos_sm,
  y = census.2000.indv$cat_trabalhou_julho_2000,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2000_range_total_rend-trabalhou.md"
)

view(summarytools::ctable(
  x = census.2000.indv$range_total_rendimentos_sm,
  y = census.2000.indv$range_idade,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2000_range_total_rend-range_idade.md"
)

# cross-tabulations by area_planejamento ----
view(summarytools::ctable(
  x = census.2000.indv$area_planejamento,
  y = census.2000.indv$cat_anos_estudo,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2000_AP-anos_estd.md"
)

view(summarytools::ctable(
  x = census.2000.indv$area_planejamento,
  y = census.2000.indv$cat_contribuicao_inss,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2000_AP-INSS.md"
)

view(summarytools::ctable(
  x = census.2000.indv$area_planejamento,
  y = census.2000.indv$cat_cor_raca,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2000_AP-raca.md"
)

view(summarytools::ctable(
  x = census.2000.indv$area_planejamento,
  y = census.2000.indv$cat_qtd_trabalhos_julho_2000,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2000_AP-qtd_trab.md"
)

view(summarytools::ctable(
  x = census.2000.indv$area_planejamento,
  y = census.2000.indv$cat_sexo,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2000_AP-sexo.md"
)

view(summarytools::ctable(
  x = census.2000.indv$area_planejamento,
  y = census.2000.indv$cat_trabalhou,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2000_AP-trabalhou.md"
)

# cross-tabulations by regiao_planejamento ----
view(summarytools::ctable(
  x = census.2000.indv$regiao_planejamento,
  y = census.2000.indv$cat_anos_estudo,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2000_RP-anos_estd.md"
)

view(summarytools::ctable(
  x = census.2000.indv$regiao_planejamento,
  y = census.2000.indv$cat_contribuicao_inss,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2000_RP-INSS.md"
)

view(summarytools::ctable(
  x = census.2000.indv$regiao_planejamento,
  y = census.2000.indv$cat_cor_raca,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2000_RP-raca.md"
)

view(summarytools::ctable(
  x = census.2000.indv$regiao_planejamento,
  y = census.2000.indv$cat_qtd_trabalhos_julho_2000,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2000_RP-qtd_trab.md"
)

view(summarytools::ctable(
  x = census.2000.indv$regiao_planejamento,
  y = census.2000.indv$cat_sexo,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2000_RP-sexo.md"
)

view(summarytools::ctable(
  x = census.2000.indv$regiao_planejamento,
  y = census.2000.indv$cat_trabalhou,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2000_RP-trabalhou.md"
)

# cross-tabulations per geographical division (area_planejamento) ----
view(stby(
  list(
    x = census.2000.indv$cat_cor_raca,
    y = census.2000.indv$cat_anos_estudo
  ),
  INDICES = census.2000.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_raca-anos_estd_AP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_cor_raca,
    y = census.2000.indv$cat_concluiu_curso
  ),
  INDICES = census.2000.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_raca-conc_curso_AP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_cor_raca,
    y = census.2000.indv$cat_contribuicao_inss
  ),
  INDICES = census.2000.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_raca-INSS_AP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_cor_raca,
    y = census.2000.indv$cat_qtd_trabalhos_julho_2000
  ),
  INDICES = census.2000.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_raca-qtd_trab_AP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_cor_raca,
    y = census.2000.indv$cat_sexo
  ),
  INDICES = census.2000.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_raca-sexo_AP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_cor_raca,
    y = census.2000.indv$cat_trabalhou
  ),
  INDICES = census.2000.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_raca-trabalhou_AP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_sexo,
    y = census.2000.indv$cat_anos_estudo
  ),
  INDICES = census.2000.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_sexo-anos_estd_AP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_sexo,
    y = census.2000.indv$cat_concluiu_curso
  ),
  INDICES = census.2000.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_sexo-conc_curso_AP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_sexo,
    y = census.2000.indv$cat_contribuicao_inss
  ),
  INDICES = census.2000.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_sexo-INSS_AP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_sexo,
    y = census.2000.indv$cat_qtd_trabalhos_julho_2000
  ),
  INDICES = census.2000.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_sexo-qtd_trab_AP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_sexo,
    y = census.2000.indv$cat_trabalhou
  ),
  INDICES = census.2000.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_sexo-trabalhou_AP.md"
)

# cross-tabulations per geographical division (regiao_planejamento) ----
view(stby(
  list(
    x = census.2000.indv$cat_cor_raca,
    y = census.2000.indv$cat_anos_estudo
  ),
  INDICES = census.2000.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_raca-anos_estd_RP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_cor_raca,
    y = census.2000.indv$cat_concluiu_curso
  ),
  INDICES = census.2000.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_raca-conc_curso_RP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_cor_raca,
    y = census.2000.indv$cat_contribuicao_inss
  ),
  INDICES = census.2000.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_raca-INSS_RP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_cor_raca,
    y = census.2000.indv$cat_qtd_trabalhos_julho_2000
  ),
  INDICES = census.2000.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_raca-qtd_trab_RP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_cor_raca,
    y = census.2000.indv$cat_sexo
  ),
  INDICES = census.2000.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_raca-sexo_RP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_cor_raca,
    y = census.2000.indv$cat_trabalhou
  ),
  INDICES = census.2000.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_raca-trabalhou_RP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_sexo,
    y = census.2000.indv$cat_anos_estudo
  ),
  INDICES = census.2000.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_sexo-anos_estd_RP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_sexo,
    y = census.2000.indv$cat_concluiu_curso
  ),
  INDICES = census.2000.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_sexo-conc_curso_RP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_sexo,
    y = census.2000.indv$cat_contribuicao_inss
  ),
  INDICES = census.2000.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_sexo-INSS_RP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_sexo,
    y = census.2000.indv$cat_qtd_trabalhos_julho_2000
  ),
  INDICES = census.2000.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_sexo-qtd_trab_RP.md"
)

view(stby(
  list(
    x = census.2000.indv$cat_sexo,
    y = census.2000.indv$cat_trabalhou
  ),
  INDICES = census.2000.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2000_sexo-trabalhou_RP.md"
)

# census 2000 indv descriptive statistics ----
# descriptive statistics of numerical variables
view(summarytools::descr(
  census.2000.indv,
  # headings = FALSE,
  stats = desc.base,
  # transpose = TRUE
),
file =
  "outputs/census/desc-stats/indv/2000_desc_stats.md"
)

# descriptive statistics by geographical group
# view(stby(
#   data = census.2000.indv,
#   INDICES = census.2000.indv$area_planejamento,
#   FUN = descr,
#   stats = desc.base
# ),
# file =
#   "outputs/census/desc-stats/indv/2000_desc_stats_AP.md")
#
# view(stby(
#   data = census.2000.indv,
#   INDICES = census.2000.indv$regiao_planejamento,
#   FUN = descr,
#   stats = desc.base
# ),
# file =
#   "outputs/census/desc-stats/indv/2000_desc_stats_RP.md")

# OR:
view(census.2000.indv %>%
  select(-c(
    "horas_trabalhadas_principal", "idade_meses",
    "rendimento_aposentadoria_pensao", "rendimento_bruto_principal",
    "total_rendimentos_principal", "total_rendimentos_principal_sm"
  )) %>%
  group_by(area_planejamento) %>%
  descr(stats = desc.base),
file =
  "outputs/census/desc-stats/indv/2000_desc_stats_AP.md"
)

view(census.2000.indv %>%
  select(-c(
    "horas_trabalhadas_principal", "idade_meses",
    "rendimento_aposentadoria_pensao", "rendimento_bruto_principal",
    "total_rendimentos_principal", "total_rendimentos_principal_sm"
  )) %>%
  group_by(regiao_planejamento) %>%
  descr(stats = desc.base),
file =
  "outputs/census/desc-stats/indv/2000_desc_stats_RP.md"
)

# data frame summary
view(summarytools::dfSummary(census.2000.indv),
  file = "outputs/census/desc-stats/indv/2000_df_summary.md"
)

# census 2000 indv plots ----
# barplot (for categorical variables)
census.2000.indv %>%
  tidyr::drop_na(range_total_rendimentos_sm) %>%
  ggplot() +
  aes(x = range_total_rendimentos_sm) +
  geom_bar() +
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  ggtitle("census.2000.indv$range_total_rendimentos_sm") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2000_range_total_rendimentos_sm.png"
  )

census.2000.indv %>%
  tidyr::drop_na(range_idade) %>%
  ggplot() +
  aes(x = range_idade) +
  geom_bar() +
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  ggtitle("census.2000.indv$range_idade") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2000_range_idade.png"
  )

census.2000.indv %>%
  tidyr::drop_na(cat_concluiu_curso) %>%
  ggplot() +
  aes(x = cat_concluiu_curso) +
  geom_bar() +
  theme_bw() +
  ggtitle("census.2000.indv$cat_concluiu_curso") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2000_cat_concluiu_curso.png"
  )

census.2000.indv %>%
  tidyr::drop_na(cat_contribuicao_inss) %>%
  ggplot() +
  aes(x = cat_contribuicao_inss) +
  geom_bar() +
  theme_bw() +
  ggtitle("census.2000.indv$cat_contribuicao_inss") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2000_cat_contribuicao_inss.png"
  )

census.2000.indv %>%
  tidyr::drop_na(cat_cor_raca) %>%
  ggplot() +
  aes(x = cat_cor_raca) +
  geom_bar() +
  theme_bw() +
  ggtitle("census.2000.indv$cat_cor_raca") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2000_cat_cor_raca.png"
  )

census.2000.indv %>%
  tidyr::drop_na(cat_qtd_trabalhos_julho_2000) %>%
  ggplot() +
  aes(x = cat_qtd_trabalhos_julho_2000) +
  geom_bar() +
  theme_bw() +
  ggtitle("census.2000.indv$cat_qtd_trabalhos_julho_2000") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2000_cat_qtd_trabalhos_julho_2000.png"
  )

census.2000.indv %>%
  tidyr::drop_na(cat_serie_aprovacao) %>%
  ggplot() +
  aes(x = cat_serie_aprovacao) +
  geom_bar() +
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  ggtitle("census.2000.indv$cat_serie_aprovacao") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2000_cat_serie_aprovacao.png"
  )

census.2000.indv %>%
  tidyr::drop_na(cat_sexo) %>%
  ggplot() +
  aes(x = cat_sexo) +
  geom_bar() +
  theme_bw() +
  ggtitle("census.2000.indv$cat_sexo") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2000_cat_sexo.png"
  )

census.2000.indv %>%
  tidyr::drop_na(cat_trabalhou) %>%
  ggplot() +
  aes(x = cat_trabalhou) +
  geom_bar() +
  theme_bw() +
  ggtitle("census.2000.indv$cat_trabalhou") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2000_cat_trabalhou.png"
  )

# adjust the height!
census.2000.indv %>%
  tidyr::drop_na(cat_anos_estudo) %>%
  ggplot() +
  aes(x = cat_anos_estudo) +
  geom_bar() +
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  ggtitle("census.2000.indv$cat_anos_estudo") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2000_cat_anos_estudo.png"
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

### descriptive analysis - census 2010 indv ----

ls(census.2010.indv)
str(census.2010.indv)

# census 2010 indv frequencies ----
# frequencies
view(summarytools::freq(
  census.2010.indv$range_total_rendimentos_sm,
  report.nas = FALSE,
  cumul = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/freqs/indv/2010_range_total_rend_sm.md"
)


view(summarytools::freq(
  census.2010.indv$range_idade,
  report.nas = FALSE,
  cumul = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/freqs/indv/2010_range_idade.md"
)

view(summarytools::freq(
  census.2010.indv$horas_trabalhadas_principal,
  report.nas = FALSE,
  order = "freq",
  cumul = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/freqs/indv/2010_hrs_trab.md"
)

view(summarytools::freq(
  census.2010.indv$idade_anos_completos,
  report.nas = FALSE,
  order = "freq",
  cumul = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/freqs/indv/2010_idade_comp.md"
)

view(summarytools::freq(
  census.2010.indv,
  report.nas = FALSE,
  order = "freq",
  cumul = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/freqs/indv/2010_freqs.md"
)

# frequencies by area_planejamento ----
view(census.2010.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    range_total_rendimentos_sm,
    report.nas = FALSE,
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_range_total_rend_sm_AP.md"
)

view(census.2010.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    horas_trabalhadas_principal,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_hrs_trab_AP.md"
)

view(census.2010.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    idade_anos_completos,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_idade_comp_AP.md"
)

view(census.2010.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_contribuicao_inss,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_INSS_AP.md"
)

view(census.2010.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_cor_raca,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_raca_AP.md"
)

view(census.2010.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_curso_mais_elevado,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_curso_elev_AP.md"
)

view(census.2010.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_nivel_instrucao,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_nv_estd_AP.md"
)

view(census.2010.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_qtd_trabalhos_julho_2010,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_qtd_trab_AP.md"
)

view(census.2010.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_rendimento_aposentadoria_pensao,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_rend_apos_pensao_AP.md"
)

view(census.2010.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_rendimento_bruto_principal,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_rend_princ_AP.md"
)

view(census.2010.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_sexo,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_sexo_AP.md"
)

view(census.2010.indv %>%
  group_by(area_planejamento) %>%
  summarytools::freq(
    cat_trabalhou,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_trabalhou_AP.md"
)

# frequencies by regiao_planejamento ----
view(census.2010.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    range_total_rendimentos_sm,
    report.nas = FALSE,
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_range_total_rend_sm_RP.md"
)

view(census.2010.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    horas_trabalhadas_principal,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_hrs_trab_RP.md"
)

view(census.2010.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    idade_anos_completos,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_idade_comp_RP.md"
)

view(census.2010.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_contribuicao_inss,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_INSS_RP.md"
)

view(census.2010.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_cor_raca,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_raca_RP.md"
)

view(census.2010.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_curso_mais_elevado,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_curso_elev_RP.md"
)

view(census.2010.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_nivel_instrucao,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_nv_estd_RP.md"
)

view(census.2010.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_qtd_trabalhos_julho_2010,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_qtd_trab_RP.md"
)

view(census.2010.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_rendimento_aposentadoria_pensao,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_rend_apos_pensao_RP.md"
)

view(census.2010.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_rendimento_bruto_principal,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_rend_princ_RP.md"
)

view(census.2010.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_sexo,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_sexo_RP.md"
)

view(census.2010.indv %>%
  group_by(regiao_planejamento) %>%
  summarytools::freq(
    cat_trabalhou,
    report.nas = FALSE,
    order = "freq",
    cumul = FALSE,
    # headings = FALSE
  ),
file =
  "outputs/census/freqs/indv/2010_trabalhou_RP.md"
)

# census 2010 indv cross-tabulations ----
# cross-tabulations by range_total_rendimentos_sm
view(summarytools::ctable(
  x = census.2010.indv$range_total_rendimentos_sm,
  y = census.2010.indv$cat_curso_mais_elevado,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_range_total_rend_sm-curso_elev.md"
)

view(summarytools::ctable(
  x = census.2010.indv$range_total_rendimentos_sm,
  y = census.2010.indv$cat_nivel_instrucao,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_range_total_rend_sm-nv_estd.md"
)

view(summarytools::ctable(
  x = census.2010.indv$range_total_rendimentos_sm,
  y = census.2010.indv$cat_contribuicao_inss,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_range_total_rend_sm-INSS.md"
)

view(summarytools::ctable(
  x = census.2010.indv$range_total_rendimentos_sm,
  y = census.2010.indv$cat_cor_raca,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_range_total_rend_sm-raca.md"
)

view(summarytools::ctable(
  x = census.2010.indv$range_total_rendimentos_sm,
  y = census.2010.indv$cat_sexo,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_range_total_rend_sm-sexo.md"
)

view(summarytools::ctable(
  x = census.2010.indv$range_total_rendimentos_sm,
  y = census.2010.indv$cat_trabalhou,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_range_total_rend_sm-trabalhou.md"
)

view(summarytools::ctable(
  x = census.2010.indv$range_total_rendimentos_sm,
  y = census.2010.indv$range_idade,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_range_total_rend_sm-range_idade.md"
)

# cross-tabulations by area_planejamento ----
view(summarytools::ctable(
  x = census.2010.indv$area_planejamento,
  y = census.2010.indv$cat_contribuicao_inss,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_AP-INSS.md"
)

view(summarytools::ctable(
  x = census.2010.indv$area_planejamento,
  y = census.2010.indv$cat_cor_raca,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_AP-raca.md"
)

view(summarytools::ctable(
  x = census.2010.indv$area_planejamento,
  y = census.2010.indv$cat_curso_mais_elevado,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_AP-curso_elev.md"
)

view(summarytools::ctable(
  x = census.2010.indv$area_planejamento,
  y = census.2010.indv$cat_nivel_instrucao,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_AP-nv_estd.md"
)

view(summarytools::ctable(
  x = census.2010.indv$area_planejamento,
  y = census.2010.indv$cat_qtd_trabalhos_julho_2010,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_AP-qtd_trab.md"
)

view(summarytools::ctable(
  x = census.2010.indv$area_planejamento,
  y = census.2010.indv$cat_rendimento_aposentadoria_pensao,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_AP-rend_apos_pensao.md"
)

view(summarytools::ctable(
  x = census.2010.indv$area_planejamento,
  y = census.2010.indv$cat_rendimento_bruto_principal,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_AP-rend_princ.md"
)

view(summarytools::ctable(
  x = census.2010.indv$area_planejamento,
  y = census.2010.indv$cat_sexo,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_AP-sexo.md"
)

view(summarytools::ctable(
  x = census.2010.indv$area_planejamento,
  y = census.2010.indv$cat_trabalhou,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_AP-trabalhou.md"
)

# cross-tabulations by regiao_planejamento ----
view(summarytools::ctable(
  x = census.2010.indv$regiao_planejamento,
  y = census.2010.indv$cat_contribuicao_inss,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_RP-INSS.md"
)

view(summarytools::ctable(
  x = census.2010.indv$regiao_planejamento,
  y = census.2010.indv$cat_cor_raca,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_RP-raca.md"
)

view(summarytools::ctable(
  x = census.2010.indv$regiao_planejamento,
  y = census.2010.indv$cat_curso_mais_elevado,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_RP-curso_elev.md"
)

view(summarytools::ctable(
  x = census.2010.indv$regiao_planejamento,
  y = census.2010.indv$cat_nivel_instrucao,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_RP-nv_estd.md"
)

view(summarytools::ctable(
  x = census.2010.indv$regiao_planejamento,
  y = census.2010.indv$cat_qtd_trabalhos_julho_2010,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_RP-qtd_trab.md"
)

view(summarytools::ctable(
  x = census.2010.indv$regiao_planejamento,
  y = census.2010.indv$cat_rendimento_aposentadoria_pensao,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_RP-rend_apos_pensao.md"
)

view(summarytools::ctable(
  x = census.2010.indv$regiao_planejamento,
  y = census.2010.indv$cat_rendimento_bruto_principal,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_RP-rend_princ.md"
)

view(summarytools::ctable(
  x = census.2010.indv$regiao_planejamento,
  y = census.2010.indv$cat_sexo,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_RP-sexo.md"
)

view(summarytools::ctable(
  x = census.2010.indv$regiao_planejamento,
  y = census.2010.indv$cat_trabalhou,
  prop = "r",
  useNA = "no",
  totals = FALSE,
  # headings = FALSE
),
file =
  "outputs/census/cross-tabs/indv/2010_RP-trabalhou.md"
)

# cross-tabulations per geographical division (area_planejamento) ----
view(stby(
  list(
    x = census.2010.indv$cat_cor_raca,
    y = census.2010.indv$cat_contribuicao_inss
  ),
  INDICES = census.2010.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-INSS_AP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_cor_raca,
    y = census.2010.indv$cat_curso_mais_elevado
  ),
  INDICES = census.2010.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-curso_elev_AP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_cor_raca,
    y = census.2010.indv$cat_nivel_instrucao
  ),
  INDICES = census.2010.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-nv_estd_AP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_cor_raca,
    y = census.2010.indv$cat_qtd_trabalhos_julho_2010
  ),
  INDICES = census.2010.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-qtd_trab_AP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_cor_raca,
    y = census.2010.indv$cat_rendimento_aposentadoria_pensao
  ),
  INDICES = census.2010.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-rend_apos_pensao_AP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_cor_raca,
    y = census.2010.indv$cat_sexo
  ),
  INDICES = census.2010.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-sexo_AP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_cor_raca,
    y = census.2010.indv$cat_trabalhou
  ),
  INDICES = census.2010.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-trabalhou_AP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_sexo,
    y = census.2010.indv$cat_contribuicao_inss
  ),
  INDICES = census.2010.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-INSS_AP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_sexo,
    y = census.2010.indv$cat_curso_mais_elevado
  ),
  INDICES = census.2010.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-curso_elev_AP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_sexo,
    y = census.2010.indv$cat_nivel_instrucao
  ),
  INDICES = census.2010.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-nv_estd_AP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_sexo,
    y = census.2010.indv$cat_qtd_trabalhos_julho_2010
  ),
  INDICES = census.2010.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-qtd_trab_AP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_sexo,
    y = census.2010.indv$cat_rendimento_aposentadoria_pensao
  ),
  INDICES = census.2010.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-rend_apos_pensao_AP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_sexo,
    y = census.2010.indv$cat_trabalhou
  ),
  INDICES = census.2010.indv$area_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-trabalhou_AP.md"
)

# cross-tabulations per geographical division (regiao_planejamento) ----
view(stby(
  list(
    x = census.2010.indv$cat_cor_raca,
    y = census.2010.indv$cat_contribuicao_inss
  ),
  INDICES = census.2010.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-INSS_RP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_cor_raca,
    y = census.2010.indv$cat_curso_mais_elevado
  ),
  INDICES = census.2010.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-curso_elev_RP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_cor_raca,
    y = census.2010.indv$cat_nivel_instrucao
  ),
  INDICES = census.2010.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-nv_estd_RP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_cor_raca,
    y = census.2010.indv$cat_qtd_trabalhos_julho_2010
  ),
  INDICES = census.2010.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-qtd_trab_RP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_cor_raca,
    y = census.2010.indv$cat_rendimento_aposentadoria_pensao
  ),
  INDICES = census.2010.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-rend_apos_pensao_RP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_cor_raca,
    y = census.2010.indv$cat_sexo
  ),
  INDICES = census.2010.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-sexo_RP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_cor_raca,
    y = census.2010.indv$cat_trabalhou
  ),
  INDICES = census.2010.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-trabalhou_RP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_sexo,
    y = census.2010.indv$cat_contribuicao_inss
  ),
  INDICES = census.2010.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-INSS_RP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_sexo,
    y = census.2010.indv$cat_curso_mais_elevado
  ),
  INDICES = census.2010.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-curso_elev_RP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_sexo,
    y = census.2010.indv$cat_nivel_instrucao
  ),
  INDICES = census.2010.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-nv_estd_RP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_sexo,
    y = census.2010.indv$cat_qtd_trabalhos_julho_2010
  ),
  INDICES = census.2010.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-qtd_trab_RP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_sexo,
    y = census.2010.indv$cat_rendimento_aposentadoria_pensao
  ),
  INDICES = census.2010.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-rend_apos_pensao_RP.md"
)

view(stby(
  list(
    x = census.2010.indv$cat_sexo,
    y = census.2010.indv$cat_trabalhou
  ),
  INDICES = census.2010.indv$regiao_planejamento,
  FUN = ctable,
  useNA = "no"
),
file =
  "outputs/census/cross-tabs/indv/2010_raca-trabalhou_RP.md"
)

# census 2010 indv descriptive statistics ----
# descriptive statistics of numerical variables
view(summarytools::descr(
  census.2010.indv,
  # headings = FALSE,
  stats = desc.base,
  # transpose = TRUE
),
file =
  "outputs/census/desc-stats/indv/2010_desc_stats.md"
)

# descriptive statistics by geographical grouping
# stby(
#   data = census.2010.indv,
#   INDICES = census.2010.indv$area_planejamento,
#   FUN = descr,
#   stats = desc.base
# )
#
# stby(
#   data = census.2010.indv,
#   INDICES = census.2010.indv$regiao_planejamento,
#   FUN = descr,
#   stats = desc.base
# )

# OR:
view(census.2010.indv %>%
  select(-c(
    "rendimento_aposentadoria_pensao", "rendimento_bruto_principal_mensal",
    "rendimento_domiciliar", "rendimento_domiciliar_pc",
    "rendimento_domiciliar_pc_sm", "rendimento_domiciliar_sm",
    "rendimento_familiar_pc", "rendimento_familiar_pc_sm",
    "total_rendimentos_principal", "total_rendimentos_principal_sm"
  )) %>%
  group_by(area_planejamento) %>%
  descr(stats = desc.base),
file =
  "outputs/census/desc-stats/indv/2010_desc_stats_AP.md"
)

view(census.2010.indv %>%
  select(-c(
    "rendimento_aposentadoria_pensao", "rendimento_bruto_principal_mensal",
    "rendimento_domiciliar", "rendimento_domiciliar_pc",
    "rendimento_domiciliar_pc_sm", "rendimento_domiciliar_sm",
    "rendimento_familiar_pc", "rendimento_familiar_pc_sm",
    "total_rendimentos_principal", "total_rendimentos_principal_sm"
  )) %>%
  group_by(regiao_planejamento) %>%
  descr(stats = desc.base),
file =
  "outputs/census/desc-stats/indv/2010_desc_stats_RP.md"
)

# data frame summary
view(
  summarytools::dfSummary(census.2010.indv),
  file =
    "outputs/census/desc-stats/indv/2010_df_summary.md"
)

# census 2010 indv plots ----
# barplot (for categorical variables)
census.2010.indv %>%
  tidyr::drop_na(range_total_rendimentos_sm) %>%
  ggplot() +
  aes(x = range_total_rendimentos_sm) +
  geom_bar() +
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  ggtitle("census.2010.indv$range_total_rendimentos_sm") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2010_range_total_rendimentos_sm.png"
  )

census.2010.indv %>%
  tidyr::drop_na(range_idade) %>%
  ggplot() +
  aes(x = range_idade) +
  geom_bar() +
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  ggtitle("census.2010.indv$range_idade") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2010_range_idade.png"
  )

census.2010.indv %>%
  tidyr::drop_na(cat_contribuicao_inss) %>%
  ggplot() +
  aes(x = cat_contribuicao_inss) +
  geom_bar() +
  theme_bw() +
  ggtitle("census.2010.indv$cat_contribuicao_inss") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2010_cat_contribuicao_inss.png"
  )

census.2010.indv %>%
  tidyr::drop_na(cat_cor_raca) %>%
  ggplot() +
  aes(x = cat_cor_raca) +
  geom_bar() +
  theme_bw() +
  ggtitle("census.2010.indv$cat_cor_raca") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2010_cat_cor_raca.png"
  )

census.2010.indv %>%
  tidyr::drop_na(cat_qtd_trabalhos_julho_2010) %>%
  ggplot() +
  aes(x = cat_qtd_trabalhos_julho_2010) +
  geom_bar() +
  theme_bw() +
  ggtitle("census.2010.indv$cat_qtd_trabalhos_julho_2010") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2010_cat_qtd_trabalhos_julho_2010.png"
  )

census.2010.indv %>%
  tidyr::drop_na(cat_rendimento_aposentadoria_pensao) %>%
  ggplot() +
  aes(x = cat_rendimento_aposentadoria_pensao) +
  geom_bar() +
  theme_bw() +
  ggtitle("census.2010.indv$cat_rendimento_aposentadoria_pensao") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2010_cat_rendimento_aposentadoria_pensao.png"
  )

census.2010.indv %>%
  tidyr::drop_na(cat_rendimento_bruto_principal) %>%
  ggplot() +
  aes(x = cat_rendimento_bruto_principal) +
  geom_bar() +
  theme_bw() +
  ggtitle("census.2010.indv$cat_rendimento_bruto_principal") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2010_cat_rendimento_bruto_principal.png"
  )

census.2010.indv %>%
  tidyr::drop_na(cat_trabalhou) %>%
  ggplot() +
  aes(x = cat_trabalhou) +
  geom_bar() +
  theme_bw() +
  ggtitle("census.2010.indv$cat_trabalhou") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2010_cat_trabalhou.png"
  )

# increase the height!
census.2010.indv %>%
  tidyr::drop_na(cat_curso_mais_elevado) %>%
  ggplot() +
  aes(x = cat_curso_mais_elevado) +
  geom_bar() +
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  ggtitle("census.2010.indv$cat_curso_mais_elevado") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2010_cat_curso_mais_elevado.png"
  )

census.2010.indv %>%
  tidyr::drop_na(cat_nivel_instrucao) %>%
  ggplot() +
  aes(x = cat_nivel_instrucao) +
  geom_bar() +
  theme_bw() +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  ggtitle("census.2010.indv$cat_nivel_instrucao") +
  ggsave(
    type = "cairo",
    filename = "outputs/census/plots/indv/2010_cat_nivel_instrucao.png"
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

### descriptive analysis - census 2000 indv x census 2010 indv ----

# cat_sexo ----
# WITHOUT geo division
sink("outputs/census/comparisons/indv/cat_sexo.md", append = FALSE)

cat("cat_sexo")

purrr::map2(
  .x = list(census.2000.indv, census.2010.indv),
  .y = c(2000, 2010),
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

# WITH geo division
sink("outputs/census/comparisons/indv/cat_sexo-AP.md", append = FALSE)

cat("cat_sexo-AP")

purrr::map2(
  .x = list(census.2000.indv, census.2010.indv),
  .y = c(2000, 2010),
  function(i, j) {
    x <- freq.loop(
      df = i,
      gp_var = "area_planejamento",
      par = "cat_sexo"
    ) %>%
      summarytools::tb()
    # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for big dataframes

    names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = c("area_planejamento", "cat_sexo")) %>%
  # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for small dataframes
  knitr::kable()

sink()

sink("outputs/census/comparisons/indv/cat_sexo-RP.md", append = FALSE)

cat("cat_sexo-RP")

purrr::map2(
  .x = list(census.2000.indv, census.2010.indv),
  .y = c(2000, 2010),
  function(i, j) {
    x <- freq.loop(
      df = i,
      gp_var = "regiao_planejamento",
      par = "cat_sexo"
    ) %>%
      summarytools::tb()
    # %>% dplyr::filter(regiao_planejamento == "Baixada de Jacarepaguá") for big dataframes

    names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = c("regiao_planejamento", "cat_sexo")) %>%
  # %>% dplyr::filter(regiao_planejamento == "Baixada de Jacarepaguá") for small dataframes
  knitr::kable()

sink()

# cat_cor_raca ----
# WITHOUT geo division
sink("outputs/census/comparisons/indv/cat_cor_raca.md", append = FALSE)

cat("cat_cor_raca")

purrr::map2(
  .x = list(census.2000.indv, census.2010.indv),
  .y = c(2000, 2010),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "cat_cor_raca"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "cat_cor_raca") %>%
  knitr::kable()

sink()

# WITH geo division
# purrr::map2(
#   .x = list(census.2000.indv, census.2010.indv),
#   .y = c(2000, 2010),
#   function(i, j) {
#     x <- freq.loop(
#       df = i,
#       gp_var = "area_planejamento",
#       par = "cat_cor_raca"
#     ) %>%
#       summarytools::tb()
#     # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for big dataframes
#
#     names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))
#
#     return(x)
#   }
# ) %>%
#   purrr::reduce(dplyr::left_join, by = c("area_planejamento", "cat_cor_raca"))
#   # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for small dataframes
#
# purrr::map2(
#   .x = list(census.2000.indv, census.2010.indv),
#   .y = c(2000, 2010),
#   function(i, j) {
#     x <- freq.loop(
#       df = i,
#       gp_var = "regiao_planejamento",
#       par = "cat_cor_raca"
#     ) %>%
#       summarytools::tb()
#     # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for big dataframes
#
#     names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))
#
#     return(x)
#   }
# ) %>%
#   purrr::reduce(dplyr::left_join, by = c("regiao_planejamento", "cat_cor_raca"))
#   # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for small dataframes

# range_total_rendimentos_sm ----
# WITHOUT geo division
sink("outputs/census/comparisons/indv/range_total_rendimentos_sm.md", append = FALSE)

cat("range_total_rendimentos_sm")

purrr::map2(
  .x = list(census.2000.indv, census.2010.indv),
  .y = c(2000, 2010),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "range_total_rendimentos_sm",
      ord = "default"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "range_total_rendimentos_sm") %>%
  knitr::kable()

sink()

# WITH geo division
sink("outputs/census/comparisons/indv/range_total_rendimentos_sm-AP.md", append = FALSE)

cat("range_total_rendimentos_sm-AP")

purrr::map2(
  .x = list(census.2000.indv, census.2010.indv),
  .y = c(2000, 2010),
  function(i, j) {
    x <- freq.loop(
      df = i,
      gp_var = "area_planejamento",
      par = "range_total_rendimentos_sm",
      ord = "default"
    ) %>%
      summarytools::tb()
    # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for big dataframes

    names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = c("area_planejamento", "range_total_rendimentos_sm")) %>%
  # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for small dataframes
  knitr::kable()

sink()

sink("outputs/census/comparisons/indv/range_total_rendimentos_sm-RP.md", append = FALSE)

cat("range_total_rendimentos_sm-RP")

purrr::map2(
  .x = list(census.2000.indv, census.2010.indv),
  .y = c(2000, 2010),
  function(i, j) {
    x <- freq.loop(
      df = i,
      gp_var = "regiao_planejamento",
      par = "range_total_rendimentos_sm",
      ord = "default"
    ) %>%
      summarytools::tb()
    # %>% dplyr::filter(regiao_planejamento == "Baixada de Jacarepaguá") for big dataframes

    names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = c("regiao_planejamento", "range_total_rendimentos_sm")) %>%
  # %>% dplyr::filter(regiao_planejamento == "Baixada de Jacarepaguá") for small dataframes
  knitr::kable()

sink()

# range_idade ----
# WITHOUT geo division
sink("outputs/census/comparisons/indv/range_idade.md", append = FALSE)

cat("range_idade")

purrr::map2(
  .x = list(census.2000.indv, census.2010.indv),
  .y = c(2000, 2010),
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

# WITH geo division
sink("outputs/census/comparisons/indv/range_idade-AP.md", append = FALSE)

cat("range_idade-AP")

purrr::map2(
  .x = list(census.2000.indv, census.2010.indv),
  .y = c(2000, 2010),
  function(i, j) {
    x <- freq.loop(
      df = i,
      gp_var = "area_planejamento",
      par = "range_idade",
      ord = "default"
    ) %>%
      summarytools::tb()
    # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for big dataframes

    names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = c("area_planejamento", "range_idade")) %>%
  # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for small dataframes
  knitr::kable()

sink()

sink("outputs/census/comparisons/indv/range_idade-RP.md", append = FALSE)

cat("range_idade-RP")

purrr::map2(
  .x = list(census.2000.indv, census.2010.indv),
  .y = c(2000, 2010),
  function(i, j) {
    x <- freq.loop(
      df = i,
      gp_var = "regiao_planejamento",
      par = "range_idade",
      ord = "default"
    ) %>%
      summarytools::tb()
    # %>% dplyr::filter(regiao_planejamento == "Baixada de Jacarepaguá") for big dataframes

    names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = c("regiao_planejamento", "range_idade")) %>%
  # %>% dplyr::filter(regiao_planejamento == "Baixada de Jacarepaguá") for small dataframes
  knitr::kable()

sink()

# cat_trabalhou ----
# WITHOUT geo division
sink("outputs/census/comparisons/indv/cat_trabalhou.md", append = FALSE)

cat("cat_trabalhou")

purrr::map2(
  .x = list(census.2000.indv, census.2010.indv),
  .y = c(2000, 2010),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "cat_trabalhou"
    ) %>%
      summarytools::tb()

    names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "cat_trabalhou") %>%
  knitr::kable()

sink()

# WITH geo division
sink("outputs/census/comparisons/indv/cat_trabalhou-AP.md", append = FALSE)

cat("cat_trabalhou-AP")

purrr::map2(
  .x = list(census.2000.indv, census.2010.indv),
  .y = c(2000, 2010),
  function(i, j) {
    x <- freq.loop(
      df = i,
      gp_var = "area_planejamento",
      par = "cat_trabalhou"
    ) %>%
      summarytools::tb()
    # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for big dataframes

    names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = c("area_planejamento", "cat_trabalhou")) %>%
  # %>% dplyr::filter(area_planejamento == "Baixada de Jacarepaguá") for small dataframes
  knitr::kable()

sink()

sink("outputs/census/comparisons/indv/cat_trabalhou-RP.md", append = FALSE)

cat("cat_trabalhou-RP")

purrr::map2(
  .x = list(census.2000.indv, census.2010.indv),
  .y = c(2000, 2010),
  function(i, j) {
    x <- freq.loop(
      df = i,
      gp_var = "regiao_planejamento",
      par = "cat_trabalhou"
    ) %>%
      summarytools::tb()
    # %>% dplyr::filter(regiao_planejamento == "Baixada de Jacarepaguá") for big dataframes

    names(x)[3:4] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))

    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = c("regiao_planejamento", "cat_trabalhou")) %>%
  # %>% dplyr::filter(regiao_planejamento == "Baixada de Jacarepaguá") for small dataframes
  knitr::kable()

sink()

### median check for regressions ----

# checking medians per AP before 2001

# sink("outputs/rais/regressions/medians-AP.md", append = FALSE)
# 
# census.2000.indv %>%
#   select(total_rendimentos, area_planejamento) %>%
#   drop_na(area_planejamento) %>%
#   descr(
#     stats = c("mean", "med"),
#     style = "rmarkdown"
#   )
# 
# census.2000.indv %>%
#   select(total_rendimentos, area_planejamento) %>%
#   drop_na(area_planejamento) %>%
#   group_by(area_planejamento) %>%
#   descr(
#     stats = c("mean", "med"),
#     style = "rmarkdown"
#   )
# 
# sink()

# checking medians per RP before 2001

# sink("outputs/rais/regressions/medians-RP.md", append = FALSE)
# 
# census.2000.indv %>%
#   select(total_rendimentos, regiao_planejamento) %>%
#   drop_na(regiao_planejamento) %>%
#   descr(
#     stats = c("mean", "med"),
#     style = "rmarkdown"
#   )
# 
# census.2000.indv %>%
#   select(total_rendimentos, regiao_planejamento) %>%
#   drop_na(regiao_planejamento) %>%
#   group_by(regiao_planejamento) %>%
#   descr(
#     stats = c("mean", "med"),
#     style = "rmarkdown"
#   )
# 
# sink()