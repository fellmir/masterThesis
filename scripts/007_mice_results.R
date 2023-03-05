### beginning ----

source("scripts/001_libraries.R")

fs::dir_create("outputs/rais/mice/estab/final")
fs::dir_create("outputs/rais/mice/vinc/final")

### function ----
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
          report.nas = T,
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
        report.nas = T,
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
          report.nas = T,
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
        report.nas = T,
        order = ord,
        cumul = FALSE,
        style = "rmarkdown",
        # headings = FALSE
      )
      
      return(x)
    }
  }
}

### estab ----

## OG dbs ----

rais.estab.2000 <- readRDS("db/rais/estab/og/rais_estab_2000.rds")
rais.estab.2001 <- readRDS("db/rais/estab/og/rais_estab_2001.rds")
rais.estab.2002 <- readRDS("db/rais/estab/og/rais_estab_2002.rds")
rais.estab.2003 <- readRDS("db/rais/estab/og/rais_estab_2003.rds")
rais.estab.2004 <- readRDS("db/rais/estab/og/rais_estab_2004.rds")
rais.estab.2005 <- readRDS("db/rais/estab/og/rais_estab_2005.rds")
rais.estab.2006 <- readRDS("db/rais/estab/og/rais_estab_2006.rds")
rais.estab.2007 <- readRDS("db/rais/estab/og/rais_estab_2007.rds")
rais.estab.2008 <- readRDS("db/rais/estab/og/rais_estab_2008.rds")
rais.estab.2009 <- readRDS("db/rais/estab/og/rais_estab_2009.rds")
rais.estab.2010 <- readRDS("db/rais/estab/og/rais_estab_2010.rds")
rais.estab.2011 <- readRDS("db/rais/estab/og/rais_estab_2011.rds")
rais.estab.2012 <- readRDS("db/rais/estab/og/rais_estab_2012.rds")
rais.estab.2013 <- readRDS("db/rais/estab/og/rais_estab_2013.rds")
rais.estab.2014 <- readRDS("db/rais/estab/og/rais_estab_2014.rds")
rais.estab.2015 <- readRDS("db/rais/estab/og/rais_estab_2015.rds")

# NAs ----

# sink("outputs/rais/mice/estab/NAs_estab.md", append = T)
# 
# cat("\n\n2000\n")
# 
# colMeans(is.na(rais.estab.2000[,17:18])) %>% 
#   knitr::kable()
# 
# cat("\n\n2001\n")
# 
# colMeans(is.na(rais.estab.2001[,17:18])) %>% 
#   knitr::kable()
# 
# cat("\n\n2002\n")
# 
# colMeans(is.na(rais.estab.2002[,17:18])) %>% 
#   knitr::kable()
# 
# cat("\n\n2003\n")
# 
# colMeans(is.na(rais.estab.2003[,17:18])) %>% 
#   knitr::kable()
# 
# cat("\n\n2004\n")
# 
# colMeans(is.na(rais.estab.2004[,17:18])) %>% 
#   knitr::kable()
# 
# cat("\n\n2005\n")
# 
# colMeans(is.na(rais.estab.2005[,17:18])) %>% 
#   knitr::kable()
# 
# cat("\n\n2006\n")
# 
# colMeans(is.na(rais.estab.2006[,17:18])) %>% 
#   knitr::kable()
# 
# cat("\n\n2007\n")
# 
# colMeans(is.na(rais.estab.2007[,17:18])) %>% 
#   knitr::kable()
# 
# cat("\n\n2008\n")
# 
# colMeans(is.na(rais.estab.2008[,17:18])) %>% 
#   knitr::kable()
# 
# cat("\n\n2009\n")
# 
# colMeans(is.na(rais.estab.2009[,17:18])) %>% 
#   knitr::kable()
# 
# cat("\n\n2010\n")
# 
# colMeans(is.na(rais.estab.2010[,17:18])) %>% 
#   knitr::kable()
# 
# cat("\n\n2011\n")
# 
# colMeans(is.na(rais.estab.2011[,17:18])) %>% 
#   knitr::kable()
# 
# cat("\n\n2012\n")
# 
# colMeans(is.na(rais.estab.2012[,17:18])) %>% 
#   knitr::kable()
# 
# cat("\n\n2013\n")
# 
# colMeans(is.na(rais.estab.2013[,17:18])) %>% 
#   knitr::kable()
# 
# cat("\n\n2014\n")
# 
# colMeans(is.na(rais.estab.2014[,17:18])) %>% 
#   knitr::kable()
# 
# cat("\n\n2015\n")
# 
# colMeans(is.na(rais.estab.2015[,17:18])) %>% 
#   knitr::kable()
# 
# sink()

# APs & RPs ----

sink("outputs/rais/mice/estab/final/AP_estab_OG.md", append = F)

cat("\n\nAP\n")

purrr::map2(
  .x = list(
    rais.estab.2000, rais.estab.2001, rais.estab.2002, rais.estab.2003, 
    rais.estab.2004, rais.estab.2005, rais.estab.2006, rais.estab.2007,
    rais.estab.2008, rais.estab.2009, rais.estab.2010, rais.estab.2011, 
    rais.estab.2012, rais.estab.2013, rais.estab.2014, rais.estab.2015
  ),
  .y = c(2000:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "AP",
      ord = "default"
    ) %>%
      summarytools::tb()
    
    names(x)[2:4] <- c(glue::glue("freq_{j}"), 
                       glue::glue("pct_{j}"), 
                       glue::glue("pct_tot_{j}"))
    
    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "AP") %>%
  knitr::kable()

sink()

sink("outputs/rais/mice/estab/final/RP_estab_OG.md", append = F)

cat("\n\nRP\n")

purrr::map2(
  .x = list(
    rais.estab.2000, rais.estab.2001, rais.estab.2002, rais.estab.2003, 
    rais.estab.2004, rais.estab.2005, rais.estab.2006, rais.estab.2007,
    rais.estab.2008, rais.estab.2009, rais.estab.2010, rais.estab.2011, 
    rais.estab.2012, rais.estab.2013, rais.estab.2014, rais.estab.2015
  ),
  .y = c(2000:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "RP",
      ord = "default"
    ) %>%
      summarytools::tb()
    
    names(x)[2:4] <- c(glue::glue("freq_{j}"), 
                       glue::glue("pct_{j}"), 
                       glue::glue("pct_tot_{j}"))
    
    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "RP") %>%
  knitr::kable()

sink()

## mod dbs ----

rais.estab.2000 <- readRDS("db/rais/estab/mod/rais_estab_2000_mod_v2.rds")
rais.estab.2001 <- readRDS("db/rais/estab/mod/rais_estab_2001_mod_v2.rds")
rais.estab.2002 <- readRDS("db/rais/estab/mod/rais_estab_2002_mod_v2.rds")
rais.estab.2003 <- readRDS("db/rais/estab/mod/rais_estab_2003_mod_v2.rds")
rais.estab.2004 <- readRDS("db/rais/estab/mod/rais_estab_2004_mod_v2.rds")
rais.estab.2005 <- readRDS("db/rais/estab/mod/rais_estab_2005_mod_v2.rds")
rais.estab.2006 <- readRDS("db/rais/estab/mod/rais_estab_2006_mod_v2.rds")
rais.estab.2007 <- readRDS("db/rais/estab/mod/rais_estab_2007_mod_v2.rds")
rais.estab.2008 <- readRDS("db/rais/estab/mod/rais_estab_2008_mod_v2.rds")
rais.estab.2009 <- readRDS("db/rais/estab/mod/rais_estab_2009_mod_v2.rds")
rais.estab.2010 <- readRDS("db/rais/estab/mod/rais_estab_2010_mod_v2.rds")
rais.estab.2011 <- readRDS("db/rais/estab/mod/rais_estab_2011_mod_v2.rds")
rais.estab.2012 <- readRDS("db/rais/estab/mod/rais_estab_2012_mod_v2.rds")
rais.estab.2013 <- readRDS("db/rais/estab/mod/rais_estab_2013_mod_v2.rds")
rais.estab.2014 <- readRDS("db/rais/estab/mod/rais_estab_2014_mod_v2.rds")
rais.estab.2015 <- readRDS("db/rais/estab/mod/rais_estab_2015_mod_v2.rds")

# NAs ----

# sink("outputs/rais/mice/estab/NAs_estab.md", append = T)
# 
# cat("\n\n2000\n")
# 
# colMeans(is.na(rais.estab.2000[,10:11])) %>% 
#   knitr::kable()
# 
# cat("\n\n2001\n")
# 
# colMeans(is.na(rais.estab.2001[,10:11])) %>% 
#   knitr::kable()
# 
# cat("\n\n2002\n")
# 
# colMeans(is.na(rais.estab.2002[,10:11])) %>% 
#   knitr::kable()
# 
# cat("\n\n2003\n")
# 
# colMeans(is.na(rais.estab.2003[,10:11])) %>% 
#   knitr::kable()
# 
# cat("\n\n2004\n")
# 
# colMeans(is.na(rais.estab.2004[,10:11])) %>% 
#   knitr::kable()
# 
# cat("\n\n2005\n")
# 
# colMeans(is.na(rais.estab.2005[,10:11])) %>% 
#   knitr::kable()
# 
# cat("\n\n2006\n")
# 
# colMeans(is.na(rais.estab.2006[,10:11])) %>% 
#   knitr::kable()
# 
# cat("\n\n2007\n")
# 
# colMeans(is.na(rais.estab.2007[,10:11])) %>% 
#   knitr::kable()
# 
# cat("\n\n2008\n")
# 
# colMeans(is.na(rais.estab.2008[,10:11])) %>% 
#   knitr::kable()
# 
# cat("\n\n2009\n")
# 
# colMeans(is.na(rais.estab.2009[,10:11])) %>% 
#   knitr::kable()
# 
# cat("\n\n2010\n")
# 
# colMeans(is.na(rais.estab.2010[,10:11])) %>% 
#   knitr::kable()
# 
# cat("\n\n2011\n")
# 
# colMeans(is.na(rais.estab.2011[,10:11])) %>% 
#   knitr::kable()
# 
# cat("\n\n2012\n")
# 
# colMeans(is.na(rais.estab.2012[,10:11])) %>% 
#   knitr::kable()
# 
# cat("\n\n2013\n")
# 
# colMeans(is.na(rais.estab.2013[,10:11])) %>% 
#   knitr::kable()
# 
# cat("\n\n2014\n")
# 
# colMeans(is.na(rais.estab.2014[,10:11])) %>% 
#   knitr::kable()
# 
# cat("\n\n2015\n")
# 
# colMeans(is.na(rais.estab.2015[,10:11])) %>% 
#   knitr::kable()
# 
# sink()

# APs & RPs ----

sink("outputs/rais/mice/estab/final/AP_estab_mod.md", append = F)

cat("\n\nAP\n")

purrr::map2(
  .x = list(
    rais.estab.2000, rais.estab.2001, rais.estab.2002, rais.estab.2003, 
    rais.estab.2004, rais.estab.2005, rais.estab.2006, rais.estab.2007,
    rais.estab.2008, rais.estab.2009, rais.estab.2010, rais.estab.2011, 
    rais.estab.2012, rais.estab.2013, rais.estab.2014, rais.estab.2015
  ),
  .y = c(2000:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "AP",
      ord = "default"
    ) %>%
      summarytools::tb()
    
    names(x)[2:4] <- c(glue::glue("freq_{j}"), 
                       glue::glue("pct_{j}"), 
                       glue::glue("pct_tot_{j}"))
    
    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "AP") %>%
  knitr::kable()

sink()

sink("outputs/rais/mice/estab/final/RP_estab_mod.md", append = F)

cat("\n\nRP\n")

purrr::map2(
  .x = list(
    rais.estab.2000, rais.estab.2001, rais.estab.2002, rais.estab.2003, 
    rais.estab.2004, rais.estab.2005, rais.estab.2006, rais.estab.2007,
    rais.estab.2008, rais.estab.2009, rais.estab.2010, rais.estab.2011, 
    rais.estab.2012, rais.estab.2013, rais.estab.2014, rais.estab.2015
  ),
  .y = c(2000:2015),
  function(i, j) {
    x <- freq.loop(
      df = i,
      par = "RP",
      ord = "default"
    ) %>%
      summarytools::tb()
    
    names(x)[2:4] <- c(glue::glue("freq_{j}"), 
                       glue::glue("pct_{j}"), 
                       glue::glue("pct_tot_{j}"))
    
    return(x)
  }
) %>%
  purrr::reduce(dplyr::left_join, by = "RP") %>%
  knitr::kable()

sink()

### vinc ----

## OG ----

# NAs ----

# sink("outputs/rais/mice/vinc/NAs_vinc.md", append = T)
# 
# rais.vinc.2000 <- readRDS("db/rais/vinc/og/rais_vinc_2000.rds")
# 
# cat("\n\n2000\n")
# 
# colMeans(is.na(rais.vinc.2000[,c(44, 45, 51)])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2000)
# 
# rais.vinc.2001 <- readRDS("db/rais/vinc/og/rais_vinc_2001.rds")
# 
# cat("\n\n2001\n")
# 
# colMeans(is.na(rais.vinc.2001[,c(44, 45, 51)])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2001)
# 
# rais.vinc.2002 <- readRDS("db/rais/vinc/og/rais_vinc_2002.rds")
# 
# cat("\n\n2002\n")
# 
# colMeans(is.na(rais.vinc.2002[,c(44, 45, 51)])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2002)
# 
# rais.vinc.2003 <- readRDS("db/rais/vinc/og/rais_vinc_2003.rds")
# 
# cat("\n\n2003\n")
# 
# colMeans(is.na(rais.vinc.2003[,c(44, 45, 51)])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2003)
# 
# rais.vinc.2004 <- readRDS("db/rais/vinc/og/rais_vinc_2004.rds")
# 
# cat("\n\n2004\n")
# 
# colMeans(is.na(rais.vinc.2004[,c(44, 45, 51)])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2004)
# 
# rais.vinc.2005 <- readRDS("db/rais/vinc/og/rais_vinc_2005.rds")
# 
# cat("\n\n2005\n")
# 
# colMeans(is.na(rais.vinc.2005[,c(44, 45, 51)])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2005)
# 
# rais.vinc.2006 <- readRDS("db/rais/vinc/og/rais_vinc_2006.rds")
# 
# cat("\n\n2006\n")
# 
# colMeans(is.na(rais.vinc.2006[,c(44, 45, 51)])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2006)
# 
# rais.vinc.2007 <- readRDS("db/rais/vinc/og/rais_vinc_2007.rds")
# 
# cat("\n\n2007\n")
# 
# colMeans(is.na(rais.vinc.2007[,c(44, 45, 51)])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2007)
# 
# rais.vinc.2008 <- readRDS("db/rais/vinc/og/rais_vinc_2008.rds")
# 
# cat("\n\n2008\n")
# 
# colMeans(is.na(rais.vinc.2008[,c(44, 45, 51)])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2008)
# 
# rais.vinc.2009 <- readRDS("db/rais/vinc/og/rais_vinc_2009.rds")
# 
# cat("\n\n2009\n")
# 
# colMeans(is.na(rais.vinc.2009[,c(44, 45, 51)])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2009)
# 
# rais.vinc.2010 <- readRDS("db/rais/vinc/og/rais_vinc_2010.rds")
# 
# cat("\n\n2010\n")
# 
# colMeans(is.na(rais.vinc.2010[,c(44, 45, 51)])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2010)
# 
# rais.vinc.2011 <- readRDS("db/rais/vinc/og/rais_vinc_2011.rds")
# 
# cat("\n\n2011\n")
# 
# colMeans(is.na(rais.vinc.2011[,c(44, 45, 51)])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2011)
# 
# rais.vinc.2012 <- readRDS("db/rais/vinc/og/rais_vinc_2012.rds")
# 
# cat("\n\n2012\n")
# 
# colMeans(is.na(rais.vinc.2012[,c(44, 45, 51)])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2012)
# 
# rais.vinc.2013 <- readRDS("db/rais/vinc/og/rais_vinc_2013.rds")
# 
# cat("\n\n2013\n")
# 
# colMeans(is.na(rais.vinc.2013[,c(44, 45, 51)])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2013)
# 
# rais.vinc.2014 <- readRDS("db/rais/vinc/og/rais_vinc_2014.rds")
# 
# cat("\n\n2014\n")
# 
# colMeans(is.na(rais.vinc.2014[,c(44, 45, 51)])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2014)
# 
# rais.vinc.2015 <- readRDS("db/rais/vinc/og/rais_vinc_2015.rds")
# 
# cat("\n\n2015\n")
# 
# colMeans(is.na(rais.vinc.2015[,c(44, 45, 51)])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2015)
# 
# sink()
# 
# gc()

# APs, RPs & race_id ----
rais.vinc.arrow <- fs::dir_ls("db/rais/vinc/og", regexp = "rais(.*)\\.parquet") %>%
  arrow::open_dataset()

sink("outputs/rais/mice/vinc/AP_vinc.md", append = T)

cat("AP")

rais.vinc.arrow %>%
  dplyr::select(AP, ano) %>%
  # dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2000:2015),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "AP",
        ord = "default"
      ) %>%
        summarytools::tb()
      
      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))
      
      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "AP") %>%
  knitr::kable()

sink()

gc()

sink("outputs/rais/mice/vinc/RP_vinc.md", append = T)

cat("RP")

rais.vinc.arrow %>%
  dplyr::select(RP, ano) %>%
  # dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2000:2015),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "RP",
        ord = "default"
      ) %>%
        summarytools::tb()
      
      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))
      
      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "RP") %>%
  knitr::kable()

sink()

gc()

ano.complete <- c(2006:2015)

sink("outputs/rais/mice/vinc/race_id_vinc.md", append = T)

cat("race_id")

rais.vinc.arrow %>%
  dplyr::select(race_id, ano) %>%
  dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2006:2015),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "race_id",
        ord = "default"
      ) %>%
        summarytools::tb()
      
      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))
      
      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "race_id") %>%
  knitr::kable()

sink()

rm(rais.vinc.arrow)

gc()

## mod ----

# NAs ----

# sink("outputs/rais/mice/vinc/NAs_vinc.md", append = T)
# 
# rais.vinc.2000 <- readRDS("db/rais/vinc/mod/rais_vinc_2000_mod_v2.rds")
# 
# cat("\n\n2000\n")
# 
# colMeans(is.na(rais.vinc.2000[,28:30])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2000)
# 
# rais.vinc.2001 <- readRDS("db/rais/vinc/mod/rais_vinc_2001_mod_v2.rds")
# 
# cat("\n\n2001\n")
# 
# colMeans(is.na(rais.vinc.2001[,28:30])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2001)
# 
# rais.vinc.2002 <- readRDS("db/rais/vinc/mod/rais_vinc_2002_mod_v2.rds")
# 
# cat("\n\n2002\n")
# 
# colMeans(is.na(rais.vinc.2002[,28:30])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2002)
# 
# rais.vinc.2003 <- readRDS("db/rais/vinc/mod/rais_vinc_2003_mod_v2.rds")
# 
# cat("\n\n2003\n")
# 
# colMeans(is.na(rais.vinc.2003[,28:30])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2003)
# 
# rais.vinc.2004 <- readRDS("db/rais/vinc/mod/rais_vinc_2004_mod_v2.rds")
# 
# cat("\n\n2004\n")
# 
# colMeans(is.na(rais.vinc.2004[,28:30])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2004)
# 
# rais.vinc.2005 <- readRDS("db/rais/vinc/mod/rais_vinc_2005_mod_v2.rds")
# 
# cat("\n\n2005\n")
# 
# colMeans(is.na(rais.vinc.2005[,28:30])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2005)
# 
# rais.vinc.2006 <- readRDS("db/rais/vinc/mod/rais_vinc_2006_mod_v2.rds")
# 
# cat("\n\n2006\n")
# 
# colMeans(is.na(rais.vinc.2006[,28:30])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2006)
# 
# rais.vinc.2007 <- readRDS("db/rais/vinc/mod/rais_vinc_2007_mod_v3.rds")
# 
# cat("\n\n2007 v3\n")
# 
# colMeans(is.na(rais.vinc.2007[,28:30])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2007)
# 
# rais.vinc.2008 <- readRDS("db/rais/vinc/mod/rais_vinc_2008_mod_v2.rds")
# 
# cat("\n\n2008\n")
# 
# colMeans(is.na(rais.vinc.2008[,28:30])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2008)
# 
# rais.vinc.2009 <- readRDS("db/rais/vinc/mod/rais_vinc_2009_mod_v2.rds")
# 
# cat("\n\n2009\n")
# 
# colMeans(is.na(rais.vinc.2009[,28:30])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2009)
# 
# rais.vinc.2010 <- readRDS("db/rais/vinc/mod/rais_vinc_2010_mod_v3.rds")
# 
# cat("\n\n2010 v3\n")
# 
# colMeans(is.na(rais.vinc.2010[,28:30])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2010)
# 
# rais.vinc.2011 <- readRDS("db/rais/vinc/mod/rais_vinc_2011_mod_v2.rds")
# 
# cat("\n\n2011\n")
# 
# colMeans(is.na(rais.vinc.2011[,28:30])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2011)
# 
# rais.vinc.2012 <- readRDS("db/rais/vinc/mod/rais_vinc_2012_mod_v2.rds")
# 
# cat("\n\n2012\n")
# 
# colMeans(is.na(rais.vinc.2012[,28:30])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2012)
# 
# rais.vinc.2013 <- readRDS("db/rais/vinc/mod/rais_vinc_2013_mod_v2.rds")
# 
# cat("\n\n2013\n")
# 
# colMeans(is.na(rais.vinc.2013[,28:30])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2013)
# 
# rais.vinc.2014 <- readRDS("db/rais/vinc/mod/rais_vinc_2014_mod_v2.rds")
# 
# cat("\n\n2014\n")
# 
# colMeans(is.na(rais.vinc.2014[,28:30])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2014)
# 
# rais.vinc.2015 <- readRDS("db/rais/vinc/mod/rais_vinc_2015_mod_v2.rds")
# 
# cat("\n\n2015\n")
# 
# colMeans(is.na(rais.vinc.2015[,28:30])) %>% 
#   knitr::kable()
# 
# rm(rais.vinc.2015)
# 
# sink()
# 
# gc()

# APs, RPs & race_id ----
rais.vinc.arrow <- fs::dir_ls("db/rais/vinc/", regexp = "rais(.*)\\.parquet") %>%
  arrow::open_dataset()

sink("outputs/rais/mice/vinc/AP_vinc.md", append = T)

cat("\n\nAP\n")

rais.vinc.arrow %>%
  dplyr::select(AP, ano) %>%
  # dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2000:2015),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "AP",
        ord = "default"
      ) %>%
        summarytools::tb()
      
      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))
      
      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "AP") %>%
  knitr::kable()

sink()

gc()

sink("outputs/rais/mice/vinc/RP_vinc.md", append = T)

cat("\n\nRP\n")

rais.vinc.arrow %>%
  dplyr::select(RP, ano) %>%
  # dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2000:2015),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "RP",
        ord = "default"
      ) %>%
        summarytools::tb()
      
      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))
      
      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "RP") %>%
  knitr::kable()

sink()

gc()

sink("outputs/rais/mice/vinc/race_id_vinc.md", append = T)

cat("\n\nrace_id\n")

rais.vinc.arrow %>%
  dplyr::select(race_id, ano) %>%
  # dplyr::filter(ano %in% ano.complete) %>%
  dplyr::collect() %>%
  dplyr::group_split(ano) %>%
  purrr::map2(
    .y = c(2000:2015),
    function(i, j) {
      x <- freq.loop(
        df = i,
        par = "race_id",
        ord = "default"
      ) %>%
        summarytools::tb()
      
      names(x)[2:3] <- c(glue::glue("freq_{j}"), glue::glue("pct_{j}"))
      
      return(x)
    }
  ) %>%
  purrr::reduce(dplyr::left_join, by = "race_id") %>%
  knitr::kable()

sink()

rm(rais.vinc.arrow)

gc()

