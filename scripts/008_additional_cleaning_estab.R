# beginning ----
source("scripts/001_libraries.R")

### geo for joins ----
bairros.rj <- readxl::read_xlsx("inputs/refs/20220409_geo.xlsx", sheet = 5) %>%
  dplyr::select(bairros_rj = bairros_rj, nome_bairro) %>%
  dplyr::mutate(bairros_rj = as.character(bairros_rj))

ap.rais <- readxl::read_xlsx("inputs/refs/20220427_rp_ap_v2.xlsx", sheet = 4) %>%
  dplyr::select(AP, area_planejamento)

rp.rais <- readxl::read_xlsx("inputs/refs/20220427_rp_ap_v2.xlsx", sheet = 5) %>%
  dplyr::select(RP, regiao_planejamento)


### RAIS estab ----
rais.estab.cat.01 <- readxl::read_xlsx("inputs/refs/test.xlsx",
                                       sheet = 8
) %>%
  dplyr::select(tamanho, cat_tamanho) %>%
  dplyr::mutate(tamanho = as.character(tamanho)) %>%
  tidyr::drop_na()

rais.estab.cat.03 <- readxl::read_xlsx("inputs/refs/test.xlsx",
                                       sheet = 8
) %>%
  dplyr::select(natureza_juridica, cat_natureza_juridica) %>%
  dplyr::mutate(natureza_juridica = as.character(natureza_juridica)) %>%
  tidyr::drop_na()

rais.estab.cat.05 <- readxl::read_xlsx("inputs/refs/test.xlsx",
                                       sheet = 8
) %>%
  dplyr::select(cnae_1, cat_cnae_1) %>%
  dplyr::mutate(cnae_1 = as.character(cnae_1)) %>%
  tidyr::drop_na()

rais.estab.cat.10 <- readxl::read_xlsx("inputs/refs/test2.xlsx",
                                       sheet = 1
) %>%
  dplyr::select(tamanho, cat_tamanho) %>%
  dplyr::mutate(tamanho = as.character(tamanho)) %>%
  tidyr::drop_na()


## new joins ----

# 2000 - 2005 ----

# 2000
rais.estab.2000 <- readRDS("db/rais/estab/mod/rais_estab_2000_mod_v2.rds") %>% 
  dplyr::mutate(post = case_when(
    ano %in% c(2000, 2001, 2002) ~ 0,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(1, 2) ~ 0,
    AP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(1.1, 2.1, 2.2, 3.3, 3.7, 4.2) ~ 0,
    RP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.estab.cat.05, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.estab.cat.10, "tamanho") %>% 
  dplyr::mutate(
    range_qtd_vinc_ativos = cut(
      quantidade_vinculos_ativos,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_clt = cut(
      quantidade_vinculos_clt,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 30000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_estat = cut(
      quantidade_vinculos_estatutarios,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::left_join(rais.estab.cat.03, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/estab/rais_estab_2000.rds"))

rm(rais.estab.2000)


# 2001
rais.estab.2001 <- readRDS("db/rais/estab/mod/rais_estab_2001_mod_v2.rds") %>% 
  dplyr::mutate(post = case_when(
    ano %in% c(2000, 2001, 2002) ~ 0,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(1, 2) ~ 0,
    AP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(1.1, 2.1, 2.2, 3.3, 3.7, 4.2) ~ 0,
    RP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.estab.cat.05, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.estab.cat.10, "tamanho") %>% 
  dplyr::mutate(
    range_qtd_vinc_ativos = cut(
      quantidade_vinculos_ativos,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_clt = cut(
      quantidade_vinculos_clt,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 30000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_estat = cut(
      quantidade_vinculos_estatutarios,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::left_join(rais.estab.cat.03, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/estab/rais_estab_2001.rds"))

rm(rais.estab.2001)


# 2002
rais.estab.2002 <- readRDS("db/rais/estab/mod/rais_estab_2002_mod_v2.rds") %>% 
  dplyr::mutate(post = case_when(
    ano %in% c(2000, 2001, 2002) ~ 0,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(1, 2) ~ 0,
    AP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(1.1, 2.1, 2.2, 3.3, 3.7, 4.2) ~ 0,
    RP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.estab.cat.05, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.estab.cat.01, "tamanho") %>% 
  dplyr::mutate(
    range_qtd_vinc_ativos = cut(
      quantidade_vinculos_ativos,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_clt = cut(
      quantidade_vinculos_clt,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 30000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_estat = cut(
      quantidade_vinculos_estatutarios,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::left_join(rais.estab.cat.03, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/estab/rais_estab_2002.rds"))

rm(rais.estab.2002)


# 2003
rais.estab.2003 <- readRDS("db/rais/estab/mod/rais_estab_2003_mod_v2.rds") %>% 
  dplyr::mutate(post = case_when(
    ano %in% c(2000, 2001, 2002) ~ 0,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(1, 2) ~ 0,
    AP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(1.1, 2.1, 2.2, 3.3, 3.7, 4.2) ~ 0,
    RP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.estab.cat.05, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.estab.cat.01, "tamanho") %>% 
  dplyr::mutate(
    range_qtd_vinc_ativos = cut(
      quantidade_vinculos_ativos,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_clt = cut(
      quantidade_vinculos_clt,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 30000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_estat = cut(
      quantidade_vinculos_estatutarios,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::left_join(rais.estab.cat.03, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/estab/rais_estab_2003.rds"))

rm(rais.estab.2003)


# 2004
rais.estab.2004 <- readRDS("db/rais/estab/mod/rais_estab_2004_mod_v2.rds") %>% 
  dplyr::mutate(post = case_when(
    ano %in% c(2000, 2001, 2002) ~ 0,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(1, 2) ~ 0,
    AP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(1.1, 2.1, 2.2, 3.3, 3.7, 4.2) ~ 0,
    RP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.estab.cat.05, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.estab.cat.01, "tamanho") %>% 
  dplyr::mutate(
    range_qtd_vinc_ativos = cut(
      quantidade_vinculos_ativos,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_clt = cut(
      quantidade_vinculos_clt,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 30000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_estat = cut(
      quantidade_vinculos_estatutarios,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::left_join(rais.estab.cat.03, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/estab/rais_estab_2004.rds"))

rm(rais.estab.2004)


# 2005
rais.estab.2005 <- readRDS("db/rais/estab/mod/rais_estab_2005_mod_v2.rds") %>% 
  dplyr::mutate(post = case_when(
    ano %in% c(2000, 2001, 2002) ~ 0,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(1, 2) ~ 0,
    AP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(1.1, 2.1, 2.2, 3.3, 3.7, 4.2) ~ 0,
    RP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.estab.cat.05, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.estab.cat.01, "tamanho") %>% 
  dplyr::mutate(
    range_qtd_vinc_ativos = cut(
      quantidade_vinculos_ativos,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_clt = cut(
      quantidade_vinculos_clt,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 30000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_estat = cut(
      quantidade_vinculos_estatutarios,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::left_join(rais.estab.cat.03, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/estab/rais_estab_2005.rds"))

rm(rais.estab.2005)

# 2006 - 2010 ----

# 2006
rais.estab.2006 <- readRDS("db/rais/estab/mod/rais_estab_2006_mod_v2.rds") %>% 
  dplyr::mutate(post = case_when(
    ano %in% c(2000, 2001, 2002) ~ 0,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(1, 2) ~ 0,
    AP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(1.1, 2.1, 2.2, 3.3, 3.7, 4.2) ~ 0,
    RP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.estab.cat.05, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.estab.cat.01, "tamanho") %>% 
  dplyr::mutate(
    range_qtd_vinc_ativos = cut(
      quantidade_vinculos_ativos,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_clt = cut(
      quantidade_vinculos_clt,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 30000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_estat = cut(
      quantidade_vinculos_estatutarios,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::left_join(rais.estab.cat.03, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/estab/rais_estab_2006.rds"))

rm(rais.estab.2006)


# 2007
rais.estab.2007 <- readRDS("db/rais/estab/mod/rais_estab_2007_mod_v2.rds") %>% 
  dplyr::mutate(post = case_when(
    ano %in% c(2000, 2001, 2002) ~ 0,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(1, 2) ~ 0,
    AP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(1.1, 2.1, 2.2, 3.3, 3.7, 4.2) ~ 0,
    RP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.estab.cat.05, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.estab.cat.01, "tamanho") %>% 
  dplyr::mutate(
    range_qtd_vinc_ativos = cut(
      quantidade_vinculos_ativos,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_clt = cut(
      quantidade_vinculos_clt,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 30000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_estat = cut(
      quantidade_vinculos_estatutarios,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::left_join(rais.estab.cat.03, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/estab/rais_estab_2007.rds"))

rm(rais.estab.2007)


# 2008
rais.estab.2008 <- readRDS("db/rais/estab/mod/rais_estab_2008_mod_v2.rds") %>% 
  dplyr::mutate(post = case_when(
    ano %in% c(2000, 2001, 2002) ~ 0,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(1, 2) ~ 0,
    AP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(1.1, 2.1, 2.2, 3.3, 3.7, 4.2) ~ 0,
    RP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.estab.cat.05, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.estab.cat.01, "tamanho") %>% 
  dplyr::mutate(
    range_qtd_vinc_ativos = cut(
      quantidade_vinculos_ativos,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_clt = cut(
      quantidade_vinculos_clt,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 30000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_estat = cut(
      quantidade_vinculos_estatutarios,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::left_join(rais.estab.cat.03, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/estab/rais_estab_2008.rds"))

rm(rais.estab.2008)


# 2009
rais.estab.2009 <- readRDS("db/rais/estab/mod/rais_estab_2009_mod_v2.rds") %>% 
  dplyr::mutate(post = case_when(
    ano %in% c(2000, 2001, 2002) ~ 0,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(1, 2) ~ 0,
    AP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(1.1, 2.1, 2.2, 3.3, 3.7, 4.2) ~ 0,
    RP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.estab.cat.05, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.estab.cat.01, "tamanho") %>% 
  dplyr::mutate(
    range_qtd_vinc_ativos = cut(
      quantidade_vinculos_ativos,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_clt = cut(
      quantidade_vinculos_clt,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 30000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_estat = cut(
      quantidade_vinculos_estatutarios,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::left_join(rais.estab.cat.03, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/estab/rais_estab_2009.rds"))

rm(rais.estab.2009)


# 2010
rais.estab.2010 <- readRDS("db/rais/estab/mod/rais_estab_2010_mod_v2.rds") %>% 
  dplyr::mutate(post = case_when(
    ano %in% c(2000, 2001, 2002) ~ 0,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(1, 2) ~ 0,
    AP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(1.1, 2.1, 2.2, 3.3, 3.7, 4.2) ~ 0,
    RP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.estab.cat.05, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.estab.cat.01, "tamanho") %>% 
  dplyr::mutate(
    range_qtd_vinc_ativos = cut(
      quantidade_vinculos_ativos,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_clt = cut(
      quantidade_vinculos_clt,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 30000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_estat = cut(
      quantidade_vinculos_estatutarios,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::left_join(rais.estab.cat.03, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/estab/rais_estab_2010.rds"))

rm(rais.estab.2010)

# 2011 - 2015 ----

# 2011
rais.estab.2011 <- readRDS("db/rais/estab/mod/rais_estab_2011_mod_v2.rds") %>% 
  dplyr::mutate(post = case_when(
    ano %in% c(2000, 2001, 2002) ~ 0,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(1, 2) ~ 0,
    AP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(1.1, 2.1, 2.2, 3.3, 3.7, 4.2) ~ 0,
    RP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.estab.cat.05, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.estab.cat.01, "tamanho") %>% 
  dplyr::mutate(
    range_qtd_vinc_ativos = cut(
      quantidade_vinculos_ativos,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_clt = cut(
      quantidade_vinculos_clt,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 30000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_estat = cut(
      quantidade_vinculos_estatutarios,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::left_join(rais.estab.cat.03, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/estab/rais_estab_2011.rds"))

rm(rais.estab.2011)


# 2012
rais.estab.2012 <- readRDS("db/rais/estab/mod/rais_estab_2012_mod_v2.rds") %>% 
  dplyr::mutate(post = case_when(
    ano %in% c(2000, 2001, 2002) ~ 0,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(1, 2) ~ 0,
    AP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(1.1, 2.1, 2.2, 3.3, 3.7, 4.2) ~ 0,
    RP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.estab.cat.05, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.estab.cat.01, "tamanho") %>% 
  dplyr::mutate(
    range_qtd_vinc_ativos = cut(
      quantidade_vinculos_ativos,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_clt = cut(
      quantidade_vinculos_clt,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 30000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_estat = cut(
      quantidade_vinculos_estatutarios,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::left_join(rais.estab.cat.03, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/estab/rais_estab_2012.rds"))

rm(rais.estab.2012)


# 2013
rais.estab.2013 <- readRDS("db/rais/estab/mod/rais_estab_2013_mod_v2.rds") %>% 
  dplyr::mutate(post = case_when(
    ano %in% c(2000, 2001, 2002) ~ 0,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(1, 2) ~ 0,
    AP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(1.1, 2.1, 2.2, 3.3, 3.7, 4.2) ~ 0,
    RP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.estab.cat.05, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.estab.cat.01, "tamanho") %>% 
  dplyr::mutate(
    range_qtd_vinc_ativos = cut(
      quantidade_vinculos_ativos,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_clt = cut(
      quantidade_vinculos_clt,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 30000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_estat = cut(
      quantidade_vinculos_estatutarios,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::left_join(rais.estab.cat.03, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/estab/rais_estab_2013.rds"))

rm(rais.estab.2013)


# 2014
rais.estab.2014 <- readRDS("db/rais/estab/mod/rais_estab_2014_mod_v2.rds") %>% 
  dplyr::mutate(post = case_when(
    ano %in% c(2000, 2001, 2002) ~ 0,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(1, 2) ~ 0,
    AP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(1.1, 2.1, 2.2, 3.3, 3.7, 4.2) ~ 0,
    RP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.estab.cat.05, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.estab.cat.01, "tamanho") %>% 
  dplyr::mutate(
    range_qtd_vinc_ativos = cut(
      quantidade_vinculos_ativos,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_clt = cut(
      quantidade_vinculos_clt,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 30000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_estat = cut(
      quantidade_vinculos_estatutarios,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::left_join(rais.estab.cat.03, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/estab/rais_estab_2014.rds"))

rm(rais.estab.2014)


# 2015
rais.estab.2015 <- readRDS("db/rais/estab/mod/rais_estab_2015_mod_v2.rds") %>% 
  dplyr::mutate(post = case_when(
    ano %in% c(2000, 2001, 2002) ~ 0,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(1, 2) ~ 0,
    AP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(1.1, 2.1, 2.2, 3.3, 3.7, 4.2) ~ 0,
    RP == NA ~ NA_real_,
    TRUE ~ 1
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.estab.cat.05, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.estab.cat.01, "tamanho") %>% 
  dplyr::mutate(
    range_qtd_vinc_ativos = cut(
      quantidade_vinculos_ativos,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_clt = cut(
      quantidade_vinculos_clt,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 30000, by = 10000)
      )
    )
  ) %>% 
  dplyr::mutate(
    range_qtd_vinc_estat = cut(
      quantidade_vinculos_estatutarios,
      c(
        seq(from = 0, to = 99, by = 10),
        seq(from = 100, to = 999, by = 100),
        seq(from = 1000, to = 9999, by = 1000),
        seq(from = 10000, to = 100000, by = 10000)
      )
    )
  ) %>% 
  dplyr::left_join(rais.estab.cat.03, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/estab/rais_estab_2015.rds"))

rm(rais.estab.2015)


# converting RAIS files to parquet (takes a while!) ----
rds_files_estab <- fs::dir_ls("db/rais/estab/", regexp = "rais(.*)\\.rds")

rds_to_arrow <- function(file) {
  name <- stringr::str_sub(file, end = -5)
  
  year <- stringr::str_sub(name, start = -4)
  
  df <- readr::read_rds(file) # %>%
  # dplyr::mutate(ano = year)
  
  arrow::write_parquet(df, glue::glue("{name}.parquet"))
  
  # fs::dir_delete(file)
}

purrr::walk(
  rds_files_estab,
  rds_to_arrow
)
