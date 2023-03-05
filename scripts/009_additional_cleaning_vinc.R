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


### RAIS vinc ----
rais.vinc.cat.01 <- readxl::read_xlsx("inputs/refs/test.xlsx",
                                      sheet = 7
) %>%
  dplyr::select(faixa_remuneracao_media_sm, cat_faixa_remuneracao_media_sm) %>%
  dplyr::mutate(faixa_remuneracao_media_sm = as.character(faixa_remuneracao_media_sm)) %>%
  tidyr::drop_na()

rais.vinc.cat.02 <- readxl::read_xlsx("inputs/refs/test.xlsx",
                                      sheet = 7
) %>%
  dplyr::select(faixa_horas_contratadas, cat_faixa_horas_contratadas) %>%
  dplyr::mutate(faixa_horas_contratadas = as.character(faixa_horas_contratadas)) %>%
  tidyr::drop_na()

rais.vinc.cat.03 <- readxl::read_xlsx("inputs/refs/test.xlsx",
                                      sheet = 7
) %>%
  dplyr::select(sexo, cat_sexo) %>%
  dplyr::mutate(sexo = as.character(sexo)) %>%
  tidyr::drop_na()

rais.vinc.cat.04 <- readxl::read_xlsx("inputs/refs/test.xlsx",
                                      sheet = 7
) %>%
  dplyr::select(raca_cor, cat_raca_cor) %>%
  dplyr::mutate(raca_cor = as.character(raca_cor)) %>%
  tidyr::drop_na()

rais.vinc.cat.05 <- readxl::read_xlsx("inputs/refs/test.xlsx",
                                      sheet = 7
) %>%
  dplyr::select(faixa_etaria, cat_faixa_etaria) %>%
  dplyr::mutate(faixa_etaria = as.character(faixa_etaria)) %>%
  tidyr::drop_na()

rais.vinc.cat.06 <- readxl::read_xlsx("inputs/refs/test.xlsx",
                                      sheet = 7
) %>%
  dplyr::select(grau_instrucao_1985_2005, cat_grau_instrucao_1985_2005) %>%
  dplyr::mutate(grau_instrucao_1985_2005 = as.character(grau_instrucao_1985_2005)) %>%
  tidyr::drop_na()

rais.vinc.cat.07 <- readxl::read_xlsx("inputs/refs/test.xlsx",
                                      sheet = 7
) %>%
  dplyr::select(grau_instrucao_apos_2005, cat_grau_instrucao_apos_2005) %>%
  dplyr::mutate(grau_instrucao_apos_2005 = as.character(grau_instrucao_apos_2005)) %>%
  tidyr::drop_na()

rais.vinc.cat.08 <- readxl::read_xlsx("inputs/refs/test.xlsx",
                                      sheet = 7
) %>%
  dplyr::select(tipo_vinculo, cat_tipo_vinculo) %>%
  dplyr::mutate(tipo_vinculo = as.character(tipo_vinculo)) %>%
  tidyr::drop_na()

rais.vinc.cat.09 <- readxl::read_xlsx("inputs/refs/test.xlsx",
                                      sheet = 7
) %>%
  dplyr::select(tamanho_estabelecimento, cat_tamanho_estabelecimento) %>%
  dplyr::mutate(tamanho_estabelecimento = as.character(tamanho_estabelecimento)) %>%
  tidyr::drop_na()

rais.vinc.cat.11 <- readxl::read_xlsx("inputs/refs/test.xlsx",
                                      sheet = 7
) %>%
  dplyr::select(natureza_juridica, cat_natureza_juridica) %>%
  dplyr::mutate(natureza_juridica = as.character(natureza_juridica)) %>%
  tidyr::drop_na()

rais.vinc.cat.16 <- readxl::read_xlsx("inputs/refs/test.xlsx",
                                      sheet = 7
) %>%
  dplyr::select(cnae_1, cat_cnae_1) %>%
  dplyr::mutate(cnae_1 = as.character(cnae_1)) %>%
  tidyr::drop_na()

rais.vinc.cat.20 <- readxl::read_xlsx("inputs/refs/test.xlsx",
                                      sheet = 7
) %>%
  dplyr::select(tipo_admissao, cat_tipo_admissao) %>%
  dplyr::mutate(tipo_admissao = as.character(tipo_admissao)) %>%
  tidyr::drop_na()

rais.vinc.cat.21 <- readxl::read_xlsx("inputs/refs/test.xlsx",
                                      sheet = 7
) %>%
  dplyr::select(motivo_desligamento, cat_motivo_desligamento) %>%
  dplyr::mutate(motivo_desligamento = as.character(motivo_desligamento)) %>%
  tidyr::drop_na()

rais.vinc.cat.23 <- readxl::read_xlsx("inputs/refs/test.xlsx",
                                      sheet = 7
) %>%
  dplyr::select(faixa_tempo_emprego, cat_faixa_tempo_emprego) %>%
  dplyr::mutate(faixa_tempo_emprego = as.character(faixa_tempo_emprego)) %>%
  tidyr::drop_na()

rais.vinc.cat.24 <- readxl::read_xlsx("inputs/refs/test.xlsx",
                                      sheet = 7
) %>%
  dplyr::select(faixa_remuneracao_dezembro_sm, cat_faixa_remuneracao_dezembro_sm) %>%
  dplyr::mutate(faixa_remuneracao_dezembro_sm = as.character(faixa_remuneracao_dezembro_sm)) %>%
  tidyr::drop_na()

## new joins ----

# 2000 - 2005 ----

rais.vinc.2000 <- readRDS("db/rais/vinc/mod/rais_vinc_2000_mod_v2.rds") %>% 
  dplyr::select(-c(
    educ_id
  )) %>% 
  dplyr::left_join(rais.vinc.cat.03, "sexo") %>% 
  dplyr::mutate(gender_id = case_when(
    cat_sexo == "feminino" ~ 1,
    cat_sexo == NA ~ NA_real_,
    TRUE ~ 0
  )) %>% 
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
  dplyr::left_join(rais.vinc.cat.20, "tipo_admissao") %>% 
  dplyr::mutate(first_job = case_when(
    cat_tipo_admissao == "PRIM EMPREGO" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::left_join(rais.vinc.cat.08, "tipo_vinculo") %>% 
  dplyr::mutate(public_work = case_when(
    cat_tipo_vinculo %in% c("ESTAT N/EFET", "ESTATUTARIO", "ESTAT RGPS") ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.vinc.cat.16, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.vinc.cat.01, "faixa_remuneracao_media_sm") %>% 
  dplyr::left_join(rais.vinc.cat.24, "faixa_remuneracao_dezembro_sm") %>% 
  dplyr::mutate(
    range_remuneracao_media = cut(
      valor_remuneracao_media,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::mutate(
    range_remuneracao_dezembro = cut(
      valor_remuneracao_dezembro,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::left_join(rais.vinc.cat.21, "motivo_desligamento") %>% 
  dplyr::left_join(rais.vinc.cat.23, "faixa_tempo_emprego") %>% 
  dplyr::left_join(rais.vinc.cat.02, "faixa_horas_contratadas") %>% 
  dplyr::left_join(rais.vinc.cat.04, "raca_cor") %>% 
  dplyr::left_join(rais.vinc.cat.05, "faixa_etaria") %>% 
  dplyr::left_join(rais.vinc.cat.06, "grau_instrucao_1985_2005") %>% 
  dplyr::left_join(rais.vinc.cat.07, "grau_instrucao_apos_2005") %>% 
  dplyr::mutate(educ_id = grau_instrucao_1985_2005) %>% 
  dplyr::mutate(cat_educ = cat_grau_instrucao_1985_2005) %>% 
  dplyr::left_join(rais.vinc.cat.09, "tamanho_estabelecimento") %>% 
  dplyr::left_join(rais.vinc.cat.11, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/vinc/rais_vinc_2000.rds"))

rm(rais.vinc.2000)

gc()

rais.vinc.2001 <- readRDS("db/rais/vinc/mod/rais_vinc_2001_mod_v2.rds") %>% 
  dplyr::select(-c(
    educ_id
  )) %>% 
  dplyr::left_join(rais.vinc.cat.03, "sexo") %>% 
  dplyr::mutate(gender_id = case_when(
    cat_sexo == "feminino" ~ 1,
    cat_sexo == NA ~ NA_real_,
    TRUE ~ 0
  )) %>% 
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
  dplyr::left_join(rais.vinc.cat.20, "tipo_admissao") %>% 
  dplyr::mutate(first_job = case_when(
    cat_tipo_admissao == "PRIM EMPREGO" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::left_join(rais.vinc.cat.08, "tipo_vinculo") %>% 
  dplyr::mutate(public_work = case_when(
    cat_tipo_vinculo %in% c("ESTAT N/EFET", "ESTATUTARIO", "ESTAT RGPS") ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.vinc.cat.16, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.vinc.cat.01, "faixa_remuneracao_media_sm") %>% 
  dplyr::left_join(rais.vinc.cat.24, "faixa_remuneracao_dezembro_sm") %>% 
  dplyr::mutate(
    range_remuneracao_media = cut(
      valor_remuneracao_media,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::mutate(
    range_remuneracao_dezembro = cut(
      valor_remuneracao_dezembro,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::left_join(rais.vinc.cat.21, "motivo_desligamento") %>% 
  dplyr::left_join(rais.vinc.cat.23, "faixa_tempo_emprego") %>% 
  dplyr::left_join(rais.vinc.cat.02, "faixa_horas_contratadas") %>% 
  dplyr::left_join(rais.vinc.cat.04, "raca_cor") %>% 
  dplyr::left_join(rais.vinc.cat.05, "faixa_etaria") %>% 
  dplyr::left_join(rais.vinc.cat.06, "grau_instrucao_1985_2005") %>% 
  dplyr::left_join(rais.vinc.cat.07, "grau_instrucao_apos_2005") %>% 
  dplyr::mutate(educ_id = grau_instrucao_1985_2005) %>% 
  dplyr::mutate(cat_educ = cat_grau_instrucao_1985_2005) %>% 
  dplyr::left_join(rais.vinc.cat.09, "tamanho_estabelecimento") %>% 
  dplyr::left_join(rais.vinc.cat.11, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/vinc/rais_vinc_2001.rds"))

rm(rais.vinc.2001)

gc()

rais.vinc.2002 <- readRDS("db/rais/vinc/mod/rais_vinc_2002_mod_v2.rds") %>% 
  dplyr::select(-c(
    educ_id
  )) %>% 
  dplyr::left_join(rais.vinc.cat.03, "sexo") %>% 
  dplyr::mutate(gender_id = case_when(
    cat_sexo == "feminino" ~ 1,
    cat_sexo == NA ~ NA_real_,
    TRUE ~ 0
  )) %>% 
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
  dplyr::left_join(rais.vinc.cat.20, "tipo_admissao") %>% 
  dplyr::mutate(first_job = case_when(
    cat_tipo_admissao == "PRIM EMPREGO" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::left_join(rais.vinc.cat.08, "tipo_vinculo") %>% 
  dplyr::mutate(public_work = case_when(
    cat_tipo_vinculo %in% c("ESTAT N/EFET", "ESTATUTARIO", "ESTAT RGPS") ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.vinc.cat.16, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.vinc.cat.01, "faixa_remuneracao_media_sm") %>% 
  dplyr::left_join(rais.vinc.cat.24, "faixa_remuneracao_dezembro_sm") %>% 
  dplyr::mutate(
    range_remuneracao_media = cut(
      valor_remuneracao_media,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::mutate(
    range_remuneracao_dezembro = cut(
      valor_remuneracao_dezembro,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::left_join(rais.vinc.cat.21, "motivo_desligamento") %>% 
  dplyr::left_join(rais.vinc.cat.23, "faixa_tempo_emprego") %>% 
  dplyr::left_join(rais.vinc.cat.02, "faixa_horas_contratadas") %>% 
  dplyr::left_join(rais.vinc.cat.04, "raca_cor") %>% 
  dplyr::left_join(rais.vinc.cat.05, "faixa_etaria") %>% 
  dplyr::left_join(rais.vinc.cat.06, "grau_instrucao_1985_2005") %>% 
  dplyr::left_join(rais.vinc.cat.07, "grau_instrucao_apos_2005") %>% 
  dplyr::mutate(educ_id = grau_instrucao_1985_2005) %>% 
  dplyr::mutate(cat_educ = cat_grau_instrucao_1985_2005) %>% 
  dplyr::left_join(rais.vinc.cat.09, "tamanho_estabelecimento") %>% 
  dplyr::left_join(rais.vinc.cat.11, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/vinc/rais_vinc_2002.rds"))

rm(rais.vinc.2002)

gc()

rais.vinc.2003 <- readRDS("db/rais/vinc/mod/rais_vinc_2003_mod_v2.rds") %>% 
  dplyr::select(-c(
    educ_id
  )) %>% 
  dplyr::left_join(rais.vinc.cat.03, "sexo") %>% 
  dplyr::mutate(gender_id = case_when(
    cat_sexo == "feminino" ~ 1,
    cat_sexo == NA ~ NA_real_,
    TRUE ~ 0
  )) %>% 
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
  dplyr::left_join(rais.vinc.cat.20, "tipo_admissao") %>% 
  dplyr::mutate(first_job = case_when(
    cat_tipo_admissao == "PRIM EMPREGO" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::left_join(rais.vinc.cat.08, "tipo_vinculo") %>% 
  dplyr::mutate(public_work = case_when(
    cat_tipo_vinculo %in% c("ESTAT N/EFET", "ESTATUTARIO", "ESTAT RGPS") ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.vinc.cat.16, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.vinc.cat.01, "faixa_remuneracao_media_sm") %>% 
  dplyr::left_join(rais.vinc.cat.24, "faixa_remuneracao_dezembro_sm") %>% 
  dplyr::mutate(
    range_remuneracao_media = cut(
      valor_remuneracao_media,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::mutate(
    range_remuneracao_dezembro = cut(
      valor_remuneracao_dezembro,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::left_join(rais.vinc.cat.21, "motivo_desligamento") %>% 
  dplyr::left_join(rais.vinc.cat.23, "faixa_tempo_emprego") %>% 
  dplyr::left_join(rais.vinc.cat.02, "faixa_horas_contratadas") %>% 
  dplyr::left_join(rais.vinc.cat.04, "raca_cor") %>% 
  dplyr::left_join(rais.vinc.cat.05, "faixa_etaria") %>% 
  dplyr::left_join(rais.vinc.cat.06, "grau_instrucao_1985_2005") %>% 
  dplyr::left_join(rais.vinc.cat.07, "grau_instrucao_apos_2005") %>% 
  dplyr::mutate(educ_id = grau_instrucao_1985_2005) %>% 
  dplyr::mutate(cat_educ = cat_grau_instrucao_1985_2005) %>% 
  dplyr::left_join(rais.vinc.cat.09, "tamanho_estabelecimento") %>% 
  dplyr::left_join(rais.vinc.cat.11, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/vinc/rais_vinc_2003.rds"))

rm(rais.vinc.2003)

gc()

rais.vinc.2004 <- readRDS("db/rais/vinc/mod/rais_vinc_2004_mod_v2.rds") %>% 
  dplyr::select(-c(
    educ_id
  )) %>% 
  dplyr::left_join(rais.vinc.cat.03, "sexo") %>% 
  dplyr::mutate(gender_id = case_when(
    cat_sexo == "feminino" ~ 1,
    cat_sexo == NA ~ NA_real_,
    TRUE ~ 0
  )) %>% 
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
  dplyr::left_join(rais.vinc.cat.20, "tipo_admissao") %>% 
  dplyr::mutate(first_job = case_when(
    cat_tipo_admissao == "PRIM EMPREGO" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::left_join(rais.vinc.cat.08, "tipo_vinculo") %>% 
  dplyr::mutate(public_work = case_when(
    cat_tipo_vinculo %in% c("ESTAT N/EFET", "ESTATUTARIO", "ESTAT RGPS") ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.vinc.cat.16, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.vinc.cat.01, "faixa_remuneracao_media_sm") %>% 
  dplyr::left_join(rais.vinc.cat.24, "faixa_remuneracao_dezembro_sm") %>% 
  dplyr::mutate(
    range_remuneracao_media = cut(
      valor_remuneracao_media,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::mutate(
    range_remuneracao_dezembro = cut(
      valor_remuneracao_dezembro,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::left_join(rais.vinc.cat.21, "motivo_desligamento") %>% 
  dplyr::left_join(rais.vinc.cat.23, "faixa_tempo_emprego") %>% 
  dplyr::left_join(rais.vinc.cat.02, "faixa_horas_contratadas") %>% 
  dplyr::left_join(rais.vinc.cat.04, "raca_cor") %>% 
  dplyr::left_join(rais.vinc.cat.05, "faixa_etaria") %>% 
  dplyr::left_join(rais.vinc.cat.06, "grau_instrucao_1985_2005") %>% 
  dplyr::left_join(rais.vinc.cat.07, "grau_instrucao_apos_2005") %>% 
  dplyr::mutate(educ_id = grau_instrucao_1985_2005) %>% 
  dplyr::mutate(cat_educ = cat_grau_instrucao_1985_2005) %>% 
  dplyr::left_join(rais.vinc.cat.09, "tamanho_estabelecimento") %>% 
  dplyr::left_join(rais.vinc.cat.11, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/vinc/rais_vinc_2004.rds"))

rm(rais.vinc.2004)

gc()

rais.vinc.2005 <- readRDS("db/rais/vinc/mod/rais_vinc_2005_mod_v2.rds") %>% 
  dplyr::select(-c(
    educ_id
  )) %>% 
  dplyr::left_join(rais.vinc.cat.03, "sexo") %>% 
  dplyr::mutate(gender_id = case_when(
    cat_sexo == "feminino" ~ 1,
    cat_sexo == NA ~ NA_real_,
    TRUE ~ 0
  )) %>% 
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
  dplyr::left_join(rais.vinc.cat.20, "tipo_admissao") %>% 
  dplyr::mutate(first_job = case_when(
    cat_tipo_admissao == "PRIM EMPREGO" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::left_join(rais.vinc.cat.08, "tipo_vinculo") %>% 
  dplyr::mutate(public_work = case_when(
    cat_tipo_vinculo %in% c("ESTAT N/EFET", "ESTATUTARIO", "ESTAT RGPS") ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.vinc.cat.16, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.vinc.cat.01, "faixa_remuneracao_media_sm") %>% 
  dplyr::left_join(rais.vinc.cat.24, "faixa_remuneracao_dezembro_sm") %>% 
  dplyr::mutate(
    range_remuneracao_media = cut(
      valor_remuneracao_media,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::mutate(
    range_remuneracao_dezembro = cut(
      valor_remuneracao_dezembro,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::left_join(rais.vinc.cat.21, "motivo_desligamento") %>% 
  dplyr::left_join(rais.vinc.cat.23, "faixa_tempo_emprego") %>% 
  dplyr::left_join(rais.vinc.cat.02, "faixa_horas_contratadas") %>% 
  dplyr::left_join(rais.vinc.cat.04, "raca_cor") %>% 
  dplyr::left_join(rais.vinc.cat.05, "faixa_etaria") %>% 
  dplyr::left_join(rais.vinc.cat.06, "grau_instrucao_1985_2005") %>% 
  dplyr::left_join(rais.vinc.cat.07, "grau_instrucao_apos_2005") %>% 
  dplyr::mutate(educ_id = grau_instrucao_1985_2005) %>% 
  dplyr::mutate(cat_educ = cat_grau_instrucao_1985_2005) %>% 
  dplyr::left_join(rais.vinc.cat.09, "tamanho_estabelecimento") %>% 
  dplyr::left_join(rais.vinc.cat.11, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/vinc/rais_vinc_2005.rds"))

rm(rais.vinc.2005)

gc()

# 2006 - 2010 ----

rais.vinc.2006 <- readRDS("db/rais/vinc/mod/rais_vinc_2006_mod_v2.rds") %>% 
  dplyr::left_join(rais.vinc.cat.03, "sexo") %>% 
  dplyr::mutate(gender_id = case_when(
    cat_sexo == "feminino" ~ 1,
    cat_sexo == NA ~ NA_real_,
    TRUE ~ 0
  )) %>% 
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
  dplyr::left_join(rais.vinc.cat.20, "tipo_admissao") %>% 
  dplyr::mutate(first_job = case_when(
    cat_tipo_admissao == "PRIM EMPREGO" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::left_join(rais.vinc.cat.08, "tipo_vinculo") %>% 
  dplyr::mutate(public_work = case_when(
    cat_tipo_vinculo %in% c("ESTAT N/EFET", "ESTATUTARIO", "ESTAT RGPS") ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.vinc.cat.16, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.vinc.cat.01, "faixa_remuneracao_media_sm") %>% 
  dplyr::left_join(rais.vinc.cat.24, "faixa_remuneracao_dezembro_sm") %>% 
  dplyr::mutate(
    range_remuneracao_media = cut(
      valor_remuneracao_media,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::mutate(
    range_remuneracao_dezembro = cut(
      valor_remuneracao_dezembro,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::left_join(rais.vinc.cat.21, "motivo_desligamento") %>% 
  dplyr::left_join(rais.vinc.cat.23, "faixa_tempo_emprego") %>% 
  dplyr::left_join(rais.vinc.cat.02, "faixa_horas_contratadas") %>% 
  dplyr::left_join(rais.vinc.cat.04, "raca_cor") %>% 
  dplyr::left_join(rais.vinc.cat.05, "faixa_etaria") %>% 
  dplyr::left_join(rais.vinc.cat.06, "grau_instrucao_1985_2005") %>% 
  dplyr::left_join(rais.vinc.cat.07, "grau_instrucao_apos_2005") %>% 
  dplyr::mutate(educ_id = grau_instrucao_apos_2005) %>% 
  dplyr::mutate(cat_educ = cat_grau_instrucao_apos_2005) %>% 
  dplyr::left_join(rais.vinc.cat.09, "tamanho_estabelecimento") %>% 
  dplyr::left_join(rais.vinc.cat.11, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/vinc/rais_vinc_2006.rds"))

rm(rais.vinc.2006)

gc()

rais.vinc.2007 <- readRDS("db/rais/vinc/mod/rais_vinc_2007_mod_v3.rds") %>% 
  dplyr::left_join(rais.vinc.cat.03, "sexo") %>% 
  dplyr::mutate(gender_id = case_when(
    cat_sexo == "feminino" ~ 1,
    cat_sexo == NA ~ NA_real_,
    TRUE ~ 0
  )) %>% 
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
  dplyr::left_join(rais.vinc.cat.20, "tipo_admissao") %>% 
  dplyr::mutate(first_job = case_when(
    cat_tipo_admissao == "PRIM EMPREGO" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::left_join(rais.vinc.cat.08, "tipo_vinculo") %>% 
  dplyr::mutate(public_work = case_when(
    cat_tipo_vinculo %in% c("ESTAT N/EFET", "ESTATUTARIO", "ESTAT RGPS") ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.vinc.cat.16, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.vinc.cat.01, "faixa_remuneracao_media_sm") %>% 
  dplyr::left_join(rais.vinc.cat.24, "faixa_remuneracao_dezembro_sm") %>% 
  dplyr::mutate(
    range_remuneracao_media = cut(
      valor_remuneracao_media,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::mutate(
    range_remuneracao_dezembro = cut(
      valor_remuneracao_dezembro,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::left_join(rais.vinc.cat.21, "motivo_desligamento") %>% 
  dplyr::left_join(rais.vinc.cat.23, "faixa_tempo_emprego") %>% 
  dplyr::left_join(rais.vinc.cat.02, "faixa_horas_contratadas") %>% 
  dplyr::left_join(rais.vinc.cat.04, "raca_cor") %>% 
  dplyr::left_join(rais.vinc.cat.05, "faixa_etaria") %>% 
  dplyr::left_join(rais.vinc.cat.06, "grau_instrucao_1985_2005") %>% 
  dplyr::left_join(rais.vinc.cat.07, "grau_instrucao_apos_2005") %>% 
  dplyr::mutate(educ_id = grau_instrucao_apos_2005) %>% 
  dplyr::mutate(cat_educ = cat_grau_instrucao_apos_2005) %>% 
  dplyr::left_join(rais.vinc.cat.09, "tamanho_estabelecimento") %>% 
  dplyr::left_join(rais.vinc.cat.11, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/vinc/rais_vinc_2007.rds"))

rm(rais.vinc.2007)

gc()

rais.vinc.2008 <- readRDS("db/rais/vinc/mod/rais_vinc_2008_mod_v2.rds") %>% 
  dplyr::left_join(rais.vinc.cat.03, "sexo") %>% 
  dplyr::mutate(gender_id = case_when(
    cat_sexo == "feminino" ~ 1,
    cat_sexo == NA ~ NA_real_,
    TRUE ~ 0
  )) %>% 
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
  dplyr::left_join(rais.vinc.cat.20, "tipo_admissao") %>% 
  dplyr::mutate(first_job = case_when(
    cat_tipo_admissao == "PRIM EMPREGO" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::left_join(rais.vinc.cat.08, "tipo_vinculo") %>% 
  dplyr::mutate(public_work = case_when(
    cat_tipo_vinculo %in% c("ESTAT N/EFET", "ESTATUTARIO", "ESTAT RGPS") ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.vinc.cat.16, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.vinc.cat.01, "faixa_remuneracao_media_sm") %>% 
  dplyr::left_join(rais.vinc.cat.24, "faixa_remuneracao_dezembro_sm") %>% 
  dplyr::mutate(
    range_remuneracao_media = cut(
      valor_remuneracao_media,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::mutate(
    range_remuneracao_dezembro = cut(
      valor_remuneracao_dezembro,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::left_join(rais.vinc.cat.21, "motivo_desligamento") %>% 
  dplyr::left_join(rais.vinc.cat.23, "faixa_tempo_emprego") %>% 
  dplyr::left_join(rais.vinc.cat.02, "faixa_horas_contratadas") %>% 
  dplyr::left_join(rais.vinc.cat.04, "raca_cor") %>% 
  dplyr::left_join(rais.vinc.cat.05, "faixa_etaria") %>% 
  dplyr::left_join(rais.vinc.cat.06, "grau_instrucao_1985_2005") %>% 
  dplyr::left_join(rais.vinc.cat.07, "grau_instrucao_apos_2005") %>% 
  dplyr::mutate(educ_id = grau_instrucao_apos_2005) %>% 
  dplyr::mutate(cat_educ = cat_grau_instrucao_apos_2005) %>% 
  dplyr::left_join(rais.vinc.cat.09, "tamanho_estabelecimento") %>% 
  dplyr::left_join(rais.vinc.cat.11, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/vinc/rais_vinc_2008.rds"))

rm(rais.vinc.2008)

gc()

rais.vinc.2009 <- readRDS("db/rais/vinc/mod/rais_vinc_2009_mod_v2.rds") %>% 
  dplyr::left_join(rais.vinc.cat.03, "sexo") %>% 
  dplyr::mutate(gender_id = case_when(
    cat_sexo == "feminino" ~ 1,
    cat_sexo == NA ~ NA_real_,
    TRUE ~ 0
  )) %>% 
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
  dplyr::left_join(rais.vinc.cat.20, "tipo_admissao") %>% 
  dplyr::mutate(first_job = case_when(
    cat_tipo_admissao == "PRIM EMPREGO" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::left_join(rais.vinc.cat.08, "tipo_vinculo") %>% 
  dplyr::mutate(public_work = case_when(
    cat_tipo_vinculo %in% c("ESTAT N/EFET", "ESTATUTARIO", "ESTAT RGPS") ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.vinc.cat.16, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.vinc.cat.01, "faixa_remuneracao_media_sm") %>% 
  dplyr::left_join(rais.vinc.cat.24, "faixa_remuneracao_dezembro_sm") %>% 
  dplyr::mutate(
    range_remuneracao_media = cut(
      valor_remuneracao_media,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::mutate(
    range_remuneracao_dezembro = cut(
      valor_remuneracao_dezembro,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::left_join(rais.vinc.cat.21, "motivo_desligamento") %>% 
  dplyr::left_join(rais.vinc.cat.23, "faixa_tempo_emprego") %>% 
  dplyr::left_join(rais.vinc.cat.02, "faixa_horas_contratadas") %>% 
  dplyr::left_join(rais.vinc.cat.04, "raca_cor") %>% 
  dplyr::left_join(rais.vinc.cat.05, "faixa_etaria") %>% 
  dplyr::left_join(rais.vinc.cat.06, "grau_instrucao_1985_2005") %>% 
  dplyr::left_join(rais.vinc.cat.07, "grau_instrucao_apos_2005") %>% 
  dplyr::mutate(educ_id = grau_instrucao_apos_2005) %>% 
  dplyr::mutate(cat_educ = cat_grau_instrucao_apos_2005) %>% 
  dplyr::left_join(rais.vinc.cat.09, "tamanho_estabelecimento") %>% 
  dplyr::left_join(rais.vinc.cat.11, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/vinc/rais_vinc_2009.rds"))

rm(rais.vinc.2009)

gc()

rais.vinc.2010 <- readRDS("db/rais/vinc/mod/rais_vinc_2010_mod_v3.rds") %>% 
  dplyr::left_join(rais.vinc.cat.03, "sexo") %>% 
  dplyr::mutate(gender_id = case_when(
    cat_sexo == "feminino" ~ 1,
    cat_sexo == NA ~ NA_real_,
    TRUE ~ 0
  )) %>% 
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
  dplyr::left_join(rais.vinc.cat.20, "tipo_admissao") %>% 
  dplyr::mutate(first_job = case_when(
    cat_tipo_admissao == "PRIM EMPREGO" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::left_join(rais.vinc.cat.08, "tipo_vinculo") %>% 
  dplyr::mutate(public_work = case_when(
    cat_tipo_vinculo %in% c("ESTAT N/EFET", "ESTATUTARIO", "ESTAT RGPS") ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.vinc.cat.16, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.vinc.cat.01, "faixa_remuneracao_media_sm") %>% 
  dplyr::left_join(rais.vinc.cat.24, "faixa_remuneracao_dezembro_sm") %>% 
  dplyr::mutate(
    range_remuneracao_media = cut(
      valor_remuneracao_media,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::mutate(
    range_remuneracao_dezembro = cut(
      valor_remuneracao_dezembro,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::left_join(rais.vinc.cat.21, "motivo_desligamento") %>% 
  dplyr::left_join(rais.vinc.cat.23, "faixa_tempo_emprego") %>% 
  dplyr::left_join(rais.vinc.cat.02, "faixa_horas_contratadas") %>% 
  dplyr::left_join(rais.vinc.cat.04, "raca_cor") %>% 
  dplyr::left_join(rais.vinc.cat.05, "faixa_etaria") %>% 
  dplyr::left_join(rais.vinc.cat.06, "grau_instrucao_1985_2005") %>% 
  dplyr::left_join(rais.vinc.cat.07, "grau_instrucao_apos_2005") %>% 
  dplyr::mutate(educ_id = grau_instrucao_apos_2005) %>% 
  dplyr::mutate(cat_educ = cat_grau_instrucao_apos_2005) %>% 
  dplyr::left_join(rais.vinc.cat.09, "tamanho_estabelecimento") %>% 
  dplyr::left_join(rais.vinc.cat.11, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/vinc/rais_vinc_2010.rds"))

rm(rais.vinc.2010)

gc()

# 2011 - 2015 ----

rais.vinc.2011 <- readRDS("db/rais/vinc/mod/rais_vinc_2011_mod_v2.rds") %>% 
  dplyr::left_join(rais.vinc.cat.03, "sexo") %>% 
  dplyr::mutate(gender_id = case_when(
    cat_sexo == "feminino" ~ 1,
    cat_sexo == NA ~ NA_real_,
    TRUE ~ 0
  )) %>% 
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
  dplyr::left_join(rais.vinc.cat.20, "tipo_admissao") %>% 
  dplyr::mutate(first_job = case_when(
    cat_tipo_admissao == "PRIM EMPREGO" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::left_join(rais.vinc.cat.08, "tipo_vinculo") %>% 
  dplyr::mutate(public_work = case_when(
    cat_tipo_vinculo %in% c("ESTAT N/EFET", "ESTATUTARIO", "ESTAT RGPS") ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.vinc.cat.16, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.vinc.cat.01, "faixa_remuneracao_media_sm") %>% 
  dplyr::left_join(rais.vinc.cat.24, "faixa_remuneracao_dezembro_sm") %>% 
  dplyr::mutate(
    range_remuneracao_media = cut(
      valor_remuneracao_media,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::mutate(
    range_remuneracao_dezembro = cut(
      valor_remuneracao_dezembro,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::left_join(rais.vinc.cat.21, "motivo_desligamento") %>% 
  dplyr::left_join(rais.vinc.cat.23, "faixa_tempo_emprego") %>% 
  dplyr::left_join(rais.vinc.cat.02, "faixa_horas_contratadas") %>% 
  dplyr::left_join(rais.vinc.cat.04, "raca_cor") %>% 
  dplyr::left_join(rais.vinc.cat.05, "faixa_etaria") %>% 
  dplyr::left_join(rais.vinc.cat.06, "grau_instrucao_1985_2005") %>% 
  dplyr::left_join(rais.vinc.cat.07, "grau_instrucao_apos_2005") %>% 
  dplyr::mutate(educ_id = grau_instrucao_apos_2005) %>% 
  dplyr::mutate(cat_educ = cat_grau_instrucao_apos_2005) %>% 
  dplyr::left_join(rais.vinc.cat.09, "tamanho_estabelecimento") %>% 
  dplyr::left_join(rais.vinc.cat.11, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/vinc/rais_vinc_2011.rds"))

rm(rais.vinc.2011)

gc()

rais.vinc.2012 <- readRDS("db/rais/vinc/mod/rais_vinc_2012_mod_v2.rds") %>% 
  dplyr::left_join(rais.vinc.cat.03, "sexo") %>% 
  dplyr::mutate(gender_id = case_when(
    cat_sexo == "feminino" ~ 1,
    cat_sexo == NA ~ NA_real_,
    TRUE ~ 0
  )) %>% 
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
  dplyr::left_join(rais.vinc.cat.20, "tipo_admissao") %>% 
  dplyr::mutate(first_job = case_when(
    cat_tipo_admissao == "PRIM EMPREGO" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::left_join(rais.vinc.cat.08, "tipo_vinculo") %>% 
  dplyr::mutate(public_work = case_when(
    cat_tipo_vinculo %in% c("ESTAT N/EFET", "ESTATUTARIO", "ESTAT RGPS") ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.vinc.cat.16, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.vinc.cat.01, "faixa_remuneracao_media_sm") %>% 
  dplyr::left_join(rais.vinc.cat.24, "faixa_remuneracao_dezembro_sm") %>% 
  dplyr::mutate(
    range_remuneracao_media = cut(
      valor_remuneracao_media,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::mutate(
    range_remuneracao_dezembro = cut(
      valor_remuneracao_dezembro,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::left_join(rais.vinc.cat.21, "motivo_desligamento") %>% 
  dplyr::left_join(rais.vinc.cat.23, "faixa_tempo_emprego") %>% 
  dplyr::left_join(rais.vinc.cat.02, "faixa_horas_contratadas") %>% 
  dplyr::left_join(rais.vinc.cat.04, "raca_cor") %>% 
  dplyr::left_join(rais.vinc.cat.05, "faixa_etaria") %>% 
  dplyr::left_join(rais.vinc.cat.06, "grau_instrucao_1985_2005") %>% 
  dplyr::left_join(rais.vinc.cat.07, "grau_instrucao_apos_2005") %>% 
  dplyr::mutate(educ_id = grau_instrucao_apos_2005) %>% 
  dplyr::mutate(cat_educ = cat_grau_instrucao_apos_2005) %>% 
  dplyr::left_join(rais.vinc.cat.09, "tamanho_estabelecimento") %>% 
  dplyr::left_join(rais.vinc.cat.11, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/vinc/rais_vinc_2012.rds"))

rm(rais.vinc.2012)

gc()

rais.vinc.2013 <- readRDS("db/rais/vinc/mod/rais_vinc_2013_mod_v2.rds") %>% 
  dplyr::left_join(rais.vinc.cat.03, "sexo") %>% 
  dplyr::mutate(gender_id = case_when(
    cat_sexo == "feminino" ~ 1,
    cat_sexo == NA ~ NA_real_,
    TRUE ~ 0
  )) %>% 
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
  dplyr::left_join(rais.vinc.cat.20, "tipo_admissao") %>% 
  dplyr::mutate(first_job = case_when(
    cat_tipo_admissao == "PRIM EMPREGO" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::left_join(rais.vinc.cat.08, "tipo_vinculo") %>% 
  dplyr::mutate(public_work = case_when(
    cat_tipo_vinculo %in% c("ESTAT N/EFET", "ESTATUTARIO", "ESTAT RGPS") ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.vinc.cat.16, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.vinc.cat.01, "faixa_remuneracao_media_sm") %>% 
  dplyr::left_join(rais.vinc.cat.24, "faixa_remuneracao_dezembro_sm") %>% 
  dplyr::mutate(
    range_remuneracao_media = cut(
      valor_remuneracao_media,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::mutate(
    range_remuneracao_dezembro = cut(
      valor_remuneracao_dezembro,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::left_join(rais.vinc.cat.21, "motivo_desligamento") %>% 
  dplyr::left_join(rais.vinc.cat.23, "faixa_tempo_emprego") %>% 
  dplyr::left_join(rais.vinc.cat.02, "faixa_horas_contratadas") %>% 
  dplyr::left_join(rais.vinc.cat.04, "raca_cor") %>% 
  dplyr::left_join(rais.vinc.cat.05, "faixa_etaria") %>% 
  dplyr::left_join(rais.vinc.cat.06, "grau_instrucao_1985_2005") %>% 
  dplyr::left_join(rais.vinc.cat.07, "grau_instrucao_apos_2005") %>% 
  dplyr::mutate(educ_id = grau_instrucao_apos_2005) %>% 
  dplyr::mutate(cat_educ = cat_grau_instrucao_apos_2005) %>% 
  dplyr::left_join(rais.vinc.cat.09, "tamanho_estabelecimento") %>% 
  dplyr::left_join(rais.vinc.cat.11, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/vinc/rais_vinc_2013.rds"))

rm(rais.vinc.2013)

gc()

rais.vinc.2014 <- readRDS("db/rais/vinc/mod/rais_vinc_2014_mod_v2.rds") %>% 
  dplyr::left_join(rais.vinc.cat.03, "sexo") %>% 
  dplyr::mutate(gender_id = case_when(
    cat_sexo == "feminino" ~ 1,
    cat_sexo == NA ~ NA_real_,
    TRUE ~ 0
  )) %>% 
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
  dplyr::left_join(rais.vinc.cat.20, "tipo_admissao") %>% 
  dplyr::mutate(first_job = case_when(
    cat_tipo_admissao == "PRIM EMPREGO" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::left_join(rais.vinc.cat.08, "tipo_vinculo") %>% 
  dplyr::mutate(public_work = case_when(
    cat_tipo_vinculo %in% c("ESTAT N/EFET", "ESTATUTARIO", "ESTAT RGPS") ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.vinc.cat.16, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.vinc.cat.01, "faixa_remuneracao_media_sm") %>% 
  dplyr::left_join(rais.vinc.cat.24, "faixa_remuneracao_dezembro_sm") %>% 
  dplyr::mutate(
    range_remuneracao_media = cut(
      valor_remuneracao_media,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::mutate(
    range_remuneracao_dezembro = cut(
      valor_remuneracao_dezembro,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::left_join(rais.vinc.cat.21, "motivo_desligamento") %>% 
  dplyr::left_join(rais.vinc.cat.23, "faixa_tempo_emprego") %>% 
  dplyr::left_join(rais.vinc.cat.02, "faixa_horas_contratadas") %>% 
  dplyr::left_join(rais.vinc.cat.04, "raca_cor") %>% 
  dplyr::left_join(rais.vinc.cat.05, "faixa_etaria") %>% 
  dplyr::left_join(rais.vinc.cat.06, "grau_instrucao_1985_2005") %>% 
  dplyr::left_join(rais.vinc.cat.07, "grau_instrucao_apos_2005") %>% 
  dplyr::mutate(educ_id = grau_instrucao_apos_2005) %>% 
  dplyr::mutate(cat_educ = cat_grau_instrucao_apos_2005) %>% 
  dplyr::left_join(rais.vinc.cat.09, "tamanho_estabelecimento") %>% 
  dplyr::left_join(rais.vinc.cat.11, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/vinc/rais_vinc_2014.rds"))

rm(rais.vinc.2014)

gc()

rais.vinc.2015 <- readRDS("db/rais/vinc/mod/rais_vinc_2015_mod_v2.rds") %>% 
  dplyr::left_join(rais.vinc.cat.03, "sexo") %>% 
  dplyr::mutate(gender_id = case_when(
    cat_sexo == "feminino" ~ 1,
    cat_sexo == NA ~ NA_real_,
    TRUE ~ 0
  )) %>% 
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
  dplyr::left_join(rais.vinc.cat.20, "tipo_admissao") %>% 
  dplyr::mutate(first_job = case_when(
    cat_tipo_admissao == "PRIM EMPREGO" ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::left_join(rais.vinc.cat.08, "tipo_vinculo") %>% 
  dplyr::mutate(public_work = case_when(
    cat_tipo_vinculo %in% c("ESTAT N/EFET", "ESTATUTARIO", "ESTAT RGPS") ~ 1,
    TRUE ~ 0
  )) %>% 
  dplyr::mutate(cnae_id = as.numeric(cnae_1)) %>% 
  dplyr::left_join(rais.vinc.cat.16, "cnae_1") %>% 
  dplyr::left_join(bairros.rj, "bairros_rj") %>% 
  dplyr::left_join(ap.rais, "AP") %>% 
  dplyr::left_join(rp.rais, "RP") %>% 
  dplyr::left_join(rais.vinc.cat.01, "faixa_remuneracao_media_sm") %>% 
  dplyr::left_join(rais.vinc.cat.24, "faixa_remuneracao_dezembro_sm") %>% 
  dplyr::mutate(
    range_remuneracao_media = cut(
      valor_remuneracao_media,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::mutate(
    range_remuneracao_dezembro = cut(
      valor_remuneracao_dezembro,
      c(seq(from = 0, to = 29000, by = 1000), 150000)
    )
  ) %>% 
  dplyr::left_join(rais.vinc.cat.21, "motivo_desligamento") %>% 
  dplyr::left_join(rais.vinc.cat.23, "faixa_tempo_emprego") %>% 
  dplyr::left_join(rais.vinc.cat.02, "faixa_horas_contratadas") %>% 
  dplyr::left_join(rais.vinc.cat.04, "raca_cor") %>% 
  dplyr::left_join(rais.vinc.cat.05, "faixa_etaria") %>% 
  dplyr::left_join(rais.vinc.cat.06, "grau_instrucao_1985_2005") %>% 
  dplyr::left_join(rais.vinc.cat.07, "grau_instrucao_apos_2005") %>% 
  dplyr::mutate(educ_id = grau_instrucao_apos_2005) %>% 
  dplyr::mutate(cat_educ = cat_grau_instrucao_apos_2005) %>% 
  dplyr::left_join(rais.vinc.cat.09, "tamanho_estabelecimento") %>% 
  dplyr::left_join(rais.vinc.cat.11, "natureza_juridica") %>% 
  saveRDS(file = ("db/rais/vinc/rais_vinc_2015.rds"))

rm(rais.vinc.2015)

gc()

# converting RAIS files to parquet (takes a while!) ----
rds_files_vinc <- fs::dir_ls("db/rais/vinc/", regexp = "rais(.*)\\.rds")

rds_to_arrow <- function(file) {
  name <- stringr::str_sub(file, end = -5)
  
  year <- stringr::str_sub(name, start = -4)
  
  df <- readr::read_rds(file) # %>%
  # dplyr::mutate(ano = year)
  
  arrow::write_parquet(df, glue::glue("{name}.parquet"))
  
  # fs::dir_delete(file)
}

purrr::walk(
  rds_files_vinc,
  rds_to_arrow
)
