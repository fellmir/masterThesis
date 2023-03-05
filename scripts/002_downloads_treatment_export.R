### beginning ----

source("scripts/001_libraries.R")

basedosdados::set_billing_id("abstract-tract-356913")

fs::dir_create("db")
fs::dir_create("db/census")
fs::dir_create("db/rais")
fs::dir_create("db/rais/estab")
fs::dir_create("db/rais/vinc")
fs::dir_create("db/pnad")

id.mun <- "3304557"

### geo for joins ----
area.ponderacao.2000 <- readxl::read_xlsx("inputs/refs/20220409_geo.xlsx", sheet = 3) %>%
  dplyr::select(area_ponderacao = area_ponderacao, nome_area_ponderacao) %>%
  dplyr::mutate(area_ponderacao = as.character(area_ponderacao))

area.ponderacao.2010 <- readxl::read_xlsx("inputs/refs/20220409_geo.xlsx", sheet = 4) %>%
  dplyr::select(area_ponderacao = area_ponderacao_corrigida, nome_area_ponderacao) %>%
  dplyr::mutate(area_ponderacao = as.character(area_ponderacao))

subdistritos.2000 <- readxl::read_xlsx("inputs/refs/20220409_geo.xlsx", sheet = 2) %>%
  dplyr::select(id_subdistrito = id_subdistrito, nome_subdistrito) %>%
  dplyr::mutate(id_subdistrito = as.character(id_subdistrito))

bairros.rj <- readxl::read_xlsx("inputs/refs/20220409_geo.xlsx", sheet = 5) %>%
  dplyr::select(bairros_rj = bairros_rj, nome_bairro) %>%
  dplyr::mutate(bairros_rj = as.character(bairros_rj))

ap.rp.2000 <- readxl::read_xlsx("inputs/refs/20220427_rp_ap_v2.xlsx", sheet = 1) %>%
  dplyr::select(area_ponderacao, AP, RP, area_planejamento, regiao_planejamento) %>%
  dplyr::mutate(area_ponderacao = as.character(area_ponderacao))

ap.rp.2010 <- readxl::read_xlsx("inputs/refs/20220427_rp_ap_v2.xlsx", sheet = 2) %>%
  dplyr::select(
    area_ponderacao = area_ponderacao_corrigida, AP, RP,
    area_planejamento, regiao_planejamento
  ) %>%
  dplyr::mutate(area_ponderacao = as.character(area_ponderacao))

ap.rp.rais <- readxl::read_xlsx("inputs/refs/20220427_rp_ap_v2.xlsx", sheet = 3) %>%
  dplyr::select(bairros_rj, AP, RP, area_planejamento, regiao_planejamento) %>%
  dplyr::mutate(bairros_rj = as.character(bairros_rj))

### Census 2000 - domiciles search ----

# cats for joins
census.2000.doms.cat.01 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 1
) %>%
  dplyr::select(especie_domicilio, cat_especie_domicilio) %>%
  dplyr::mutate(especie_domicilio = as.integer(especie_domicilio)) %>%
  tidyr::drop_na()

census.2000.doms.cat.02 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 1
) %>%
  dplyr::select(tipo_domicilio, cat_tipo_domicilio) %>%
  dplyr::mutate(tipo_domicilio = as.integer(tipo_domicilio)) %>%
  tidyr::drop_na()

census.2000.doms.cat.03 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 1
) %>%
  dplyr::select(microcomputador, cat_microcomputador) %>%
  dplyr::mutate(microcomputador = as.integer(microcomputador)) %>%
  tidyr::drop_na()


# BDD collect
census.2000.doms.cols <- c(
  "v7616",
  "v7617",
  "area_ponderacao",
  "id_subdistrito",
  "v0201",
  "v0202",
  "v0220"
)

db.census.2000.doms <-
  basedosdados::bdplyr("basedosdados.br_ibge_censo_demografico.microdados_domicilio_2000") %>%
  dplyr::filter(id_municipio %in% id.mun) %>%
  dplyr::select(all_of(census.2000.doms.cols)) %>%
  dplyr::mutate(area_ponderacao = as.character(area_ponderacao)) %>%
  dplyr::rename(
    "total_rendimentos_domicilio" = "v7616",
    "total_rendimentos_domicilio_sm" = "v7617",
    "area_ponderacao" = "area_ponderacao",
    "id_subdistrito" = "id_subdistrito",
    "especie_domicilio" = "v0201",
    "tipo_domicilio" = "v0202",
    "microcomputador" = "v0220"
  ) %>%
  dplyr::collect()

# left join
db.census.2000.doms.geo <- db.census.2000.doms %>%
  dplyr::left_join(area.ponderacao.2000, "area_ponderacao") %>%
  dplyr::left_join(subdistritos.2000, "id_subdistrito") %>%
  dplyr::left_join(census.2000.doms.cat.01, "especie_domicilio") %>%
  dplyr::left_join(census.2000.doms.cat.02, "tipo_domicilio") %>%
  dplyr::left_join(census.2000.doms.cat.03, "microcomputador") %>%
  dplyr::left_join(ap.rp.2000, "area_ponderacao") %>%
  dplyr::mutate(ano = 2000) %>%
  dplyr::mutate(post = 0) %>%
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(2, 4) ~ 0,
    TRUE ~ 1
  )) %>%
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(2.1, 2.2, 3.2, 3.7, 4.2) ~ 0,
    TRUE ~ 1
  )) %>%
  mutate(
    range_total_rendimentos_sm = cut(
      total_rendimentos_domicilio_sm,
      c(
        0:15, seq(from = 20, to = 100, by = 10),
        1000, 6000
      )
    )
  )


# saving
saveRDS(db.census.2000.doms.geo, file = "db/census/census_2000_doms.rds")


### Census 2010 - domiciles search ----

# cats for joins
census.2010.doms.cat.01 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 2
) %>%
  dplyr::select(especie_domicilio, cat_especie_domicilio) %>%
  dplyr::mutate(especie_domicilio = as.integer(especie_domicilio)) %>%
  tidyr::drop_na()

census.2010.doms.cat.02 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 2
) %>%
  dplyr::select(tipo_domicilio, cat_tipo_domicilio) %>%
  dplyr::mutate(tipo_domicilio = as.integer(tipo_domicilio)) %>%
  tidyr::drop_na()

census.2010.doms.cat.03 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 2
) %>%
  dplyr::select(microcomputador, cat_microcomputador) %>%
  dplyr::mutate(microcomputador = as.integer(microcomputador)) %>%
  tidyr::drop_na()


# BDD collect
census.2010.doms.cols <- c(
  "v6529",
  "v6530",
  "v6531",
  "v6532",
  "area_ponderacao",
  "v4001",
  "v4002",
  "v0219"
)


db.census.2010.doms <-
  basedosdados::bdplyr("basedosdados.br_ibge_censo_demografico.microdados_domicilio_2010") %>%
  dplyr::filter(id_municipio %in% id.mun) %>%
  dplyr::select(all_of(census.2010.doms.cols)) %>%
  dplyr::mutate(area_ponderacao = as.character(area_ponderacao)) %>%
  dplyr::rename(
    "total_rendimentos_domicilio" = "v6529",
    "total_rendimentos_domicilio_sm" = "v6530",
    "total_rendimentos_domicilio_pc" = "v6531",
    "total_rendimentos_domicilio_pc_sm" = "v6532",
    "area_ponderacao" = "area_ponderacao",
    "especie_domicilio" = "v4001",
    "tipo_domicilio" = "v4002",
    "microcomputador" = "v0219"
  ) %>%
  dplyr::collect()


# left join
db.census.2010.doms.geo <- db.census.2010.doms %>%
  dplyr::left_join(area.ponderacao.2010, "area_ponderacao") %>%
  dplyr::left_join(census.2010.doms.cat.01, "especie_domicilio") %>%
  dplyr::left_join(census.2010.doms.cat.02, "tipo_domicilio") %>%
  dplyr::left_join(census.2010.doms.cat.03, "microcomputador") %>%
  dplyr::left_join(ap.rp.2010, "area_ponderacao") %>%
  dplyr::mutate(ano = 2010) %>%
  dplyr::mutate(post = 1) %>%
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(2, 4) ~ 0,
    TRUE ~ 1
  )) %>%
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(2.1, 2.2, 3.2, 3.7, 4.2) ~ 0,
    TRUE ~ 1
  )) %>%
  mutate(
    range_total_rendimentos_sm = cut(
      total_rendimentos_domicilio_sm,
      c(
        0:15, seq(from = 20, to = 100, by = 10),
        1000, 6000
      )
    )
  )


# saving
saveRDS(db.census.2010.doms.geo, file = "db/census/census_2010_doms.rds")


### Census 2000 - individuals search ----

# cats for joins
census.2000.indv.cat.01 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 3
) %>%
  dplyr::select(trabalhou_julho_2000, cat_trabalhou_julho_2000) %>%
  dplyr::mutate(trabalhou_julho_2000 = as.integer(trabalhou_julho_2000)) %>%
  tidyr::drop_na()

census.2000.indv.cat.02 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 3
) %>%
  dplyr::select(qtd_trabalhos_julho_2000, cat_qtd_trabalhos_julho_2000) %>%
  dplyr::mutate(qtd_trabalhos_julho_2000 = as.integer(qtd_trabalhos_julho_2000)) %>%
  tidyr::drop_na()

census.2000.indv.cat.03 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 3
) %>%
  dplyr::select(qtd_trabalhadores_firma, cat_qtd_trabalhadores_firma) %>%
  dplyr::mutate(qtd_trabalhadores_firma = as.integer(qtd_trabalhadores_firma)) %>%
  tidyr::drop_na()

census.2000.indv.cat.04 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 3
) %>%
  dplyr::select(contribuicao_inss, cat_contribuicao_inss) %>%
  dplyr::mutate(contribuicao_inss = as.integer(contribuicao_inss)) %>%
  tidyr::drop_na()

census.2000.indv.cat.05 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 3
) %>%
  dplyr::select(sexo, cat_sexo) %>%
  dplyr::mutate(sexo = as.integer(sexo)) %>%
  tidyr::drop_na()

census.2000.indv.cat.06 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 3
) %>%
  dplyr::select(cor_raca, cat_cor_raca) %>%
  dplyr::mutate(cor_raca = as.integer(cor_raca)) %>%
  tidyr::drop_na()

census.2000.indv.cat.07 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 3
) %>%
  dplyr::select(serie_aprovacao, cat_serie_aprovacao) %>%
  dplyr::mutate(serie_aprovacao = as.integer(serie_aprovacao)) %>%
  tidyr::drop_na()

census.2000.indv.cat.08 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 3
) %>%
  dplyr::select(concluiu_curso, cat_concluiu_curso) %>%
  dplyr::mutate(concluiu_curso = as.integer(concluiu_curso)) %>%
  tidyr::drop_na()

census.2000.indv.cat.09 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 3
) %>%
  dplyr::select(anos_estudo, cat_anos_estudo) %>%
  dplyr::mutate(anos_estudo = as.integer(anos_estudo)) %>%
  tidyr::drop_na()

census.2000.indv.cat.10 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 3
) %>%
  dplyr::select(situacao_domicilio, cat_situacao_domicilio) %>%
  dplyr::mutate(situacao_domicilio = as.integer(situacao_domicilio)) %>%
  tidyr::drop_na()

census.2000.indv.cat.11 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 3
) %>%
  dplyr::select(cod_antigo_ocupacao, cat_cod_antigo_ocupacao) %>%
  dplyr::mutate(cod_antigo_ocupacao = as.integer(cod_antigo_ocupacao)) %>%
  tidyr::drop_na()

census.2000.indv.cat.12 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 3
) %>%
  dplyr::select(cod_antigo_atividade, cat_cod_antigo_atividade) %>%
  dplyr::mutate(cod_antigo_atividade = as.integer(cod_antigo_atividade)) %>%
  tidyr::drop_na()


# BDD collect
census.2000.indv.cols <- c(
  "v4614",
  "v4615",
  "v4512",
  "v4513",
  "v4514",
  "v4525",
  "v4526",
  "v4573",
  "v0439",
  "v0444",
  "v0449",
  "v0453",
  "v4534",
  "v0450",
  "id_subdistrito",
  "area_ponderacao",
  "v4250",
  "v0401",
  "v0408",
  "v4752",
  "v4754",
  "v0433",
  "v0434",
  "v4355",
  "v4300",
  "v4354",
  "v1006",
  "v4451",
  "v4461"
)


db.census.2000.indv <-
  basedosdados::bdplyr("basedosdados.br_ibge_censo_demografico.microdados_pessoa_2000") %>%
  dplyr::filter(id_municipio %in% id.mun) %>%
  dplyr::select(all_of(census.2000.indv.cols)) %>%
  dplyr::mutate(area_ponderacao = as.character(area_ponderacao)) %>%
  dplyr::rename(
    "total_rendimentos" = "v4614",
    "total_rendimentos_sm" = "v4615",
    "rendimento_bruto_principal" = "v4512",
    "total_rendimentos_principal" = "v4513",
    "total_rendimentos_principal_sm" = "v4514",
    "total_rendimentos_todos_trabalhos" = "v4525",
    "total_rendimentos_todos_trabalhos_sm" = "v4526",
    "rendimento_aposentadoria_pensao" = "v4573",
    "trabalhou_julho_2000" = "v0439",
    "qtd_trabalhos_julho_2000" = "v0444",
    "qtd_trabalhadores_firma" = "v0449",
    "horas_trabalhadas_principal" = "v0453",
    "total_horas_trabalhadas" = "v4534",
    "contribuicao_inss" = "v0450",
    "id_subdistrito" = "id_subdistrito",
    "area_ponderacao" = "area_ponderacao",
    "cod_municipio_residencia" = "v4250",
    "sexo" = "v0401",
    "cor_raca" = "v0408",
    "idade_anos_completos" = "v4752",
    "idade_meses" = "v4754",
    "serie_aprovacao" = "v0433",
    "concluiu_curso" = "v0434",
    "curso_mais_elevado" = "v4355",
    "anos_estudo" = "v4300",
    "curso_mais_elevado_CONCLA" = "v4354",
    "situacao_domicilio" = "v1006",
    "cod_antigo_ocupacao" = "v4451",
    "cod_antigo_atividade" = "v4461"
  ) %>%
  dplyr::collect()


# left join
db.census.2000.indv.geo <- db.census.2000.indv %>%
  dplyr::left_join(area.ponderacao.2000, "area_ponderacao") %>%
  dplyr::left_join(subdistritos.2000, "id_subdistrito") %>%
  dplyr::left_join(census.2000.indv.cat.01, "trabalhou_julho_2000") %>%
  dplyr::left_join(census.2000.indv.cat.02, "qtd_trabalhos_julho_2000") %>%
  dplyr::left_join(census.2000.indv.cat.03, "qtd_trabalhadores_firma") %>%
  dplyr::left_join(census.2000.indv.cat.04, "contribuicao_inss") %>%
  dplyr::left_join(census.2000.indv.cat.05, "sexo") %>%
  dplyr::left_join(census.2000.indv.cat.06, "cor_raca") %>%
  dplyr::left_join(census.2000.indv.cat.07, "serie_aprovacao") %>%
  dplyr::left_join(census.2000.indv.cat.08, "concluiu_curso") %>%
  dplyr::left_join(census.2000.indv.cat.09, "anos_estudo") %>%
  dplyr::left_join(census.2000.indv.cat.10, "situacao_domicilio") %>%
  dplyr::left_join(census.2000.indv.cat.11, "cod_antigo_ocupacao") %>%
  dplyr::left_join(census.2000.indv.cat.12, "cod_antigo_atividade") %>%
  dplyr::left_join(ap.rp.2000, "area_ponderacao") %>%
  dplyr::mutate(ano = 2000) %>%
  dplyr::mutate(post = 0) %>%
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(2, 4) ~ 0,
    TRUE ~ 1
  )) %>%
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(2.1, 2.2, 3.2, 3.7, 4.2) ~ 0,
    TRUE ~ 1
  )) %>%
  dplyr::mutate(race_id = case_when(
    cat_cor_raca %in% "Branca" ~ 0,
    cat_cor_raca %in% "Ignorado" ~ NA_real_,
    TRUE ~ 1
  )) %>%
  mutate(
    range_total_rendimentos_sm = cut(
      total_rendimentos_sm,
      c(
        0:15, seq(from = 20, to = 100, by = 10),
        1000, 6000
      )
    )
  ) %>%
  mutate(
    range_idade = cut(
      idade_anos_completos,
      c(0, 14, 18, 25, 34, 45, 56, 65, 76, 87, 130)
    )
  ) %>%
  rename(cat_trabalhou = cat_trabalhou_julho_2000)


# saving
saveRDS(db.census.2000.indv.geo, file = "db/census/census_2000_indv.rds")


### Census 2010 - individuals search ----

# cats for joins
census.2010.indv.cat.01 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 4
) %>%
  dplyr::select(rendimento_bruto_principal, cat_rendimento_bruto_principal) %>%
  dplyr::mutate(rendimento_bruto_principal = as.integer(rendimento_bruto_principal)) %>%
  tidyr::drop_na()

census.2010.indv.cat.02 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 4
) %>%
  dplyr::select(trabalhou_julho_2010, cat_trabalhou_julho_2010) %>%
  dplyr::mutate(trabalhou_julho_2010 = as.integer(trabalhou_julho_2010)) %>%
  tidyr::drop_na()

census.2010.indv.cat.03 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 4
) %>%
  dplyr::select(qtd_trabalhos_julho_2010, cat_qtd_trabalhos_julho_2010) %>%
  dplyr::mutate(qtd_trabalhos_julho_2010 = as.integer(qtd_trabalhos_julho_2010)) %>%
  tidyr::drop_na()

census.2010.indv.cat.04 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 4
) %>%
  dplyr::select(procurou_trabalho, cat_procurou_trabalho) %>%
  dplyr::mutate(procurou_trabalho = as.integer(procurou_trabalho)) %>%
  tidyr::drop_na()

census.2010.indv.cat.05 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 4
) %>%
  dplyr::select(
    rendimento_aposentadoria_pensao,
    cat_rendimento_aposentadoria_pensao
  ) %>%
  dplyr::mutate(rendimento_aposentadoria_pensao = as.integer(rendimento_aposentadoria_pensao)) %>%
  tidyr::drop_na()

census.2010.indv.cat.06 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 4
) %>%
  dplyr::select(qtd_trabalhadores_firma, cat_qtd_trabalhadores_firma) %>%
  dplyr::mutate(qtd_trabalhadores_firma = as.integer(qtd_trabalhadores_firma)) %>%
  tidyr::drop_na()

census.2010.indv.cat.07 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 4
) %>%
  dplyr::select(contribuicao_inss, cat_contribuicao_inss) %>%
  dplyr::mutate(contribuicao_inss = as.integer(contribuicao_inss)) %>%
  tidyr::drop_na()

census.2010.indv.cat.08 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 4
) %>%
  dplyr::select(municipio_trabalho, cat_municipio_trabalho) %>%
  dplyr::mutate(municipio_trabalho = as.integer(municipio_trabalho)) %>%
  tidyr::drop_na()

census.2010.indv.cat.09 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 4
) %>%
  dplyr::select(sexo, cat_sexo) %>%
  dplyr::mutate(sexo = as.integer(sexo)) %>%
  tidyr::drop_na()

census.2010.indv.cat.10 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 4
) %>%
  dplyr::select(cor_raca, cat_cor_raca) %>%
  dplyr::mutate(cor_raca = as.integer(cor_raca)) %>%
  tidyr::drop_na()

census.2010.indv.cat.11 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 4
) %>%
  dplyr::select(curso_mais_elevado, cat_curso_mais_elevado) %>%
  dplyr::mutate(curso_mais_elevado = as.integer(curso_mais_elevado)) %>%
  tidyr::drop_na()

census.2010.indv.cat.12 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 4
) %>%
  dplyr::select(tipo_curso_mais_elevado, cat_tipo_curso_mais_elevado) %>%
  dplyr::mutate(tipo_curso_mais_elevado = as.integer(tipo_curso_mais_elevado)) %>%
  tidyr::drop_na()

census.2010.indv.cat.13 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 4
) %>%
  dplyr::select(nivel_instrucao, cat_nivel_instrucao) %>%
  dplyr::mutate(nivel_instrucao = as.integer(nivel_instrucao)) %>%
  tidyr::drop_na()

census.2010.indv.cat.14 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 4
) %>%
  dplyr::select(cod_antigo_ocupacao, cat_cod_antigo_ocupacao) %>%
  dplyr::mutate(cod_antigo_ocupacao = as.integer(cod_antigo_ocupacao)) %>%
  tidyr::drop_na()

census.2010.indv.cat.15 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 4
) %>%
  dplyr::select(cod_antigo_atividade, cat_cod_antigo_atividade) %>%
  dplyr::mutate(cod_antigo_atividade = as.integer(cod_antigo_atividade)) %>%
  tidyr::drop_na()

census.2010.indv.cat.16 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 4
) %>%
  dplyr::select(cbo_ocupacao, cat_cbo_ocupacao) %>%
  dplyr::mutate(cbo_ocupacao = as.integer(cbo_ocupacao)) %>%
  tidyr::drop_na()

census.2010.indv.cat.17 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 4
) %>%
  dplyr::select(cnae_atividade, cat_cnae_atividade) %>%
  dplyr::mutate(cnae_atividade = as.integer(cnae_atividade)) %>%
  tidyr::drop_na()


# BDD collect
census.2010.indv.cols <- c(
  "v0651",
  "v6511",
  "v6513",
  "v6514",
  "v6525",
  "v6526",
  "v6527",
  "v6528",
  "v6529",
  "v6530",
  "v6531",
  "v6532",
  "v5070",
  "v5080",
  "v0641",
  "v0645",
  "v0653",
  "v0654",
  "v0656",
  "v0649",
  "v0650",
  "area_ponderacao",
  "v0660",
  "v6604",
  "v0601",
  "v0606",
  "v6036",
  "v0633",
  "v0635",
  "v6400",
  "v6461",
  "v6471",
  "v6462",
  "v6472"
)


db.census.2010.indv <-
  basedosdados::bdplyr("basedosdados.br_ibge_censo_demografico.microdados_pessoa_2010") %>%
  dplyr::filter(id_municipio %in% id.mun) %>%
  dplyr::select(all_of(census.2010.indv.cols)) %>%
  dplyr::mutate(area_ponderacao = as.character(area_ponderacao)) %>%
  dplyr::rename(
    "rendimento_bruto_principal" = "v0651",
    "rendimento_bruto_principal_mensal" = "v6511",
    "total_rendimentos_principal" = "v6513",
    "total_rendimentos_principal_sm" = "v6514",
    "total_rendimentos_todos_trabalhos" = "v6525",
    "total_rendimentos_todos_trabalhos_sm" = "v6526",
    "total_rendimentos_tudo" = "v6527",
    "total_rendimentos_tudo_sm" = "v6528",
    "rendimento_domiciliar" = "v6529",
    "rendimento_domiciliar_sm" = "v6530",
    "rendimento_domiciliar_pc" = "v6531",
    "rendimento_domiciliar_pc_sm" = "v6532",
    "rendimento_familiar_pc" = "v5070",
    "rendimento_familiar_pc_sm" = "v5080",
    "trabalhou_julho_2010" = "v0641",
    "qtd_trabalhos_julho_2010" = "v0645",
    "horas_trabalhadas_principal" = "v0653",
    "procurou_trabalho" = "v0654",
    "rendimento_aposentadoria_pensao" = "v0656",
    "qtd_trabalhadores_firma" = "v0649",
    "contribuicao_inss" = "v0650",
    "area_ponderacao" = "area_ponderacao",
    "municipio_trabalho" = "v0660",
    "cod_municipio_trabalho" = "v6604",
    "sexo" = "v0601",
    "cor_raca" = "v0606",
    "idade_anos_completos" = "v6036",
    "curso_mais_elevado" = "v0633",
    "tipo_curso_mais_elevado" = "v0635",
    "nivel_instrucao" = "v6400",
    "cod_antigo_ocupacao" = "v6461",
    "cod_antigo_atividade" = "v6471",
    "cbo_ocupacao" = "v6462",
    "cnae_atividade" = "v6472"
  ) %>%
  dplyr::collect()


# left join
db.census.2010.indv.geo <- db.census.2010.indv %>%
  dplyr::left_join(area.ponderacao.2010, "area_ponderacao") %>%
  dplyr::left_join(census.2010.indv.cat.01, "rendimento_bruto_principal") %>%
  dplyr::left_join(census.2010.indv.cat.02, "trabalhou_julho_2010") %>%
  dplyr::left_join(census.2010.indv.cat.03, "qtd_trabalhos_julho_2010") %>%
  dplyr::left_join(census.2010.indv.cat.04, "procurou_trabalho") %>%
  dplyr::left_join(census.2010.indv.cat.05, "rendimento_aposentadoria_pensao") %>%
  dplyr::left_join(census.2010.indv.cat.06, "qtd_trabalhadores_firma") %>%
  dplyr::left_join(census.2010.indv.cat.07, "contribuicao_inss") %>%
  dplyr::left_join(census.2010.indv.cat.08, "municipio_trabalho") %>%
  dplyr::left_join(census.2010.indv.cat.09, "sexo") %>%
  dplyr::left_join(census.2010.indv.cat.10, "cor_raca") %>%
  dplyr::left_join(census.2010.indv.cat.11, "curso_mais_elevado") %>%
  dplyr::left_join(census.2010.indv.cat.12, "tipo_curso_mais_elevado") %>%
  dplyr::left_join(census.2010.indv.cat.13, "nivel_instrucao") %>%
  dplyr::left_join(census.2010.indv.cat.14, "cod_antigo_ocupacao") %>%
  dplyr::left_join(census.2010.indv.cat.15, "cod_antigo_atividade") %>%
  dplyr::left_join(census.2010.indv.cat.16, "cbo_ocupacao") %>%
  dplyr::left_join(census.2010.indv.cat.17, "cnae_atividade") %>%
  dplyr::left_join(ap.rp.2010, "area_ponderacao") %>%
  dplyr::mutate(ano = 2010) %>%
  dplyr::mutate(race_id = case_when(
    cat_cor_raca %in% "Branca" ~ 0,
    cat_cor_raca %in% "Ignorado" ~ NA_real_,
    TRUE ~ 1
  )) %>%
  dplyr::mutate(post = 1) %>%
  dplyr::mutate(treat_AP = case_when(
    AP %in% c(2, 4) ~ 0,
    TRUE ~ 1
  )) %>%
  dplyr::mutate(treat_RP = case_when(
    RP %in% c(2.1, 2.2, 3.2, 3.7, 4.2) ~ 0,
    TRUE ~ 1
  )) %>%
  mutate(
    range_total_rendimentos_sm = cut(
      total_rendimentos_tudo_sm,
      c(
        0:15, seq(from = 20, to = 100, by = 10),
        1000, 6000
      )
    )
  ) %>%
  mutate(
    range_idade = cut(
      idade_anos_completos,
      c(0, 14, 18, 25, 34, 45, 56, 65, 76, 87, 130)
    )
  ) %>%
  rename(cat_trabalhou = cat_trabalhou_julho_2010)


# saving
saveRDS(db.census.2010.indv.geo, file = "db/census/census_2010_indv.rds")


### PNAD - domiciles search ----

# cats for joins
pnad.doms.cat.01 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 5
) %>%
  dplyr::select(zona_urbana, cat_zona_urbana) %>%
  dplyr::mutate(zona_urbana = as.character(zona_urbana)) %>%
  tidyr::drop_na()

pnad.doms.cat.02 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 5
) %>%
  dplyr::select(especie_domicilio, cat_especie_domicilio) %>%
  dplyr::mutate(especie_domicilio = as.character(especie_domicilio)) %>%
  tidyr::drop_na()

pnad.doms.cat.03 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 5
) %>%
  dplyr::select(tipo_domicilio, cat_tipo_domicilio) %>%
  dplyr::mutate(tipo_domicilio = as.character(tipo_domicilio)) %>%
  tidyr::drop_na()


# BDD collect
pnad.doms.cols <- c(
  "ano",
  "renda_mensal_domiciliar",
  "renda_mensal_domiciliar_compativel_1992",
  "renda_domicilio_deflacionado",
  "renda_mensal_domiciliar_compativel_1992_deflacionado",
  "deflator",
  "conversor_moeda",
  "zona_urbana",
  "total_pessoas",
  "total_pessoas_10_mais",
  "especie_domicilio",
  "tipo_domicilio"
)


by_year.pnad.doms <- function(i) {
  bdd.link <- "basedosdados.br_ibge_pnad.microdados_compatibilizados_domicilio"
  db.pnad.doms <- basedosdados::bdplyr(bdd.link) %>%
    dplyr::filter(ano == i & id_uf == "33" & regiao_metropolitana == 1) %>%
    dplyr::select(all_of(pnad.doms.cols)) %>%
    dplyr::collect() %>%
    dplyr::left_join(pnad.doms.cat.01, "zona_urbana") %>%
    dplyr::left_join(pnad.doms.cat.02, "especie_domicilio") %>%
    dplyr::left_join(pnad.doms.cat.03, "tipo_domicilio") %>%
    dplyr::mutate(post = case_when(
      ano %in% c(2001, 2002) ~ 0,
      TRUE ~ 1
    )) %>%
    mutate(
      range_total_rendimentos = cut(
        renda_mensal_domiciliar_compativel_1992,
        c(seq(from = 0, to = 29000, by = 1000), 200000)
      )
    )

  # saving
  saveRDS(db.pnad.doms, file = glue::glue("db/pnad/pnad_doms_{i}.rds"))
}


purrr::walk(
  2000:2015,
  by_year.pnad.doms
)


### PNAD - individuals search ----

# cats for joins
pnad.indv.cat.01 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 6
) %>%
  dplyr::select(sexo, cat_sexo) %>%
  dplyr::mutate(sexo = as.character(sexo)) %>%
  tidyr::drop_na()

pnad.indv.cat.02 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 6
) %>%
  dplyr::select(raca_cor, cat_raca_cor) %>%
  dplyr::mutate(raca_cor = as.character(raca_cor)) %>%
  tidyr::drop_na()

pnad.indv.cat.03 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 6
) %>%
  dplyr::select(ultimo_grau_frequentado, cat_ultimo_grau_frequentado) %>%
  dplyr::mutate(ultimo_grau_frequentado = as.character(ultimo_grau_frequentado)) %>%
  tidyr::drop_na()

pnad.indv.cat.04 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 6
) %>%
  dplyr::select(atividade_ramo_negocio_agregado, cat_atividade_ramo_negocio_agregado) %>%
  dplyr::mutate(atividade_ramo_negocio_agregado = as.character(atividade_ramo_negocio_agregado)) %>%
  tidyr::drop_na()

pnad.indv.cat.05 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 6
) %>%
  dplyr::select(atividade_ramo_negocio_semana, cat_atividade_ramo_negocio_semana) %>%
  dplyr::mutate(atividade_ramo_negocio_semana = as.integer(atividade_ramo_negocio_semana)) %>%
  tidyr::drop_na()

pnad.indv.cat.06 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 6
) %>%
  dplyr::select(posicao_ocupacao, cat_posicao_ocupacao) %>%
  dplyr::mutate(posicao_ocupacao = as.character(posicao_ocupacao)) %>%
  tidyr::drop_na()

pnad.indv.cat.07 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 6
) %>%
  dplyr::select(grupos_ocupacao, cat_grupos_ocupacao) %>%
  dplyr::mutate(grupos_ocupacao = as.character(grupos_ocupacao)) %>%
  tidyr::drop_na()

pnad.indv.cat.08 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 6
) %>%
  dplyr::select(condicao_domicilio, cat_condicao_domicilio) %>%
  dplyr::mutate(condicao_domicilio = as.character(condicao_domicilio)) %>%
  tidyr::drop_na()

pnad.indv.cat.09 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 6
) %>%
  dplyr::select(condicao_familia, cat_condicao_familia) %>%
  dplyr::mutate(condicao_familia = as.character(condicao_familia)) %>%
  tidyr::drop_na()


# BDD collect
pnad.indv.cols <- c(
  "ano",
  "renda_mensal_todas_fontes",
  "renda_mensal_dinheiro_deflacionado",
  "renda_mensal_ocupacao_principal_deflacionado",
  "renda_mensal_todas_fontes_deflacionado",
  "renda_aposentadoria_deflacionado",
  "renda_pensao_deflacionado",
  "renda_mensal_familia_deflacionado",
  "sexo",
  "raca_cor",
  "ano_nascimento",
  "idade",
  "ultima_serie_frequentada",
  "ultimo_grau_frequentado",
  "anos_estudo",
  "trabalhou_semana",
  "tinha_trabalhado_semana",
  "tinha_outro_trabalho",
  "tomou_providencia_conseguir_trabalho_semana",
  "tomou_providencia_ultimos_2_meses",
  "possui_carteira_assinada",
  "contribui_previdencia",
  "horas_trabalhadas_semana",
  "horas_trabalhadas_todos_trabalhos",
  "ocupacao_semana",
  "atividade_ramo_negocio_agregado",
  "atividade_ramo_negocio_semana",
  "posicao_ocupacao",
  "grupos_ocupacao",
  "condicao_domicilio",
  "condicao_familia",
  "numero_familia",
  "numero_membros_familia"
)

by_year.pnad.indv <- function(i) {
  bdd.link <- "basedosdados.br_ibge_pnad.microdados_compatibilizados_pessoa"
  db.pnad.indv <- basedosdados::bdplyr(bdd.link) %>%
    dplyr::filter(ano == i & id_uf == "33" & regiao_metropolitana == "1") %>%
    dplyr::select(all_of(pnad.indv.cols)) %>%
    dplyr::collect() %>%
    dplyr::left_join(pnad.indv.cat.01, "sexo") %>%
    dplyr::left_join(pnad.indv.cat.02, "raca_cor") %>%
    dplyr::left_join(pnad.indv.cat.03, "ultimo_grau_frequentado") %>%
    dplyr::left_join(pnad.indv.cat.04, "atividade_ramo_negocio_agregado") %>%
    dplyr::left_join(pnad.indv.cat.05, "atividade_ramo_negocio_semana") %>%
    dplyr::left_join(pnad.indv.cat.06, "posicao_ocupacao") %>%
    dplyr::left_join(pnad.indv.cat.07, "grupos_ocupacao") %>%
    dplyr::left_join(pnad.indv.cat.08, "condicao_domicilio") %>%
    dplyr::left_join(pnad.indv.cat.09, "condicao_familia") %>%
    dplyr::mutate(gender_id = case_when(
      cat_sexo %in% "homem" ~ 0,
      is.na(cat_sexo) ~ NA_real_,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(race_id = case_when(
      cat_raca_cor %in% "branca" ~ 0,
      cat_raca_cor %in% "outro" ~ NA_real_,
      is.na(cat_raca_cor) ~ NA_real_,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(post = case_when(
      ano %in% c(2001, 2002) ~ 0,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(entrep = case_when(
      cat_posicao_ocupacao %in% c("conta propria", "empregador") ~ 1,
      is.na(cat_posicao_ocupacao) ~ NA_real_,
      TRUE ~ 0
    )) %>%
    mutate(
      range_idade = cut(
        idade,
        c(0, 14, 18, 25, 34, 45, 56, 65, 76, 87, 115)
      )
    ) %>%
    mutate(
      range_total_rendimentos = cut(
        renda_mensal_todas_fontes_deflacionado,
        c(seq(from = 0, to = 29000, by = 1000), 200000)
      )
    ) %>%
    mutate(
      range_horas_trabalhadas = cut(
        horas_trabalhadas_todos_trabalhos,
        c(seq(from = 0, to = 120, by = 10))
      )
    )

  # saving
  saveRDS(db.pnad.indv, file = glue::glue("db/pnad/pnad_indv_{i}.rds"))
}


purrr::walk(
  2000:2015,
  by_year.pnad.indv
)

pnad.indv.cols.2015 <- c(
  "ano",
  "renda_mensal_todas_fontes",
  "renda_mensal_dinheiro",
  "renda_mensal_ocupacao_principal",
  "renda_mensal_todas_fontes",
  "renda_aposentadoria",
  "renda_pensao",
  "renda_mensal_familia",
  "sexo",
  "raca_cor",
  "ano_nascimento",
  "idade",
  "ultima_serie_frequentada",
  "ultimo_grau_frequentado",
  "anos_estudo",
  "trabalhou_semana",
  "tinha_trabalhado_semana",
  "tinha_outro_trabalho",
  "tomou_providencia_conseguir_trabalho_semana",
  "tomou_providencia_ultimos_2_meses",
  "possui_carteira_assinada",
  "contribui_previdencia",
  "horas_trabalhadas_semana",
  "horas_trabalhadas_todos_trabalhos",
  "ocupacao_semana",
  "atividade_ramo_negocio_agregado",
  "atividade_ramo_negocio_semana",
  "posicao_ocupacao",
  "grupos_ocupacao",
  "condicao_domicilio",
  "condicao_familia",
  "numero_familia",
  "numero_membros_familia"
)

by_year.pnad.indv.2015 <- function(i) {
  bdd.link <- "basedosdados.br_ibge_pnad.microdados_compatibilizados_pessoa"
  db.pnad.indv <- basedosdados::bdplyr(bdd.link) %>%
    dplyr::filter(ano == i & id_uf == "33" & regiao_metropolitana == "1") %>%
    dplyr::select(all_of(pnad.indv.cols.2015)) %>%
    dplyr::collect() %>%
    dplyr::left_join(pnad.indv.cat.01, "sexo") %>%
    dplyr::left_join(pnad.indv.cat.02, "raca_cor") %>%
    dplyr::left_join(pnad.indv.cat.03, "ultimo_grau_frequentado") %>%
    dplyr::left_join(pnad.indv.cat.04, "atividade_ramo_negocio_agregado") %>%
    dplyr::left_join(pnad.indv.cat.05, "atividade_ramo_negocio_semana") %>%
    dplyr::left_join(pnad.indv.cat.06, "posicao_ocupacao") %>%
    dplyr::left_join(pnad.indv.cat.07, "grupos_ocupacao") %>%
    dplyr::left_join(pnad.indv.cat.08, "condicao_domicilio") %>%
    dplyr::left_join(pnad.indv.cat.09, "condicao_familia") %>%
    dplyr::mutate(gender_id = case_when(
      cat_sexo %in% "homem" ~ 0,
      is.na(cat_sexo) ~ NA_real_,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(race_id = case_when(
      cat_raca_cor %in% "branca" ~ 0,
      cat_raca_cor %in% "outro" ~ NA_real_,
      is.na(cat_raca_cor) ~ NA_real_,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(post = case_when(
      ano %in% c(2001, 2002) ~ 0,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(entrep = case_when(
      cat_posicao_ocupacao %in% c("conta propria", "empregador") ~ 1,
      is.na(cat_posicao_ocupacao) ~ NA_real_,
      TRUE ~ 0
    )) %>%
    mutate(
      range_idade = cut(
        idade,
        c(0, 14, 18, 25, 34, 45, 56, 65, 76, 87, 115)
      )
    ) %>%
    mutate(
      range_total_rendimentos = cut(
        renda_mensal_todas_fontes,
        c(seq(from = 0, to = 29000, by = 1000), 200000)
      )
    ) %>%
    mutate(
      range_horas_trabalhadas = cut(
        horas_trabalhadas_todos_trabalhos,
        c(seq(from = 0, to = 120, by = 10))
      )
    )
  
  # saving
  saveRDS(db.pnad.indv, file = glue::glue("db/pnad/pnad_indv_{i}.rds"))
}

purrr::walk(
  2015,
  by_year.pnad.indv.2015
)


### RAIS - companies search ----

# cats for joins
rais.estab.cat.01 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 8
) %>%
  dplyr::select(tamanho, cat_tamanho) %>%
  dplyr::mutate(tamanho = as.character(tamanho)) %>%
  tidyr::drop_na()

rais.estab.cat.02 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 8
) %>%
  dplyr::select(natureza, cat_natureza) %>%
  dplyr::mutate(natureza = as.character(natureza)) %>%
  tidyr::drop_na()

rais.estab.cat.03 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 8
) %>%
  dplyr::select(natureza_juridica, cat_natureza_juridica) %>%
  dplyr::mutate(natureza_juridica = as.character(natureza_juridica)) %>%
  tidyr::drop_na()

rais.estab.cat.04 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 8
) %>%
  dplyr::select(tipo, cat_tipo) %>%
  dplyr::mutate(tipo = as.character(tipo)) %>%
  tidyr::drop_na()

rais.estab.cat.05 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 8
) %>%
  dplyr::select(cnae_1, cat_cnae_1) %>%
  dplyr::mutate(cnae_1 = as.character(cnae_1)) %>%
  tidyr::drop_na()

rais.estab.cat.06 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 8
) %>%
  dplyr::select(cnae_2, cat_cnae_2) %>%
  dplyr::mutate(cnae_2 = as.character(cnae_2)) %>%
  tidyr::drop_na()

rais.estab.cat.07 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 8
) %>%
  dplyr::select(cnae_2_subclasse, cat_cnae_2_subclasse) %>%
  dplyr::mutate(cnae_2_subclasse = as.character(cnae_2_subclasse)) %>%
  tidyr::drop_na()

rais.estab.cat.08 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 8
) %>%
  dplyr::select(subsetor_ibge, cat_subsetor_ibge) %>%
  dplyr::mutate(subsetor_ibge = as.character(subsetor_ibge)) %>%
  tidyr::drop_na()

rais.estab.cat.09 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 8
) %>%
  dplyr::select(subatividade_ibge, cat_subatividade_ibge) %>%
  dplyr::mutate(subatividade_ibge = as.character(subatividade_ibge)) %>%
  tidyr::drop_na()

rais.estab.cat.10 <- readxl::read_xlsx("inputs/refs/test2.xlsx",
                                          sheet = 1
) %>%
  dplyr::select(tamanho, cat_tamanho) %>%
  dplyr::mutate(tamanho = as.character(tamanho)) %>%
  tidyr::drop_na()


# BDD collect
rais.companies.cols <- c(
  "ano",
  "quantidade_vinculos_ativos",
  "quantidade_vinculos_clt",
  "quantidade_vinculos_estatutarios",
  "tamanho",
  "cep",
  "bairros_rj",
  "natureza",
  "natureza_juridica",
  "tipo",
  "indicador_cei_vinculado",
  "indicador_simples",
  "indicador_rais_negativa",
  "indicador_atividade_ano",
  "cnae_1",
  "cnae_2",
  "cnae_2_subclasse",
  "subsetor_ibge",
  "subatividade_ibge"
)


by_year.rais.estab.2000.2001 <- function(i) {
  bdd.link <- "br_me_rais.microdados_estabelecimentos"
  db.rais.estab <- basedosdados::bdplyr(bdd.link) %>%
    dplyr::filter(ano == i & id_municipio %in% id.mun) %>%
    dplyr::select(all_of(rais.companies.cols)) %>%
    dplyr::mutate(bairros_rj = as.numeric(bairros_rj)) %>%
    dplyr::mutate(bairros_rj = as.character(bairros_rj)) %>%
    dplyr::mutate(bairros_rj = paste0(bairros_rj, "0")) %>%
    dplyr::collect() %>%
    dplyr::left_join(bairros.rj, "bairros_rj") %>%
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
    dplyr::left_join(rais.estab.cat.02, "natureza") %>%
    dplyr::left_join(rais.estab.cat.03, "natureza_juridica") %>%
    dplyr::left_join(rais.estab.cat.04, "tipo") %>%
    dplyr::left_join(rais.estab.cat.05, "cnae_1") %>%
    dplyr::left_join(rais.estab.cat.06, "cnae_2") %>%
    dplyr::left_join(rais.estab.cat.07, "cnae_2_subclasse") %>%
    dplyr::left_join(rais.estab.cat.08, "subsetor_ibge") %>%
    dplyr::left_join(rais.estab.cat.09, "subatividade_ibge") %>%
    dplyr::left_join(ap.rp.rais, "bairros_rj") %>%
    dplyr::mutate(post = case_when(
      ano %in% c(2000, 2001, 2002) ~ 0,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(treat_AP = case_when(
      AP %in% c(2, 4) ~ 0,
      AP == NA ~ NA_real_,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(treat_RP = case_when(
      RP %in% c(2.1, 2.2, 3.2, 3.7, 4.2) ~ 0,
      RP == NA ~ NA_real_,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(cnae_id = as.numeric(cnae_1))



  # saving
  saveRDS(db.rais.estab, file = glue::glue("db/rais/estab/rais_estab_{i}.rds"))
}


purrr::walk(
  2000:2001,
  by_year.rais.estab.2000.2001
)

by_year.rais.estab.2002.2015 <- function(i) {
  bdd.link <- "br_me_rais.microdados_estabelecimentos"
  db.rais.estab <- basedosdados::bdplyr(bdd.link) %>%
    dplyr::filter(ano == i & id_municipio %in% id.mun) %>%
    dplyr::select(all_of(rais.companies.cols)) %>%
    dplyr::mutate(bairros_rj = as.numeric(bairros_rj)) %>%
    dplyr::mutate(bairros_rj = as.character(bairros_rj)) %>%
    dplyr::collect() %>%
    dplyr::left_join(bairros.rj, "bairros_rj") %>%
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
    dplyr::left_join(rais.estab.cat.02, "natureza") %>%
    dplyr::left_join(rais.estab.cat.03, "natureza_juridica") %>%
    dplyr::left_join(rais.estab.cat.04, "tipo") %>%
    dplyr::left_join(rais.estab.cat.05, "cnae_1") %>%
    dplyr::left_join(rais.estab.cat.06, "cnae_2") %>%
    dplyr::left_join(rais.estab.cat.07, "cnae_2_subclasse") %>%
    dplyr::left_join(rais.estab.cat.08, "subsetor_ibge") %>%
    dplyr::left_join(rais.estab.cat.09, "subatividade_ibge") %>%
    dplyr::left_join(ap.rp.rais, "bairros_rj") %>%
    dplyr::mutate(post = case_when(
      ano %in% c(2000, 2001, 2002) ~ 0,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(treat_AP = case_when(
      AP %in% c(2, 4) ~ 0,
      AP == NA ~ NA_real_,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(treat_RP = case_when(
      RP %in% c(2.1, 2.2, 3.2, 3.7, 4.2) ~ 0,
      RP == NA ~ NA_real_,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(id_cnae = as.numeric(cnae_1))



  # saving
  saveRDS(db.rais.estab, file = glue::glue("db/rais/estab/rais_estab_{i}.rds"))
}

purrr::walk(
  2002:2015,
  by_year.rais.estab.2002.2015
)

### RAIS - workers search ----

# cats for joins
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

rais.vinc.cat.10 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 7
) %>%
  dplyr::select(tipo_estabelecimento, cat_tipo_estabelecimento) %>%
  dplyr::mutate(tipo_estabelecimento = as.character(tipo_estabelecimento)) %>%
  tidyr::drop_na()

rais.vinc.cat.11 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 7
) %>%
  dplyr::select(natureza_juridica, cat_natureza_juridica) %>%
  dplyr::mutate(natureza_juridica = as.character(natureza_juridica)) %>%
  tidyr::drop_na()

rais.vinc.cat.12 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 7
) %>%
  dplyr::select(subatividade_ibge, cat_subatividade_ibge) %>%
  dplyr::mutate(subatividade_ibge = as.character(subatividade_ibge)) %>%
  tidyr::drop_na()

rais.vinc.cat.13 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 7
) %>%
  dplyr::select(subsetor_ibge, cat_subsetor_ibge) %>%
  dplyr::mutate(subsetor_ibge = as.character(subsetor_ibge)) %>%
  tidyr::drop_na()

rais.vinc.cat.14 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 7
) %>%
  dplyr::select(cbo_1994, cat_cbo_1994) %>%
  dplyr::mutate(cbo_1994 = as.character(cbo_1994)) %>%
  tidyr::drop_na()

rais.vinc.cat.15 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 7
) %>%
  dplyr::select(cbo_2002, cat_cbo_2002) %>%
  dplyr::mutate(cbo_2002 = as.character(cbo_2002)) %>%
  tidyr::drop_na()

rais.vinc.cat.16 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 7
) %>%
  dplyr::select(cnae_1, cat_cnae_1) %>%
  dplyr::mutate(cnae_1 = as.character(cnae_1)) %>%
  tidyr::drop_na()

rais.vinc.cat.17 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 7
) %>%
  dplyr::select(cnae_2, cat_cnae_2) %>%
  dplyr::mutate(cnae_2 = as.character(cnae_2)) %>%
  tidyr::drop_na()

rais.vinc.cat.18 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 7
) %>%
  dplyr::select(cnae_2_subclasse, cat_cnae_2_subclasse) %>%
  dplyr::mutate(cnae_2_subclasse = as.character(cnae_2_subclasse)) %>%
  tidyr::drop_na()

rais.vinc.cat.19 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 7
) %>%
  dplyr::select(tipo_salario, cat_tipo_salario) %>%
  dplyr::mutate(tipo_salario = as.character(tipo_salario)) %>%
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

rais.vinc.cat.22 <- readxl::read_xlsx("inputs/refs/test.xlsx",
  sheet = 7
) %>%
  dplyr::select(nacionalidade, cat_nacionalidade) %>%
  dplyr::mutate(nacionalidade = as.character(nacionalidade)) %>%
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


# BDD collect
rais.workers.cols <- c(
  "ano",
  "faixa_remuneracao_media_sm",
  "valor_remuneracao_media_sm",
  "valor_remuneracao_media",
  "faixa_remuneracao_dezembro_sm",
  "valor_remuneracao_dezembro_sm",
  "valor_remuneracao_dezembro",
  "indicador_cei_vinculado",
  "mes_admissao",
  "tipo_admissao",
  "vinculo_ativo_3112",
  "mes_desligamento",
  "motivo_desligamento",
  "faixa_tempo_emprego",
  "tempo_emprego",
  "faixa_horas_contratadas",
  "quantidade_horas_contratadas",
  "bairros_rj",
  "sexo",
  "raca_cor",
  "nacionalidade",
  "faixa_etaria",
  "idade",
  "grau_instrucao_1985_2005",
  "grau_instrucao_apos_2005",
  "tipo_vinculo",
  "tamanho_estabelecimento",
  "tipo_estabelecimento",
  "natureza_juridica",
  "subatividade_ibge",
  "subsetor_ibge",
  "cbo_1994",
  "cbo_2002",
  "cnae_1",
  "cnae_2",
  "cnae_2_subclasse"
)


by_year.rais.vinc.2000.2005 <- function(i) {
  bdd.link <- "basedosdados.br_me_rais.microdados_vinculos"
  db.rais.vinc <- basedosdados::bdplyr(bdd.link) %>%
    dplyr::filter(ano == i & id_municipio %in% id.mun) %>%
    dplyr::select(all_of(rais.workers.cols)) %>%
    dplyr::mutate(bairros_rj = as.numeric(bairros_rj)) %>%
    dplyr::mutate(bairros_rj = as.character(bairros_rj)) %>%
    dplyr::collect() %>%
    dplyr::left_join(bairros.rj, "bairros_rj") %>%
    dplyr::left_join(rais.vinc.cat.01, "faixa_remuneracao_media_sm") %>%
    dplyr::left_join(rais.vinc.cat.24, "faixa_remuneracao_dezembro_sm") %>%
    dplyr::left_join(rais.vinc.cat.20, "tipo_admissao") %>%
    dplyr::left_join(rais.vinc.cat.21, "motivo_desligamento") %>%
    dplyr::left_join(rais.vinc.cat.23, "faixa_tempo_emprego") %>%
    dplyr::left_join(rais.vinc.cat.02, "faixa_horas_contratadas") %>%
    dplyr::left_join(rais.vinc.cat.03, "sexo") %>%
    dplyr::left_join(rais.vinc.cat.04, "raca_cor") %>%
    dplyr::left_join(rais.vinc.cat.22, "nacionalidade") %>%
    dplyr::left_join(rais.vinc.cat.05, "faixa_etaria") %>%
    dplyr::left_join(rais.vinc.cat.06, "grau_instrucao_1985_2005") %>%
    dplyr::left_join(rais.vinc.cat.07, "grau_instrucao_apos_2005") %>%
    dplyr::left_join(rais.vinc.cat.08, "tipo_vinculo") %>%
    dplyr::left_join(rais.vinc.cat.09, "tamanho_estabelecimento") %>%
    dplyr::left_join(rais.vinc.cat.10, "tipo_estabelecimento") %>%
    dplyr::left_join(rais.vinc.cat.11, "natureza_juridica") %>%
    dplyr::left_join(rais.vinc.cat.12, "subatividade_ibge") %>%
    dplyr::left_join(rais.vinc.cat.13, "subsetor_ibge") %>%
    dplyr::left_join(rais.vinc.cat.14, "cbo_1994") %>%
    dplyr::left_join(rais.vinc.cat.15, "cbo_2002") %>%
    dplyr::left_join(rais.vinc.cat.16, "cnae_1") %>%
    dplyr::left_join(rais.vinc.cat.17, "cnae_2") %>%
    dplyr::left_join(rais.vinc.cat.18, "cnae_2_subclasse") %>%
    dplyr::left_join(ap.rp.rais, "bairros_rj") %>%
    dplyr::mutate(post = case_when(
      ano %in% c(2000, 2001, 2002) ~ 0,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(treat_AP = case_when(
      AP %in% c(2, 4) ~ 0,
      AP == NA ~ NA_real_,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(treat_RP = case_when(
      RP %in% c(2.1, 2.2, 3.2, 3.7, 4.2) ~ 0,
      RP == NA ~ NA_real_,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(race_id = NA) %>%
    dplyr::mutate(public_work = case_when(
      cat_tipo_vinculo %in% c("ESTAT N/EFET", "ESTATUTARIO") ~ 1,
      TRUE ~ 0
    )) %>%
    dplyr::mutate(id_cnae = as.numeric(cnae_1)) %>%
    dplyr::mutate(id_educ = grau_instrucao_1985_2005) %>%
    dplyr::mutate(cat_educ = cat_grau_instrucao_1985_2005) %>%
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
    dplyr::mutate(first_job = case_when(
      cat_tipo_admissao == "PRIM EMPREGO" ~ 1,
      TRUE ~ 0
    )) %>%
    dplyr::mutate(gender_id = case_when(
      cat_sexo == "feminino" ~ 1,
      TRUE ~ 0
    ))


  # saving
  saveRDS(db.rais.vinc, file = glue::glue("db/rais/vinc/rais_vinc_{i}.rds"))
}


purrr::walk(
  2000:2005,
  by_year.rais.vinc.2000.2005
)

by_year.rais.vinc.2006.2015 <- function(i) {
  bdd.link <- "basedosdados.br_me_rais.microdados_vinculos"
  db.rais.vinc <- basedosdados::bdplyr(bdd.link) %>%
    dplyr::filter(ano == i & id_municipio %in% id.mun) %>%
    dplyr::select(all_of(rais.workers.cols)) %>%
    dplyr::mutate(bairros_rj = as.numeric(bairros_rj)) %>%
    dplyr::mutate(bairros_rj = as.character(bairros_rj)) %>%
    dplyr::collect() %>%
    dplyr::left_join(bairros.rj, "bairros_rj") %>%
    dplyr::left_join(rais.vinc.cat.01, "faixa_remuneracao_media_sm") %>%
    dplyr::left_join(rais.vinc.cat.24, "faixa_remuneracao_dezembro_sm") %>%
    dplyr::left_join(rais.vinc.cat.20, "tipo_admissao") %>%
    dplyr::left_join(rais.vinc.cat.21, "motivo_desligamento") %>%
    dplyr::left_join(rais.vinc.cat.23, "faixa_tempo_emprego") %>%
    dplyr::left_join(rais.vinc.cat.02, "faixa_horas_contratadas") %>%
    dplyr::left_join(rais.vinc.cat.03, "sexo") %>%
    dplyr::left_join(rais.vinc.cat.04, "raca_cor") %>%
    dplyr::left_join(rais.vinc.cat.22, "nacionalidade") %>%
    dplyr::left_join(rais.vinc.cat.05, "faixa_etaria") %>%
    dplyr::left_join(rais.vinc.cat.06, "grau_instrucao_1985_2005") %>%
    dplyr::left_join(rais.vinc.cat.07, "grau_instrucao_apos_2005") %>%
    dplyr::left_join(rais.vinc.cat.08, "tipo_vinculo") %>%
    dplyr::left_join(rais.vinc.cat.09, "tamanho_estabelecimento") %>%
    dplyr::left_join(rais.vinc.cat.10, "tipo_estabelecimento") %>%
    dplyr::left_join(rais.vinc.cat.11, "natureza_juridica") %>%
    dplyr::left_join(rais.vinc.cat.12, "subatividade_ibge") %>%
    dplyr::left_join(rais.vinc.cat.13, "subsetor_ibge") %>%
    dplyr::left_join(rais.vinc.cat.14, "cbo_1994") %>%
    dplyr::left_join(rais.vinc.cat.15, "cbo_2002") %>%
    dplyr::left_join(rais.vinc.cat.16, "cnae_1") %>%
    dplyr::left_join(rais.vinc.cat.17, "cnae_2") %>%
    dplyr::left_join(rais.vinc.cat.18, "cnae_2_subclasse") %>%
    dplyr::left_join(ap.rp.rais, "bairros_rj") %>%
    dplyr::mutate(post = case_when(
      ano %in% c(2000, 2001, 2002) ~ 0,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(treat_AP = case_when(
      AP %in% c(2, 4) ~ 0,
      AP == NA ~ NA_real_,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(treat_RP = case_when(
      RP %in% c(2.1, 2.2, 3.2, 3.7, 4.2) ~ 0,
      RP == NA ~ NA_real_,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(race_id = case_when(
      cat_raca_cor %in% "Branca" ~ 0,
      cat_raca_cor %in% "Nao identificado" ~ NA_real_,
      is.na(cat_raca_cor) ~ NA_real_,
      TRUE ~ 1
    )) %>%
    dplyr::mutate(public_work = case_when(
      cat_tipo_vinculo %in% c("ESTAT N/EFET", "ESTATUTARIO", "ESTAT RGPS") ~ 1,
      TRUE ~ 0
    )) %>%
    dplyr::mutate(id_cnae = as.numeric(cnae_1)) %>%
    dplyr::mutate(id_educ = grau_instrucao_apos_2005) %>%
    dplyr::mutate(cat_educ = cat_grau_instrucao_apos_2005) %>%
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
    dplyr::mutate(first_job = case_when(
      cat_tipo_admissao == "PRIM EMPREGO" ~ 1,
      TRUE ~ 0
    )) %>%
    dplyr::mutate(gender_id = case_when(
      cat_sexo == "feminino" ~ 1,
      TRUE ~ 0
    ))


  # saving
  saveRDS(db.rais.vinc, file = glue::glue("db/rais/vinc/rais_vinc_{i}.rds"))
}

purrr::walk(
  2006:2010,
  by_year.rais.vinc.2006.2015
)

purrr::walk(
  2011:2015,
  by_year.rais.vinc.2006.2015
)

# converting RAIS files to parquet (takes a while!) ----
rds_files_estab <- fs::dir_ls("db/rais/estab/", regexp = "rais(.*)\\.rds")

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
  rds_files_estab,
  rds_to_arrow
)

purrr::walk(
  rds_files_vinc,
  rds_to_arrow
)
