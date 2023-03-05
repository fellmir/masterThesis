### beginning ----

source("scripts/001_libraries.R")

fs::dir_create("db/rais/estab/mod")
fs::dir_create("db/rais/estab/og")
fs::dir_create("db/rais/vinc/mod")
fs::dir_create("db/rais/vinc/og")

### mice for RAIS estab ----

## 2000 - 2005 ----

# 2000
rais.estab.2000 <- readRDS("db/rais/estab/og/rais_estab_2000.rds")

rais.estab.test <- rais.estab.2000 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "id",
      "post")) | ends_with("planejamento")))

rm(rais.estab.2000)

qp <- mice::quickpred(rais.estab.test, exclude = c(
  "quantidade_vinculos_clt",
  "quantidade_vinculos_estatutarios",
  "tamanho"
))

gc()

inputed.estab.2000 <- mice::mice(
  rais.estab.test,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "pmm", "pmm")
)

gc()

completed.estab.2000 <- mice::complete(inputed.estab.2000) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/estab/mod/rais_estab_2000_mod_v2.rds")

rm(inputed.estab.2000, completed.estab.2000, rais.estab.test, qp)

gc()


# 2001
rais.estab.2001 <- readRDS("db/rais/estab/og/rais_estab_2001.rds")

rais.estab.test <- rais.estab.2001 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "id",
      "post")) | ends_with("planejamento")))

rm(rais.estab.2001)

qp <- mice::quickpred(rais.estab.test, exclude = c(
  "quantidade_vinculos_clt",
  "quantidade_vinculos_estatutarios",
  "tamanho"
))

gc()

inputed.estab.2001 <- mice::mice(
  rais.estab.test,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "pmm", "pmm")
)

gc()

completed.estab.2001 <- mice::complete(inputed.estab.2001) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/estab/mod/rais_estab_2001_mod_v2.rds")

rm(inputed.estab.2001, completed.estab.2001, rais.estab.test, qp)

gc()


# 2002
rais.estab.rds <- lapply(2001:2002, function(x) readRDS((paste0("db/rais/estab/og/rais_estab_", x, ".rds"))))

rais.estab.rds <- do.call(rbind, rais.estab.rds)

rais.estab.test <- rais.estab.rds %>%
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "id",
      "post")) | ends_with("planejamento")))

rm(rais.estab.rds)

qp <- mice::quickpred(rais.estab.test, exclude = c(
  "quantidade_vinculos_clt",
  "quantidade_vinculos_estatutarios",
  "tamanho"
))

gc()

inputed.estab.2002 <- mice::mice(
  rais.estab.test,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "pmm", "pmm")
)

gc()

completed.estab.2002 <- mice::complete(inputed.estab.2002) %>%
  tibble::as_tibble() %>% 
  dplyr::filter(ano == 2002) %>% 
  saveRDS(file = "db/rais/estab/mod/rais_estab_2002_mod_v2.rds")

rm(inputed.estab.2002, completed.estab.2002, rais.estab.test, qp)

gc()


# 2003
rais.estab.2003 <- readRDS("db/rais/estab/og/rais_estab_2003.rds")

rais.estab.test <- rais.estab.2003 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "id",
      "post")) | ends_with("planejamento")))

rm(rais.estab.2003)

# qp <- mice::quickpred(rais.estab.test, exclude = c(
#   "quantidade_vinculos_clt",
#   "quantidade_vinculos_estatutarios",
#   "tamanho"
# ))

gc()

inputed.estab.2003 <- mice::mice(
  rais.estab.test,
  m = 5,
  maxit = 10,
  # predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "pmm", "pmm")
)

gc()

completed.estab.2003 <- mice::complete(inputed.estab.2003) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/estab/mod/rais_estab_2003_mod_v2.rds")

rm(inputed.estab.2003, completed.estab.2003, rais.estab.test, qp)

gc()


# 2004
rais.estab.2004 <- readRDS("db/rais/estab/og/rais_estab_2004.rds")

rais.estab.test <- rais.estab.2004 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "id",
      "post")) | ends_with("planejamento")))

rm(rais.estab.2004)

# qp <- mice::quickpred(rais.estab.test, exclude = c(
#   "quantidade_vinculos_clt",
#   "quantidade_vinculos_estatutarios",
#   "tamanho"
# ))

gc()

inputed.estab.2004 <- mice::mice(
  rais.estab.test,
  m = 5,
  maxit = 10,
  # predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "pmm", "pmm")
)

gc()

completed.estab.2004 <- mice::complete(inputed.estab.2004) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/estab/mod/rais_estab_2004_mod_v2.rds")

rm(inputed.estab.2004, completed.estab.2004, rais.estab.test)

gc()


# 2005
rais.estab.2005 <- readRDS("db/rais/estab/og/rais_estab_2005.rds")

rais.estab.test <- rais.estab.2005 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "id",
      "post")) | ends_with("planejamento")))

rm(rais.estab.2005)

# qp <- mice::quickpred(rais.estab.test, exclude = c(
#   "quantidade_vinculos_clt",
#   "quantidade_vinculos_estatutarios",
#   "tamanho"
# ))

gc()

inputed.estab.2005 <- mice::mice(
  rais.estab.test,
  m = 5,
  maxit = 10,
  # predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "pmm", "pmm")
)

gc()

completed.estab.2005 <- mice::complete(inputed.estab.2005) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/estab/mod/rais_estab_2005_mod_v2.rds")

rm(inputed.estab.2005, completed.estab.2005, rais.estab.test)

gc()

## 2006 - 2010 ----

# 2006
rais.estab.2006 <- readRDS("db/rais/estab/og/rais_estab_2006.rds")

rais.estab.test <- rais.estab.2006 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "id",
      "post")) | ends_with("planejamento")))

rm(rais.estab.2006)

# qp <- mice::quickpred(rais.estab.test, exclude = c(
#   "quantidade_vinculos_clt",
#   "quantidade_vinculos_estatutarios",
#   "tamanho"
# ))

gc()

inputed.estab.2006 <- mice::mice(
  rais.estab.test,
  m = 5,
  maxit = 10,
  # predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "pmm", "pmm")
)

gc()

completed.estab.2006 <- mice::complete(inputed.estab.2006) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/estab/mod/rais_estab_2006_mod_v2.rds")

rm(inputed.estab.2006, completed.estab.2006, rais.estab.test)

gc()


# 2007
rais.estab.2007 <- readRDS("db/rais/estab/og/rais_estab_2007.rds")

rais.estab.test <- rais.estab.2007 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "id",
      "post")) | ends_with("planejamento")))

rm(rais.estab.2007)

# qp <- mice::quickpred(rais.estab.test, exclude = c(
#   "quantidade_vinculos_clt",
#   "quantidade_vinculos_estatutarios",
#   "tamanho"
# ))

gc()

inputed.estab.2007 <- mice::mice(
  rais.estab.test,
  m = 5,
  maxit = 10,
  # predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "pmm", "pmm")
)

gc()

completed.estab.2007 <- mice::complete(inputed.estab.2007) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/estab/mod/rais_estab_2007_mod_v2.rds")

rm(inputed.estab.2007, completed.estab.2007, rais.estab.test)

gc()


# 2008
rais.estab.2008 <- readRDS("db/rais/estab/og/rais_estab_2008.rds")

rais.estab.test <- rais.estab.2008 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "id",
      "post")) | ends_with("planejamento")))

rm(rais.estab.2008)

# qp <- mice::quickpred(rais.estab.test, exclude = c(
#   "quantidade_vinculos_clt",
#   "quantidade_vinculos_estatutarios",
#   "tamanho"
# ))

gc()

inputed.estab.2008 <- mice::mice(
  rais.estab.test,
  m = 5,
  maxit = 10,
  # predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "pmm", "pmm")
)

gc()

completed.estab.2008 <- mice::complete(inputed.estab.2008) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/estab/mod/rais_estab_2008_mod_v2.rds")

rm(inputed.estab.2008, completed.estab.2008, rais.estab.test)

gc()


# 2009
rais.estab.2009 <- readRDS("db/rais/estab/og/rais_estab_2009.rds")

rais.estab.test <- rais.estab.2009 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "id",
      "post")) | ends_with("planejamento")))

rm(rais.estab.2009)

# qp <- mice::quickpred(rais.estab.test, exclude = c(
#   "quantidade_vinculos_clt",
#   "quantidade_vinculos_estatutarios",
#   "tamanho"
# ))

gc()

inputed.estab.2009 <- mice::mice(
  rais.estab.test,
  m = 5,
  maxit = 10,
  # predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "pmm", "pmm")
)


gc()

completed.estab.2009 <- mice::complete(inputed.estab.2009) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/estab/mod/rais_estab_2009_mod_v2.rds")

rm(inputed.estab.2009, completed.estab.2009, rais.estab.test)

gc()


# 2010
rais.estab.2010 <- readRDS("db/rais/estab/og/rais_estab_2010.rds")

rais.estab.test <- rais.estab.2010 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "id",
      "post")) | ends_with("planejamento")))

rm(rais.estab.2010)

# qp <- mice::quickpred(rais.estab.test, exclude = c(
#   "quantidade_vinculos_clt",
#   "quantidade_vinculos_estatutarios",
#   "tamanho"
# ))

gc()

inputed.estab.2010 <- mice::mice(
  rais.estab.test,
  m = 5,
  maxit = 10,
  # predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "pmm", "pmm")
)

gc()

completed.estab.2010 <- mice::complete(inputed.estab.2010) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/estab/mod/rais_estab_2010_mod_v2.rds")

rm(inputed.estab.2010, completed.estab.2010, rais.estab.test)

gc()

## 2011 - 2015 ----

# 2011
rais.estab.2011 <- readRDS("db/rais/estab/og/rais_estab_2011.rds")

rais.estab.test <- rais.estab.2011 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "id",
      "post")) | ends_with("planejamento")))

rm(rais.estab.2011)

# qp <- mice::quickpred(rais.estab.test, exclude = c(
#   "quantidade_vinculos_clt",
#   "quantidade_vinculos_estatutarios",
#   "tamanho"
# ))

gc()

inputed.estab.2011 <- mice::mice(
  rais.estab.test,
  m = 5,
  maxit = 10,
  # predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "pmm", "pmm")
)

gc()

completed.estab.2011 <- mice::complete(inputed.estab.2011) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/estab/mod/rais_estab_2011_mod_v2.rds")

rm(inputed.estab.2011, completed.estab.2011, rais.estab.test)

gc()


# 2012
rais.estab.2012 <- readRDS("db/rais/estab/og/rais_estab_2012.rds")

rais.estab.test <- rais.estab.2012 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "id",
      "post")) | ends_with("planejamento")))

rm(rais.estab.2012)

# qp <- mice::quickpred(rais.estab.test, exclude = c(
#   "quantidade_vinculos_clt",
#   "quantidade_vinculos_estatutarios",
#   "tamanho"
# ))

gc()

inputed.estab.2012 <- mice::mice(
  rais.estab.test,
  m = 5,
  maxit = 10,
  # predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "pmm", "pmm")
)

gc()

completed.estab.2012 <- mice::complete(inputed.estab.2012) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/estab/mod/rais_estab_2012_mod_v2.rds")

rm(inputed.estab.2012, completed.estab.2012, rais.estab.test)

gc()


# 2013
rais.estab.2013 <- readRDS("db/rais/estab/og/rais_estab_2013.rds")

rais.estab.test <- rais.estab.2013 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "id",
      "post")) | ends_with("planejamento")))

rm(rais.estab.2013)

# qp <- mice::quickpred(rais.estab.test, exclude = c(
#   "quantidade_vinculos_clt",
#   "quantidade_vinculos_estatutarios",
#   "tamanho"
# ))

gc()

inputed.estab.2013 <- mice::mice(
  rais.estab.test,
  m = 5,
  maxit = 10,
  # predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "pmm", "pmm")
)

gc()

completed.estab.2013 <- mice::complete(inputed.estab.2013) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/estab/mod/rais_estab_2013_mod_v2.rds")

rm(inputed.estab.2013, completed.estab.2013, rais.estab.test)

gc()


# 2014
rais.estab.2014 <- readRDS("db/rais/estab/og/rais_estab_2014.rds")

rais.estab.test <- rais.estab.2014 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "id",
      "post")) | ends_with("planejamento")))

rm(rais.estab.2014)

# qp <- mice::quickpred(rais.estab.test, exclude = c(
#   "quantidade_vinculos_clt",
#   "quantidade_vinculos_estatutarios",
#   "tamanho"
# ))

gc()

inputed.estab.2014 <- mice::mice(
  rais.estab.test,
  m = 5,
  maxit = 10,
  # predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "pmm", "pmm")
)

gc()

completed.estab.2014 <- mice::complete(inputed.estab.2014) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/estab/mod/rais_estab_2014_mod_v2.rds")

rm(inputed.estab.2014, completed.estab.2014, rais.estab.test)

gc()


# 2015
rais.estab.2015 <- readRDS("db/rais/estab/og/rais_estab_2015.rds")

rais.estab.test <- rais.estab.2015 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "id",
      "post")) | ends_with("planejamento")))

rm(rais.estab.2015)

# qp <- mice::quickpred(rais.estab.test, exclude = c(
#   "quantidade_vinculos_clt",
#   "quantidade_vinculos_estatutarios",
#   "tamanho"
# ))

gc()

inputed.estab.2015 <- mice::mice(
  rais.estab.test,
  m = 5,
  maxit = 10,
  # predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "pmm", "pmm")
)

gc()

completed.estab.2015 <- mice::complete(inputed.estab.2015) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/estab/mod/rais_estab_2015_mod_v2.rds")

rm(inputed.estab.2015, completed.estab.2015, rais.estab.test)

gc()

### mice for RAIS vinc ----

## 2006 - 2010 ----

# 2006

rais.vinc.2006 <- readRDS("db/rais/vinc/og/rais_vinc_2006.rds")

rais.vinc.test <- rais.vinc.2006 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "post",
      "gender")) | ends_with(c("planejamento",
                               "work",
                               "job")))) %>% 
  dplyr::select(-c(
    "id_cnae",
    "id_educ"
  ))
# %>% 
# dplyr::mutate(
#   race_id = as.factor(race_id)
# )

rm(rais.vinc.2006)

qp <- mice::quickpred(rais.vinc.test, exclude = c(
  "ano",
  "valor_remuneracao_media_sm",
  "valor_remuneracao_dezembro_sm",
  "tempo_emprego",
  "quantidade_horas_contratadas",
  "idade",
  "grau_instrucao_1985_2005"
))

gc()

inputed.vinc.2006 <- mice::mice(
  rais.vinc.test,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
             "", "", "", "", "", "", "", "", "", "", "", "", "pmm", "pmm", "pmm")
)

gc()

completed.vinc.2006 <- mice::complete(inputed.vinc.2006) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/vinc/mod/rais_vinc_2006_mod_v2.rds")

rm(inputed.vinc.2006, completed.vinc.2006, rais.vinc.test, qp)

gc()

# 2007
# rais.vinc.2007 <- readRDS("db/rais/vinc/og/rais_vinc_2007.rds")
# 
# rais.vinc.test <- rais.vinc.2007 %>% 
#   dplyr::select(-(
#     starts_with(c(
#       "nome",
#       "cat",
#       "range",
#       "treat",
#       "post",
#       "gender")) | ends_with(c("planejamento",
#                                "work",
#                                "job")))) %>% 
#   dplyr::select(-c(
#     "id_cnae",
#     "id_educ"
#   )) %>% 
#   dplyr::mutate(
#     race_id = as.factor(race_id)
#   )
# 
# rm(rais.vinc.2007)
# 
# qp <- mice::quickpred(rais.vinc.test, exclude = c(
#   "ano",
#   "valor_remuneracao_media_sm",
#   "valor_remuneracao_dezembro_sm",
#   "tempo_emprego",
#   "quantidade_horas_contratadas",
#   "idade",
#   "grau_instrucao_1985_2005"
# ))
# 
# gc()
# 
# inputed.vinc.2007 <- mice::mice(
#   rais.vinc.test,
#   m = 5,
#   maxit = 10,
#   # predictorMatrix = qp,
#   method = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
#              "", "", "", "", "", "", "", "", "", "", "", "", "pmm", "pmm", "pmm")
# )
# 
# gc()
# 
# completed.vinc.2007 <- mice::complete(inputed.vinc.2007) %>% 
#   tibble::as_tibble() %>% 
#   saveRDS(file = "db/rais/vinc/mod/rais_vinc_2007_mod_v2.rds")
# 
# rm(inputed.vinc.2007, completed.vinc.2007, rais.vinc.test, qp)
# 
# gc()


# 2008
rais.vinc.2008 <- readRDS("db/rais/vinc/og/rais_vinc_2008.rds")

rais.vinc.test <- rais.vinc.2008 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "post",
      "gender")) | ends_with(c("planejamento",
                               "work",
                               "job")))) %>% 
  dplyr::select(-c(
    "id_cnae",
    "id_educ"
  )) # %>% 
# dplyr::mutate(
#   race_id = as.factor(race_id)
# )

rm(rais.vinc.2008)

qp <- mice::quickpred(rais.vinc.test, exclude = c(
  "ano",
  "valor_remuneracao_media_sm",
  "valor_remuneracao_dezembro_sm",
  "tempo_emprego",
  "quantidade_horas_contratadas",
  "idade",
  "grau_instrucao_1985_2005"
))

gc()

inputed.vinc.2008 <- mice::mice(
  rais.vinc.test,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
             "", "", "", "", "", "", "", "", "", "", "", "", "pmm", "pmm", "pmm")
)

gc()

completed.vinc.2008 <- mice::complete(inputed.vinc.2008) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/vinc/mod/rais_vinc_2008_mod_v2.rds")

rm(inputed.vinc.2008, completed.vinc.2008, rais.vinc.test, qp)

gc()


# 2009
rais.vinc.2009 <- readRDS("db/rais/vinc/og/rais_vinc_2009.rds")

rais.vinc.test <- rais.vinc.2009 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "post",
      "gender")) | ends_with(c("planejamento",
                               "work",
                               "job")))) %>% 
  dplyr::select(-c(
    "id_cnae",
    "id_educ"
  )) # %>% 
# dplyr::mutate(
#   race_id = as.factor(race_id)
# )

rm(rais.vinc.2009)

qp <- mice::quickpred(rais.vinc.test, exclude = c(
  "ano",
  "valor_remuneracao_media_sm",
  "valor_remuneracao_dezembro_sm",
  "tempo_emprego",
  "quantidade_horas_contratadas",
  "idade",
  "grau_instrucao_1985_2005"
))

gc()

inputed.vinc.2009 <- mice::mice(
  rais.vinc.test,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
             "", "", "", "", "", "", "", "", "", "", "", "", "pmm", "pmm", "pmm")
)

gc()

completed.vinc.2009 <- mice::complete(inputed.vinc.2009) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/vinc/mod/rais_vinc_2009_mod_v2.rds")

rm(inputed.vinc.2009, completed.vinc.2009, rais.vinc.test, qp)

gc()


# 2010
# rais.vinc.2010 <- readRDS("db/rais/vinc/og/rais_vinc_2010.rds")
# 
# rais.vinc.test <- rais.vinc.2010 %>% 
#   dplyr::select(-(
#     starts_with(c(
#       "nome",
#       "cat",
#       "range",
#       "treat",
#       "post",
#       "gender")) | ends_with(c("planejamento",
#                                "work",
#                                "job")))) %>% 
#   dplyr::select(-c(
#     "id_cnae",
#     "id_educ"
#   )) # %>% 
# # dplyr::mutate(
# #   race_id = as.factor(race_id)
# # )
# 
# rm(rais.vinc.2010)
# 
# qp <- mice::quickpred(rais.vinc.test, exclude = c(
#   "ano",
#   "valor_remuneracao_media_sm",
#   "valor_remuneracao_dezembro_sm",
#   "tempo_emprego",
#   "quantidade_horas_contratadas",
#   "idade",
#   "grau_instrucao_1985_2005"
# ))
# 
# gc()
# 
# inputed.vinc.2010 <- mice::mice(
#   rais.vinc.test,
#   m = 5,
#   maxit = 10,
#   predictorMatrix = qp,
#   method = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
#              "", "", "", "", "", "", "", "", "", "", "", "", "pmm", "pmm", "pmm")
# )
# 
# gc()
# 
# completed.vinc.2010 <- mice::complete(inputed.vinc.2010) %>% 
#   tibble::as_tibble() %>% 
#   saveRDS(file = "db/rais/vinc/mod/rais_vinc_2010_mod_v2.rds")
# 
# rm(inputed.vinc.2010, completed.vinc.2010, rais.vinc.test, qp)
# 
# gc()

## 2011 - 2015 ----

# 2011
rais.vinc.2011 <- readRDS("db/rais/vinc/og/rais_vinc_2011.rds")

rais.vinc.test <- rais.vinc.2011 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "post",
      "gender")) | ends_with(c("planejamento",
                               "work",
                               "job")))) %>% 
  dplyr::select(-c(
    "id_cnae",
    "id_educ"
  )) # %>% 
# dplyr::mutate(
#   race_id = as.factor(race_id)
# )

rm(rais.vinc.2011)

qp <- mice::quickpred(rais.vinc.test, exclude = c(
  "ano",
  "valor_remuneracao_media_sm",
  "valor_remuneracao_dezembro_sm",
  "tempo_emprego",
  "quantidade_horas_contratadas",
  "idade",
  "grau_instrucao_1985_2005"
))

gc()

inputed.vinc.2011 <- mice::mice(
  rais.vinc.test,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
             "", "", "", "", "", "", "", "", "", "", "", "", "pmm", "pmm", "pmm")
)

gc()

completed.vinc.2011 <- mice::complete(inputed.vinc.2011) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/vinc/mod/rais_vinc_2011_mod_v2.rds")

rm(inputed.vinc.2011, completed.vinc.2011, rais.vinc.test, qp)

gc()


# 2012
rais.vinc.2012 <- readRDS("db/rais/vinc/og/rais_vinc_2012.rds")

rais.vinc.test <- rais.vinc.2012 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "post",
      "gender")) | ends_with(c("planejamento",
                               "work",
                               "job")))) %>% 
  dplyr::select(-c(
    "id_cnae",
    "id_educ"
  )) # %>% 
# dplyr::mutate(
#   race_id = as.factor(race_id)
# )

rm(rais.vinc.2012)

qp <- mice::quickpred(rais.vinc.test, exclude = c(
  "ano",
  "valor_remuneracao_media_sm",
  "valor_remuneracao_dezembro_sm",
  "tempo_emprego",
  "quantidade_horas_contratadas",
  "idade",
  "grau_instrucao_1985_2005"
))

gc()

inputed.vinc.2012 <- mice::mice(
  rais.vinc.test,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
             "", "", "", "", "", "", "", "", "", "", "", "", "pmm", "pmm", "pmm")
)

gc()

completed.vinc.2012 <- mice::complete(inputed.vinc.2012) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/vinc/mod/rais_vinc_2012_mod_v2.rds")

rm(inputed.vinc.2012, completed.vinc.2012, rais.vinc.test, qp)

gc()


# 2013
rais.vinc.2013 <- readRDS("db/rais/vinc/og/rais_vinc_2013.rds")

rais.vinc.test <- rais.vinc.2013 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "post",
      "gender")) | ends_with(c("planejamento",
                               "work",
                               "job")))) %>% 
  dplyr::select(-c(
    "id_cnae",
    "id_educ"
  )) # %>% 
# dplyr::mutate(
#   race_id = as.factor(race_id)
# )

rm(rais.vinc.2013)

qp <- mice::quickpred(rais.vinc.test, exclude = c(
  "ano",
  "valor_remuneracao_media_sm",
  "valor_remuneracao_dezembro_sm",
  "tempo_emprego",
  "quantidade_horas_contratadas",
  "idade",
  "grau_instrucao_1985_2005"
))

gc()

inputed.vinc.2013 <- mice::mice(
  rais.vinc.test,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
             "", "", "", "", "", "", "", "", "", "", "", "", "pmm", "pmm", "pmm")
)

gc()

completed.vinc.2013 <- mice::complete(inputed.vinc.2013) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/vinc/mod/rais_vinc_2013_mod_v2.rds")

rm(inputed.vinc.2013, completed.vinc.2013, rais.vinc.test, qp)

gc()


# 2014
rais.vinc.2014 <- readRDS("db/rais/vinc/og/rais_vinc_2014.rds")

rais.vinc.test <- rais.vinc.2014 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "post",
      "gender")) | ends_with(c("planejamento",
                               "work",
                               "job")))) %>% 
  dplyr::select(-c(
    "id_cnae",
    "id_educ"
  )) # %>% 
# dplyr::mutate(
#   race_id = as.factor(race_id)
# )

rm(rais.vinc.2014)

qp <- mice::quickpred(rais.vinc.test, exclude = c(
  "ano",
  "valor_remuneracao_media_sm",
  "valor_remuneracao_dezembro_sm",
  "tempo_emprego",
  "quantidade_horas_contratadas",
  "idade",
  "grau_instrucao_1985_2005"
))

gc()

inputed.vinc.2014 <- mice::mice(
  rais.vinc.test,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
             "", "", "", "", "", "", "", "", "", "", "", "", "pmm", "pmm", "pmm")
)

gc()

completed.vinc.2014 <- mice::complete(inputed.vinc.2014) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/vinc/mod/rais_vinc_2014_mod_v2.rds")

rm(inputed.vinc.2014, completed.vinc.2014, rais.vinc.test, qp)

gc()


# 2015
rais.vinc.2015 <- readRDS("db/rais/vinc/og/rais_vinc_2015.rds")

rais.vinc.test <- rais.vinc.2015 %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "post",
      "gender")) | ends_with(c("planejamento",
                               "work",
                               "job")))) %>% 
  dplyr::select(-c(
    "id_cnae",
    "id_educ"
  )) # %>% 
# dplyr::mutate(
#   race_id = as.factor(race_id)
# )

rm(rais.vinc.2015)

qp <- mice::quickpred(rais.vinc.test, exclude = c(
  "ano",
  "valor_remuneracao_media_sm",
  "valor_remuneracao_dezembro_sm",
  "tempo_emprego",
  "quantidade_horas_contratadas",
  "idade",
  "grau_instrucao_1985_2005"
))

gc()

inputed.vinc.2015 <- mice::mice(
  rais.vinc.test,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
             "", "", "", "", "", "", "", "", "", "", "", "", "pmm", "pmm", "pmm")
)

gc()

completed.vinc.2015 <- mice::complete(inputed.vinc.2015) %>% 
  tibble::as_tibble() %>% 
  saveRDS(file = "db/rais/vinc/mod/rais_vinc_2015_mod_v2.rds")

rm(inputed.vinc.2015, completed.vinc.2015, rais.vinc.test, qp)

gc()

## race_id "basis" (2005-2006 bind) ----

rais.vinc.2005 <- readRDS("db/rais/vinc/og/rais_vinc_2005.rds") %>% 
  dplyr::mutate(educ_id = grau_instrucao_1985_2005) %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "post",
      "gender")) | ends_with(c("planejamento",
                               "work",
                               "job")))) %>% 
  dplyr::select(-c(
    "id_cnae",
    "id_educ"
  ))

rais.vinc.2006 <- readRDS("db/rais/vinc/mod/rais_vinc_2006_mod_v2.rds") %>% 
  dplyr::mutate(educ_id = grau_instrucao_apos_2005)

rais.vinc.rds <- dplyr::bind_rows(list(rais.vinc.2005, rais.vinc.2006))

rm(rais.vinc.2005, rais.vinc.2006)

gc()

qp <- mice::quickpred(rais.vinc.rds, exclude = c(
  "ano",
  "valor_remuneracao_media_sm",
  "valor_remuneracao_dezembro_sm",
  "tempo_emprego",
  "quantidade_horas_contratadas",
  "idade",
  "grau_instrucao_1985_2005",
  "grau_instrucao_apos_2005"
))

gc()

inputed.vinc.2005.2006 <- mice::mice(
  rais.vinc.rds,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
             "", "", "", "", "", "", "", "", "", "", "", "", "pmm", "pmm", "pmm",
             "")
)

gc()

completed.vinc.2005 <- mice::complete(inputed.vinc.2005.2006) %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(ano == 2005) %>% 
  saveRDS(file = "db/rais/vinc/mod/rais_vinc_2005_mod_v2.rds")

rm(completed.vinc.2005, rais.vinc.rds, inputed.vinc.2005.2006, qp)

gc()

## 2004 - 2000 ----

# 2004
rais.vinc.2004 <- readRDS("db/rais/vinc/og/rais_vinc_2004.rds") %>% 
  dplyr::mutate(educ_id = grau_instrucao_1985_2005) %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "post",
      "gender")) | ends_with(c("planejamento",
                               "work",
                               "job")))) %>% 
  dplyr::select(-c(
    "id_cnae",
    "id_educ"
  ))

rais.vinc.2005 <- readRDS("db/rais/vinc/mod/rais_vinc_2005_mod_v2.rds")

rais.vinc.2006 <- readRDS("db/rais/vinc/mod/rais_vinc_2006_mod_v2.rds") %>% 
  dplyr::mutate(educ_id = grau_instrucao_apos_2005)

rais.vinc.rds <- dplyr::bind_rows(list(rais.vinc.2004, 
                                       rais.vinc.2005,
                                       rais.vinc.2006))

rm(rais.vinc.2004, rais.vinc.2005, rais.vinc.2006)

gc()

qp <- mice::quickpred(rais.vinc.rds, exclude = c(
  "ano",
  "valor_remuneracao_media_sm",
  "valor_remuneracao_dezembro_sm",
  "tempo_emprego",
  "quantidade_horas_contratadas",
  "idade",
  "grau_instrucao_1985_2005",
  "grau_instrucao_apos_2005"
))

inputed.vinc.2004.2005 <- mice::mice(
  rais.vinc.rds,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
             "", "", "", "", "", "", "", "", "", "", "", "", "pmm", "pmm", "pmm",
             "")
)

gc()

completed.vinc.2004 <- mice::complete(inputed.vinc.2004.2005) %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(ano == 2004) %>% 
  saveRDS(file = "db/rais/vinc/mod/rais_vinc_2004_mod_v2.rds")

rm(completed.vinc.2004, rais.vinc.rds, inputed.vinc.2004.2005, qp)

gc()

# 2003
rais.vinc.2003 <- readRDS("db/rais/vinc/og/rais_vinc_2003.rds") %>% 
  dplyr::mutate(educ_id = grau_instrucao_1985_2005) %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "post",
      "gender")) | ends_with(c("planejamento",
                               "work",
                               "job")))) %>% 
  dplyr::select(-c(
    "id_cnae",
    "id_educ"
  ))

rais.vinc.2004 <- readRDS("db/rais/vinc/mod/rais_vinc_2004_mod_v2.rds")

rais.vinc.2005 <- readRDS("db/rais/vinc/mod/rais_vinc_2005_mod_v2.rds")

rais.vinc.2006 <- readRDS("db/rais/vinc/mod/rais_vinc_2006_mod_v2.rds") %>% 
  dplyr::mutate(educ_id = grau_instrucao_apos_2005)

rais.vinc.rds <- dplyr::bind_rows(list(rais.vinc.2003, 
                                       rais.vinc.2004,
                                       rais.vinc.2005,
                                       rais.vinc.2006))

rm(rais.vinc.2003, 
   rais.vinc.2004, 
   rais.vinc.2005, 
   rais.vinc.2006)

gc()

qp <- mice::quickpred(rais.vinc.rds, exclude = c(
  "ano",
  "valor_remuneracao_media_sm",
  "valor_remuneracao_dezembro_sm",
  "tempo_emprego",
  "quantidade_horas_contratadas",
  "idade",
  "grau_instrucao_1985_2005",
  "grau_instrucao_apos_2005"
))

gc()

inputed.vinc.2003.2004 <- mice::mice(
  rais.vinc.rds,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
             "", "", "", "", "", "", "", "", "", "", "", "", "pmm", "pmm", "pmm",
             "")
)

gc()

completed.vinc.2003 <- mice::complete(inputed.vinc.2003.2004) %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(ano == 2003) %>% 
  saveRDS(file = "db/rais/vinc/mod/rais_vinc_2003_mod_v2.rds")

rm(completed.vinc.2003, rais.vinc.rds, inputed.vinc.2003.2004, qp)

gc()

# 2002
rais.vinc.2002 <- readRDS("db/rais/vinc/og/rais_vinc_2002.rds") %>% 
  dplyr::mutate(educ_id = grau_instrucao_1985_2005) %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "post",
      "gender")) | ends_with(c("planejamento",
                               "work",
                               "job")))) %>% 
  dplyr::select(-c(
    "id_cnae",
    "id_educ"
  ))

rais.vinc.2003 <- readRDS("db/rais/vinc/mod/rais_vinc_2003_mod_v2.rds")

rais.vinc.2004 <- readRDS("db/rais/vinc/mod/rais_vinc_2004_mod_v2.rds")

rais.vinc.2005 <- readRDS("db/rais/vinc/mod/rais_vinc_2005_mod_v2.rds")

rais.vinc.2006 <- readRDS("db/rais/vinc/mod/rais_vinc_2006_mod_v2.rds") %>% 
  dplyr::mutate(educ_id = grau_instrucao_apos_2005)

rais.vinc.rds <- dplyr::bind_rows(list(rais.vinc.2002, 
                                       rais.vinc.2003,
                                       rais.vinc.2004,
                                       rais.vinc.2005,
                                       rais.vinc.2006))

rm(rais.vinc.2002, 
   rais.vinc.2003, 
   rais.vinc.2004, 
   rais.vinc.2005, 
   rais.vinc.2006)

gc()

qp <- mice::quickpred(rais.vinc.rds, exclude = c(
  "ano",
  "valor_remuneracao_media_sm",
  "valor_remuneracao_dezembro_sm",
  "tempo_emprego",
  "quantidade_horas_contratadas",
  "idade",
  "grau_instrucao_1985_2005",
  "grau_instrucao_apos_2005"
))

gc()

inputed.vinc.2002.2003 <- mice::mice(
  rais.vinc.rds,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
             "", "", "", "", "", "", "", "", "", "", "", "", "pmm", "pmm", "pmm",
             "")
)

gc()

completed.vinc.2002 <- mice::complete(inputed.vinc.2002.2003) %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(ano == 2002) %>% 
  saveRDS(file = "db/rais/vinc/mod/rais_vinc_2002_mod_v2.rds")

rm(completed.vinc.2002, rais.vinc.rds, inputed.vinc.2002.2003, qp)

gc()

# 2001
rais.vinc.2001 <- readRDS("db/rais/vinc/og/rais_vinc_2001.rds") %>% 
  dplyr::mutate(educ_id = grau_instrucao_1985_2005) %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "post",
      "gender")) | ends_with(c("planejamento",
                               "work",
                               "job")))) %>% 
  dplyr::select(-c(
    "id_cnae",
    "id_educ"
  ))

rais.vinc.2002 <- readRDS("db/rais/vinc/mod/rais_vinc_2002_mod_v2.rds")

rais.vinc.2003 <- readRDS("db/rais/vinc/mod/rais_vinc_2003_mod_v2.rds")

rais.vinc.2004 <- readRDS("db/rais/vinc/mod/rais_vinc_2004_mod_v2.rds")

rais.vinc.2005 <- readRDS("db/rais/vinc/mod/rais_vinc_2005_mod_v2.rds")

rais.vinc.2006 <- readRDS("db/rais/vinc/mod/rais_vinc_2006_mod_v2.rds") %>% 
  dplyr::mutate(educ_id = grau_instrucao_apos_2005)

rais.vinc.rds <- dplyr::bind_rows(list(rais.vinc.2001, 
                                       rais.vinc.2002,
                                       rais.vinc.2003,
                                       rais.vinc.2004,
                                       rais.vinc.2005,
                                       rais.vinc.2006))

rm(rais.vinc.2001, 
   rais.vinc.2002, 
   rais.vinc.2003, 
   rais.vinc.2004, 
   rais.vinc.2005, 
   rais.vinc.2006)

gc()

qp <- mice::quickpred(rais.vinc.rds, exclude = c(
  "ano",
  "valor_remuneracao_media_sm",
  "valor_remuneracao_dezembro_sm",
  "tempo_emprego",
  "quantidade_horas_contratadas",
  "idade",
  "grau_instrucao_1985_2005",
  "grau_instrucao_apos_2005"
))

gc()

inputed.vinc.2001.2002 <- mice::mice(
  rais.vinc.rds,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
             "", "", "", "", "", "", "", "", "", "", "", "", "pmm", "pmm", "pmm",
             "")
)

gc()

completed.vinc.2001 <- mice::complete(inputed.vinc.2001.2002) %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(ano == 2001) %>% 
  saveRDS(file = "db/rais/vinc/mod/rais_vinc_2001_mod_v2.rds")

rm(completed.vinc.2001, rais.vinc.rds, inputed.vinc.2001.2002, qp)

gc()

# 2000
rais.vinc.2000 <- readRDS("db/rais/vinc/og/rais_vinc_2000.rds") %>% 
  dplyr::mutate(educ_id = grau_instrucao_1985_2005) %>% 
  dplyr::select(-(
    starts_with(c(
      "nome",
      "cat",
      "range",
      "treat",
      "post",
      "gender")) | ends_with(c("planejamento",
                               "work",
                               "job")))) %>% 
  dplyr::select(-c(
    "id_cnae",
    "id_educ"
  ))

rais.vinc.2001 <- readRDS("db/rais/vinc/mod/rais_vinc_2001_mod_v2.rds")

rais.vinc.2002 <- readRDS("db/rais/vinc/mod/rais_vinc_2002_mod_v2.rds")

rais.vinc.2003 <- readRDS("db/rais/vinc/mod/rais_vinc_2003_mod_v2.rds")

rais.vinc.2004 <- readRDS("db/rais/vinc/mod/rais_vinc_2004_mod_v2.rds")

rais.vinc.2005 <- readRDS("db/rais/vinc/mod/rais_vinc_2005_mod_v2.rds")

rais.vinc.2006 <- readRDS("db/rais/vinc/mod/rais_vinc_2006_mod_v2.rds") %>% 
  dplyr::mutate(educ_id = grau_instrucao_apos_2005)

rais.vinc.rds <- dplyr::bind_rows(list(rais.vinc.2000, 
                                       rais.vinc.2001, 
                                       rais.vinc.2002,
                                       rais.vinc.2003,
                                       rais.vinc.2004,
                                       rais.vinc.2005,
                                       rais.vinc.2006))

rm(rais.vinc.2000, 
   rais.vinc.2001, 
   rais.vinc.2002, 
   rais.vinc.2003, 
   rais.vinc.2004, 
   rais.vinc.2005, 
   rais.vinc.2006)

gc()

qp <- mice::quickpred(rais.vinc.rds, exclude = c(
  "ano",
  "valor_remuneracao_media_sm",
  "valor_remuneracao_dezembro_sm",
  "tempo_emprego",
  "quantidade_horas_contratadas",
  "idade",
  "grau_instrucao_1985_2005",
  "grau_instrucao_apos_2005"
))

gc()

inputed.vinc.2000.2001 <- mice::mice(
  rais.vinc.rds,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
             "", "", "", "", "", "", "", "", "", "", "", "", "pmm", "pmm", "pmm",
             "")
)

gc()

completed.vinc.2000 <- mice::complete(inputed.vinc.2000.2001) %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(ano == 2000) %>% 
  saveRDS(file = "db/rais/vinc/mod/rais_vinc_2000_mod_v2.rds")

rm(completed.vinc.2000, rais.vinc.rds, inputed.vinc.2000.2001, qp)

gc()

# 2007 ----

rais.vinc.2006 <- readRDS("db/rais/vinc/mod/rais_vinc_2006_mod_v2.rds")

rais.vinc.2007 <- readRDS("db/rais/vinc/mod/rais_vinc_2007_mod_v2.rds")

rais.vinc.2008 <- readRDS("db/rais/vinc/mod/rais_vinc_2008_mod_v2.rds")

rais.vinc.rds <- dplyr::bind_rows(list(rais.vinc.2006, 
                                       rais.vinc.2007,
                                       rais.vinc.2008))

rm(rais.vinc.2006, 
   rais.vinc.2007, 
   rais.vinc.2008)

gc()

qp <- mice::quickpred(rais.vinc.rds, exclude = c(
  "ano",
  "valor_remuneracao_media_sm",
  "valor_remuneracao_dezembro_sm",
  "tempo_emprego",
  "quantidade_horas_contratadas",
  "idade",
  "grau_instrucao_1985_2005",
  "grau_instrucao_apos_2005"
))

gc()

inputed.vinc.2006.2007.2008 <- mice::mice(
  rais.vinc.rds,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
             "", "", "", "", "", "", "", "", "", "", "", "", "pmm", "pmm", "pmm")
)

gc()

completed.vinc.2007.v3 <- mice::complete(inputed.vinc.2006.2007.2008) %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(ano == 2007) %>% 
  saveRDS(file = "db/rais/vinc/mod/rais_vinc_2007_mod_v3.rds")

rm(completed.vinc.2007.v3,
   rais.vinc.rds,
   inputed.vinc.2006.2007.2008,
   qp)

gc()

# 2010 ----

rais.vinc.2009 <- readRDS("db/rais/vinc/mod/rais_vinc_2009_mod_v2.rds")

rais.vinc.2010 <- readRDS("db/rais/vinc/mod/rais_vinc_2010_mod_v2.rds")

rais.vinc.2011 <- readRDS("db/rais/vinc/mod/rais_vinc_2011_mod_v2.rds")

rais.vinc.rds <- dplyr::bind_rows(list(rais.vinc.2009, 
                                       rais.vinc.2010,
                                       rais.vinc.2011))

rm(rais.vinc.2009, 
   rais.vinc.2010, 
   rais.vinc.2011)

gc()

qp <- mice::quickpred(rais.vinc.rds, exclude = c(
  "ano",
  "valor_remuneracao_media_sm",
  "valor_remuneracao_dezembro_sm",
  "tempo_emprego",
  "quantidade_horas_contratadas",
  "idade",
  "grau_instrucao_1985_2005",
  "grau_instrucao_apos_2005"
))

gc()

inputed.vinc.2009.2010.2011 <- mice::mice(
  rais.vinc.rds,
  m = 5,
  maxit = 10,
  predictorMatrix = qp,
  method = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
             "", "", "", "", "", "", "", "", "", "", "", "", "pmm", "pmm", "pmm")
)

gc()

completed.vinc.2010.v3 <- mice::complete(inputed.vinc.2009.2010.2011) %>% 
  tibble::as_tibble() %>% 
  dplyr::filter(ano == 2010) %>% 
  saveRDS(file = "db/rais/vinc/mod/rais_vinc_2010_mod_v3.rds")

rm(completed.vinc.2010.v3,
   rais.vinc.rds,
   inputed.vinc.2009.2010.2011,
   qp)

gc()
