#### beginning ----

source("scripts/001_libraries.R")

fs::dir_create("outputs/pnad/regressions/PCA")
fs::dir_create("outputs/pnad/regressions/PCA/corr")
fs::dir_create("outputs/pnad/regressions/PCA/plots")
fs::dir_create("outputs/pnad/regressions/PCA/loads")
fs::dir_create("outputs/pnad/regressions/PCA/reports")

#### PCA - PNAD ----

### 2001 ----

## loading db ----

pnad.indv.2001 <- readRDS("db/pnad/pnad_indv_2001.rds") %>% 
  dplyr::mutate(boss_id = case_when(
    cat_condicao_domicilio == "chefe" ~ 1,
    is.na(cat_condicao_domicilio) ~ NA_real_,
    TRUE ~ 0)) %>% 
  dplyr::select(
    c(
      # dinheiro:
      "renda_mensal_ocupacao_principal_deflacionado",
      "renda_mensal_todas_fontes_deflacionado",
      "renda_aposentadoria_deflacionado",
      "renda_pensao_deflacionado",
      "q1_id",
      # características individuais:
      "idade",
      "anos_estudo",
      "gender_id",
      "race_id",
      # trabalho:
      "trabalhou_semana",
      # "tinha_outro_trabalho",
      "possui_carteira_assinada",
      "contribui_previdencia",
      "horas_trabalhadas_todos_trabalhos",
      "entrep",
      # "posicao_ocupacao",
      # socioeconomia:
      "boss_id",
      # "condicao_familia"
    )
  ) %>% 
  dplyr::mutate_if(
    is.character, as.numeric
  ) %>% 
  mutate_at(
    vars(
      starts_with(c("renda",
                    "tinha",
                    "possui",
                    "contribui",
                    "horas",
                    "entrep"))), ~replace(., is.na(.), 0)) %>% 
  mutate_at(
    vars(
      q1_id), ~replace(., is.na(.), 0)) %>% 
  dplyr::`filter`(
    idade > 23
  )

## evaluating correlation matrix ----

# correlation over 0.3! and less than 0.8! (see rPubs)

dat <- na.omit(pnad.indv.2001)

mydata <- dat

summary(mydata)

colSums(is.na(mydata))

# missings <- colSums(is.na(mydata))
# 
# summary(missings)
# 
# dim(mydata)

rcorr.adjust(mydata)

write.csv(cor(mydata)>0.8, file="outputs/pnad/regressions/PCA/corr/2001_Suspect_Correlations.csv")

write.csv(cor(mydata), file="outputs/pnad/regressions/PCA/corr/2001_Correlation_Values.csv")


## KMO test ----

# KMO over 0.6! (see rPubs)

sink(file = "outputs/pnad/regressions/PCA/reports/2001_PNAD_PCA.md", append = FALSE)

KMO(mydata)

# mydat <- mydata[, KMO(mydata)$MSAi==0.39] # Get rid of all variables with MSA < 0.50
# 
# mydata <- mydat

round( KMO(mydata)$MSA, 2 )


## Bartlett’s test ----

# p<0.05! (see rPubs)

cortest.bartlett(mydata)



## scree plot and parallel analysis ----

# extract eigenvalues 

ev <- eigen(cor(mydata)) # get eigenvalues

ev$values

# make the scree plot 

scree(mydata, pc = FALSE)  # Use pc=FALSE for factor analysis

# and parallel analysis to decide on no. of factors (see rPubs)

fa.parallel(mydata, fa="fa")


## extracting and rotating factors ----

# use promax first

Nfacs <- 6  # This is for four factors. You can change this as needed.

fit <- factanal(mydata, Nfacs, rotation="promax")

print(fit, digits=2, cutoff=0.3, sort=TRUE)


# then varimax if correlations between factors are low (r < |0.3|) (see rPubs)

Nfacs <- 6  # This is for four factors. You can change this as needed.

fit <- factanal(mydata, Nfacs, rotation="varimax")

print(fit, digits=2, cutoff=0.3, sort=TRUE)


## plotting factors ----

# for interpretation of each variable (see rPubs)

load <- fit$loadings[,1:2]

png(file = "outputs/pnad/regressions/PCA/f1_v_f2_2001_test.png",
    width = 800,
    height = 600,
    type = "cairo")

plot(load,type="n") # set up plot

text(load,labels=names(mydata),cex=.7)

dev.off()



## factor analysis ----

# for interpretation of each variable (see rPubs)

loads <- fit$loadings

png(file = "outputs/pnad/regressions/PCA/plots/2001_factor_analysis.png",
    width = 800,
    height = 600,
    type = "cairo")

fa.diagram(loads)

dev.off()


## export factor loadings ----

dim(fit$loadings)

round(fit$loadings[ 1:15,], 6)

FactorLoadings <- round(fit$loadings[1:14, ], 5)

write.csv(FactorLoadings, file="outputs/pnad/regressions/PCA/loads/2001_FacLoads.csv")

# check if each variable loads sufficiently into a factor (loading > |0.4|)

# check if each factor at least three variables loading onto them

# see if crossloadings can be avoided 

# see if each factor is interpretable (see rPubs)

# Cronbach's Alpha (just for the sake of it)

f1 <- mydata[ , c("possui_carteira_assinada", "trabalhou_semana", "contribui_previdencia",
                  "horas_trabalhadas_todos_trabalhos", "idade")]
f2 <- mydata[ , c("renda_mensal_ocupacao_principal_deflacionado", "renda_mensal_todas_fontes_deflacionado")]
f3 <- mydata[ , c("gender_id", "boss_id")]
# f4 <- mydata[ , c("entrep")]
f5 <- mydata[ , c("anos_estudo", "q1_id", "race_id")]
f6 <- mydata[ , c("renda_aposentadoria_deflacionado", "renda_pensao_deflacionado")]

alpha(f1, check.keys=TRUE)$total[1]
alpha(f2, check.keys=TRUE)$total[1]
alpha(f3, check.keys=TRUE)$total[1]
# alpha(f4, check.keys=TRUE)$total[1]
alpha(f5, check.keys=TRUE)$total[1]
alpha(f6, check.keys=TRUE)$total[1]

sink()


### 2002 - 2009 ----

### 2002 ----

## loading db ----

pnad.indv.2002 <- readRDS("db/pnad/pnad_indv_2002.rds") %>% 
  dplyr::mutate(boss_id = case_when(
    cat_condicao_domicilio == "chefe" ~ 1,
    is.na(cat_condicao_domicilio) ~ NA_real_,
    TRUE ~ 0)) %>% 
  dplyr::select(
    c(
      # dinheiro:
      "renda_mensal_ocupacao_principal_deflacionado",
      "renda_mensal_todas_fontes_deflacionado",
      "renda_aposentadoria_deflacionado",
      "renda_pensao_deflacionado",
      "q1_id",
      # características individuais:
      "idade",
      "anos_estudo",
      "gender_id",
      "race_id",
      # trabalho:
      "trabalhou_semana",
      # "tinha_outro_trabalho",
      "possui_carteira_assinada",
      "contribui_previdencia",
      "horas_trabalhadas_todos_trabalhos",
      "entrep",
      # "posicao_ocupacao",
      # socioeconomia:
      "boss_id",
      # "condicao_familia"
    )
  ) %>% 
  dplyr::mutate_if(
    is.character, as.numeric
  ) %>% 
  mutate_at(
    vars(
      starts_with(c("renda",
                    "tinha",
                    "possui",
                    "contribui",
                    "horas",
                    "entrep"))), ~replace(., is.na(.), 0)) %>% 
  mutate_at(
    vars(
      q1_id), ~replace(., is.na(.), 0)) %>% 
  dplyr::`filter`(
    idade > 23
  )


## evaluating correlation matrix ----

dat <- na.omit(pnad.indv.2002)
mydata <- dat
summary(mydata)
colSums(is.na(mydata))
rcorr.adjust(mydata)
write.csv(cor(mydata)>0.8, file="outputs/pnad/regressions/PCA/corr/2002_Suspect_Correlations.csv")
write.csv(cor(mydata), file="outputs/pnad/regressions/PCA/corr/2002_Correlation_Values.csv")


## KMO test ----

sink(file = "outputs/pnad/regressions/PCA/reports/2002_PNAD_PCA.md", append = FALSE)
KMO(mydata)
round( KMO(mydata)$MSA, 2 )


## Bartlett’s test ----

cortest.bartlett(mydata)


## scree plot and parallel analysis ----

ev <- eigen(cor(mydata)) # get eigenvalues
ev$values
scree(mydata, pc = FALSE)  # Use pc=FALSE for factor analysis
fa.parallel(mydata, fa="fa")


## extracting and rotating factors ----

Nfacs <- 5  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="promax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)

Nfacs <- 5  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="varimax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)


## plotting factors ----

load <- fit$loadings[,1:2]
png(file = "outputs/pnad/regressions/PCA/f1_v_f2_2002_test.png",
    width = 800,
    height = 600,
    type = "cairo")
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7)
dev.off()


## factor analysis ----

loads <- fit$loadings
png(file = "outputs/pnad/regressions/PCA/plots/2002_factor_analysis.png",
    width = 800,
    height = 600,
    type = "cairo")
fa.diagram(loads)
dev.off()


## export factor loadings ----

dim(fit$loadings)
round(fit$loadings[ 1:15,], 6)
FactorLoadings <- round(fit$loadings[1:14, ], 5)
write.csv(FactorLoadings, file="outputs/pnad/regressions/PCA/loads/2002_FacLoads.csv")


# Cronbach's Alpha (just for the sake of it)

# f1 <- mydata[ , c("possui_carteira_assinada", "trabalhou_semana", "contribui_previdencia",
#                   "horas_trabalhadas_todos_trabalhos", "idade")]
# f2 <- mydata[ , c("renda_mensal_ocupacao_principal_deflacionado", "renda_mensal_todas_fontes_deflacionado")]
# f3 <- mydata[ , c("gender_id", "boss_id")]
# # f4 <- mydata[ , c("entrep")]
# f5 <- mydata[ , c("anos_estudo", "q1_id", "race_id")]
# f6 <- mydata[ , c("renda_aposentadoria_deflacionado", "renda_pensao_deflacionado")]
# alpha(f1, check.keys=TRUE)$total[1]
# alpha(f2, check.keys=TRUE)$total[1]
# alpha(f3, check.keys=TRUE)$total[1]
# # alpha(f4, check.keys=TRUE)$total[1]
# alpha(f5, check.keys=TRUE)$total[1]
# alpha(f6, check.keys=TRUE)$total[1]

sink()




### 2003 ----

## loading db ----

pnad.indv.2003 <- readRDS("db/pnad/pnad_indv_2003.rds") %>% 
  dplyr::mutate(boss_id = case_when(
    cat_condicao_domicilio == "chefe" ~ 1,
    is.na(cat_condicao_domicilio) ~ NA_real_,
    TRUE ~ 0)) %>% 
  dplyr::select(
    c(
      # dinheiro:
      "renda_mensal_ocupacao_principal_deflacionado",
      "renda_mensal_todas_fontes_deflacionado",
      "renda_aposentadoria_deflacionado",
      "renda_pensao_deflacionado",
      "q1_id",
      # características individuais:
      "idade",
      "anos_estudo",
      "gender_id",
      "race_id",
      # trabalho:
      "trabalhou_semana",
      # "tinha_outro_trabalho",
      "possui_carteira_assinada",
      "contribui_previdencia",
      "horas_trabalhadas_todos_trabalhos",
      "entrep",
      # "posicao_ocupacao",
      # socioeconomia:
      "boss_id",
      # "condicao_familia"
    )
  ) %>% 
  dplyr::mutate_if(
    is.character, as.numeric
  ) %>% 
  mutate_at(
    vars(
      starts_with(c("renda",
                    "tinha",
                    "possui",
                    "contribui",
                    "horas",
                    "entrep"))), ~replace(., is.na(.), 0)) %>% 
  mutate_at(
    vars(
      q1_id), ~replace(., is.na(.), 0)) %>% 
  dplyr::`filter`(
    idade > 23
  )


## evaluating correlation matrix ----

dat <- na.omit(pnad.indv.2003)
mydata <- dat
summary(mydata)
colSums(is.na(mydata))
rcorr.adjust(mydata)
write.csv(cor(mydata)>0.8, file="outputs/pnad/regressions/PCA/corr/2003_Suspect_Correlations.csv")
write.csv(cor(mydata), file="outputs/pnad/regressions/PCA/corr/2003_Correlation_Values.csv")


## KMO test ----

sink(file = "outputs/pnad/regressions/PCA/reports/2003_PNAD_PCA.md", append = FALSE)
KMO(mydata)
round( KMO(mydata)$MSA, 2 )


## Bartlett’s test ----

cortest.bartlett(mydata)


## scree plot and parallel analysis ----

ev <- eigen(cor(mydata)) # get eigenvalues
ev$values
scree(mydata, pc = FALSE)  # Use pc=FALSE for factor analysis
fa.parallel(mydata, fa="fa")


## extracting and rotating factors ----

Nfacs <- 6  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="promax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)

Nfacs <- 6  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="varimax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)


## plotting factors ----

load <- fit$loadings[,1:2]
png(file = "outputs/pnad/regressions/PCA/f1_v_f2_2003_test.png",
    width = 800,
    height = 600,
    type = "cairo")
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7)
dev.off()


## factor analysis ----

loads <- fit$loadings
png(file = "outputs/pnad/regressions/PCA/plots/2003_factor_analysis.png",
    width = 800,
    height = 600,
    type = "cairo")
fa.diagram(loads)
dev.off()


## export factor loadings ----

dim(fit$loadings)
round(fit$loadings[ 1:15,], 6)
FactorLoadings <- round(fit$loadings[1:14, ], 5)
write.csv(FactorLoadings, file="outputs/pnad/regressions/PCA/loads/2003_FacLoads.csv")


# Cronbach's Alpha (just for the sake of it)

f1 <- mydata[ , c("possui_carteira_assinada", "trabalhou_semana", "contribui_previdencia",
                  "horas_trabalhadas_todos_trabalhos", "idade")]
f2 <- mydata[ , c("renda_mensal_ocupacao_principal_deflacionado", "renda_mensal_todas_fontes_deflacionado")]
f3 <- mydata[ , c("gender_id", "boss_id")]
# f4 <- mydata[ , c("entrep")]
f5 <- mydata[ , c("anos_estudo", "q1_id", "race_id")]
f6 <- mydata[ , c("renda_aposentadoria_deflacionado", "renda_pensao_deflacionado")]
alpha(f1, check.keys=TRUE)$total[1]
alpha(f2, check.keys=TRUE)$total[1]
alpha(f3, check.keys=TRUE)$total[1]
# alpha(f4, check.keys=TRUE)$total[1]
alpha(f5, check.keys=TRUE)$total[1]
alpha(f6, check.keys=TRUE)$total[1]

sink()




### 2004 ----

## loading db ----

pnad.indv.2004 <- readRDS("db/pnad/pnad_indv_2004.rds") %>% 
  dplyr::mutate(boss_id = case_when(
    cat_condicao_domicilio == "chefe" ~ 1,
    is.na(cat_condicao_domicilio) ~ NA_real_,
    TRUE ~ 0)) %>% 
  dplyr::select(
    c(
      # dinheiro:
      "renda_mensal_ocupacao_principal_deflacionado",
      "renda_mensal_todas_fontes_deflacionado",
      "renda_aposentadoria_deflacionado",
      "renda_pensao_deflacionado",
      "q1_id",
      # características individuais:
      "idade",
      "anos_estudo",
      "gender_id",
      "race_id",
      # trabalho:
      "trabalhou_semana",
      # "tinha_outro_trabalho",
      "possui_carteira_assinada",
      "contribui_previdencia",
      "horas_trabalhadas_todos_trabalhos",
      "entrep",
      # "posicao_ocupacao",
      # socioeconomia:
      "boss_id",
      # "condicao_familia"
    )
  ) %>% 
  dplyr::mutate_if(
    is.character, as.numeric
  ) %>% 
  mutate_at(
    vars(
      starts_with(c("renda",
                    "tinha",
                    "possui",
                    "contribui",
                    "horas",
                    "entrep"))), ~replace(., is.na(.), 0)) %>% 
  mutate_at(
    vars(
      q1_id), ~replace(., is.na(.), 0)) %>% 
  dplyr::`filter`(
    idade > 23
  )


## evaluating correlation matrix ----

dat <- na.omit(pnad.indv.2004)
mydata <- dat
summary(mydata)
colSums(is.na(mydata))
rcorr.adjust(mydata)
write.csv(cor(mydata)>0.8, file="outputs/pnad/regressions/PCA/corr/2004_Suspect_Correlations.csv")
write.csv(cor(mydata), file="outputs/pnad/regressions/PCA/corr/2004_Correlation_Values.csv")


## KMO test ----

sink(file = "outputs/pnad/regressions/PCA/reports/2004_PNAD_PCA.md", append = FALSE)
KMO(mydata)
round( KMO(mydata)$MSA, 2 )


## Bartlett’s test ----

cortest.bartlett(mydata)


## scree plot and parallel analysis ----

ev <- eigen(cor(mydata)) # get eigenvalues
ev$values
scree(mydata, pc = FALSE)  # Use pc=FALSE for factor analysis
fa.parallel(mydata, fa="fa")


## extracting and rotating factors ----

Nfacs <- 6  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="promax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)

Nfacs <- 6  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="varimax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)


## plotting factors ----

load <- fit$loadings[,1:2]
png(file = "outputs/pnad/regressions/PCA/f1_v_f2_2004_test.png",
    width = 800,
    height = 600,
    type = "cairo")
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7)
dev.off()


## factor analysis ----

loads <- fit$loadings
png(file = "outputs/pnad/regressions/PCA/plots/2004_factor_analysis.png",
    width = 800,
    height = 600,
    type = "cairo")
fa.diagram(loads)
dev.off()


## export factor loadings ----

dim(fit$loadings)
round(fit$loadings[ 1:15,], 6)
FactorLoadings <- round(fit$loadings[1:14, ], 5)
write.csv(FactorLoadings, file="outputs/pnad/regressions/PCA/loads/2004_FacLoads.csv")


# Cronbach's Alpha (just for the sake of it)

f1 <- mydata[ , c("possui_carteira_assinada", "trabalhou_semana", "contribui_previdencia",
                  "horas_trabalhadas_todos_trabalhos", "idade")]
f2 <- mydata[ , c("renda_mensal_ocupacao_principal_deflacionado", "renda_mensal_todas_fontes_deflacionado")]
f3 <- mydata[ , c("gender_id", "boss_id")]
# f4 <- mydata[ , c("entrep")]
f5 <- mydata[ , c("anos_estudo", "q1_id", "race_id")]
f6 <- mydata[ , c("renda_aposentadoria_deflacionado", "renda_pensao_deflacionado")]
alpha(f1, check.keys=TRUE)$total[1]
alpha(f2, check.keys=TRUE)$total[1]
alpha(f3, check.keys=TRUE)$total[1]
# alpha(f4, check.keys=TRUE)$total[1]
alpha(f5, check.keys=TRUE)$total[1]
alpha(f6, check.keys=TRUE)$total[1]

sink()




### 2005 ----

## loading db ----

pnad.indv.2005 <- readRDS("db/pnad/pnad_indv_2005.rds") %>% 
  dplyr::mutate(boss_id = case_when(
    cat_condicao_domicilio == "chefe" ~ 1,
    is.na(cat_condicao_domicilio) ~ NA_real_,
    TRUE ~ 0)) %>% 
  dplyr::select(
    c(
      # dinheiro:
      "renda_mensal_ocupacao_principal_deflacionado",
      "renda_mensal_todas_fontes_deflacionado",
      "renda_aposentadoria_deflacionado",
      "renda_pensao_deflacionado",
      "q1_id",
      # características individuais:
      "idade",
      "anos_estudo",
      "gender_id",
      "race_id",
      # trabalho:
      "trabalhou_semana",
      # "tinha_outro_trabalho",
      "possui_carteira_assinada",
      "contribui_previdencia",
      "horas_trabalhadas_todos_trabalhos",
      "entrep",
      # "posicao_ocupacao",
      # socioeconomia:
      "boss_id",
      # "condicao_familia"
    )
  ) %>% 
  dplyr::mutate_if(
    is.character, as.numeric
  ) %>% 
  mutate_at(
    vars(
      starts_with(c("renda",
                    "tinha",
                    "possui",
                    "contribui",
                    "horas",
                    "entrep"))), ~replace(., is.na(.), 0)) %>% 
  mutate_at(
    vars(
      q1_id), ~replace(., is.na(.), 0)) %>% 
  dplyr::`filter`(
    idade > 23
  )


## evaluating correlation matrix ----

dat <- na.omit(pnad.indv.2005)
mydata <- dat
summary(mydata)
colSums(is.na(mydata))
rcorr.adjust(mydata)
write.csv(cor(mydata)>0.8, file="outputs/pnad/regressions/PCA/corr/2005_Suspect_Correlations.csv")
write.csv(cor(mydata), file="outputs/pnad/regressions/PCA/corr/2005_Correlation_Values.csv")


## KMO test ----

sink(file = "outputs/pnad/regressions/PCA/reports/2005_PNAD_PCA.md", append = FALSE)
KMO(mydata)
round( KMO(mydata)$MSA, 2 )


## Bartlett’s test ----

cortest.bartlett(mydata)


## scree plot and parallel analysis ----

ev <- eigen(cor(mydata)) # get eigenvalues
ev$values
scree(mydata, pc = FALSE)  # Use pc=FALSE for factor analysis
fa.parallel(mydata, fa="fa")


## extracting and rotating factors ----

Nfacs <- 6  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="promax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)

Nfacs <- 6  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="varimax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)


## plotting factors ----

load <- fit$loadings[,1:2]
png(file = "outputs/pnad/regressions/PCA/f1_v_f2_2005_test.png",
    width = 800,
    height = 600,
    type = "cairo")
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7)
dev.off()


## factor analysis ----

loads <- fit$loadings
png(file = "outputs/pnad/regressions/PCA/plots/2005_factor_analysis.png",
    width = 800,
    height = 600,
    type = "cairo")
fa.diagram(loads)
dev.off()


## export factor loadings ----

dim(fit$loadings)
round(fit$loadings[ 1:15,], 6)
FactorLoadings <- round(fit$loadings[1:14, ], 5)
write.csv(FactorLoadings, file="outputs/pnad/regressions/PCA/loads/2005_FacLoads.csv")


# Cronbach's Alpha (just for the sake of it)

f1 <- mydata[ , c("possui_carteira_assinada", "trabalhou_semana", "contribui_previdencia",
                  "horas_trabalhadas_todos_trabalhos", "idade")]
f2 <- mydata[ , c("renda_mensal_ocupacao_principal_deflacionado", "renda_mensal_todas_fontes_deflacionado")]
f3 <- mydata[ , c("gender_id", "boss_id")]
# f4 <- mydata[ , c("entrep")]
f5 <- mydata[ , c("anos_estudo", "q1_id", "race_id")]
f6 <- mydata[ , c("renda_aposentadoria_deflacionado", "renda_pensao_deflacionado")]
alpha(f1, check.keys=TRUE)$total[1]
alpha(f2, check.keys=TRUE)$total[1]
alpha(f3, check.keys=TRUE)$total[1]
# alpha(f4, check.keys=TRUE)$total[1]
alpha(f5, check.keys=TRUE)$total[1]
alpha(f6, check.keys=TRUE)$total[1]

sink()




### 2006 ----

## loading db ----

pnad.indv.2006 <- readRDS("db/pnad/pnad_indv_2006.rds") %>% 
  dplyr::mutate(boss_id = case_when(
    cat_condicao_domicilio == "chefe" ~ 1,
    is.na(cat_condicao_domicilio) ~ NA_real_,
    TRUE ~ 0)) %>% 
  dplyr::select(
    c(
      # dinheiro:
      "renda_mensal_ocupacao_principal_deflacionado",
      "renda_mensal_todas_fontes_deflacionado",
      "renda_aposentadoria_deflacionado",
      "renda_pensao_deflacionado",
      "q1_id",
      # características individuais:
      "idade",
      "anos_estudo",
      "gender_id",
      "race_id",
      # trabalho:
      "trabalhou_semana",
      # "tinha_outro_trabalho",
      "possui_carteira_assinada",
      "contribui_previdencia",
      "horas_trabalhadas_todos_trabalhos",
      "entrep",
      # "posicao_ocupacao",
      # socioeconomia:
      "boss_id",
      # "condicao_familia"
    )
  ) %>% 
  dplyr::mutate_if(
    is.character, as.numeric
  ) %>% 
  mutate_at(
    vars(
      starts_with(c("renda",
                    "tinha",
                    "possui",
                    "contribui",
                    "horas",
                    "entrep"))), ~replace(., is.na(.), 0)) %>% 
  mutate_at(
    vars(
      q1_id), ~replace(., is.na(.), 0)) %>% 
  dplyr::`filter`(
    idade > 23
  )


## evaluating correlation matrix ----

dat <- na.omit(pnad.indv.2006)
mydata <- dat
summary(mydata)
colSums(is.na(mydata))
rcorr.adjust(mydata)
write.csv(cor(mydata)>0.8, file="outputs/pnad/regressions/PCA/corr/2006_Suspect_Correlations.csv")
write.csv(cor(mydata), file="outputs/pnad/regressions/PCA/corr/2006_Correlation_Values.csv")


## KMO test ----

sink(file = "outputs/pnad/regressions/PCA/reports/2006_PNAD_PCA.md", append = FALSE)
KMO(mydata)
round( KMO(mydata)$MSA, 2 )


## Bartlett’s test ----

cortest.bartlett(mydata)


## scree plot and parallel analysis ----

ev <- eigen(cor(mydata)) # get eigenvalues
ev$values
scree(mydata, pc = FALSE)  # Use pc=FALSE for factor analysis
fa.parallel(mydata, fa="fa")


## extracting and rotating factors ----

Nfacs <- 6  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="promax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)

Nfacs <- 6  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="varimax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)


## plotting factors ----

load <- fit$loadings[,1:2]
png(file = "outputs/pnad/regressions/PCA/f1_v_f2_2006_test.png",
    width = 800,
    height = 600,
    type = "cairo")
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7)
dev.off()


## factor analysis ----

loads <- fit$loadings
png(file = "outputs/pnad/regressions/PCA/plots/2006_factor_analysis.png",
    width = 800,
    height = 600,
    type = "cairo")
fa.diagram(loads)
dev.off()


## export factor loadings ----

dim(fit$loadings)
round(fit$loadings[ 1:15,], 6)
FactorLoadings <- round(fit$loadings[1:14, ], 5)
write.csv(FactorLoadings, file="outputs/pnad/regressions/PCA/loads/2006_FacLoads.csv")


# Cronbach's Alpha (just for the sake of it)

f1 <- mydata[ , c("possui_carteira_assinada", "trabalhou_semana", "contribui_previdencia",
                  "horas_trabalhadas_todos_trabalhos", "idade")]
f2 <- mydata[ , c("renda_mensal_ocupacao_principal_deflacionado", "renda_mensal_todas_fontes_deflacionado")]
f3 <- mydata[ , c("gender_id", "boss_id")]
# f4 <- mydata[ , c("entrep")]
f5 <- mydata[ , c("anos_estudo", "q1_id", "race_id")]
f6 <- mydata[ , c("renda_aposentadoria_deflacionado", "renda_pensao_deflacionado")]
alpha(f1, check.keys=TRUE)$total[1]
alpha(f2, check.keys=TRUE)$total[1]
alpha(f3, check.keys=TRUE)$total[1]
# alpha(f4, check.keys=TRUE)$total[1]
alpha(f5, check.keys=TRUE)$total[1]
alpha(f6, check.keys=TRUE)$total[1]

sink()




### 2007 ----

## loading db ----

pnad.indv.2007 <- readRDS("db/pnad/pnad_indv_2007.rds") %>% 
  dplyr::mutate(boss_id = case_when(
    cat_condicao_domicilio == "chefe" ~ 1,
    is.na(cat_condicao_domicilio) ~ NA_real_,
    TRUE ~ 0)) %>% 
  dplyr::select(
    c(
      # dinheiro:
      "renda_mensal_ocupacao_principal_deflacionado",
      "renda_mensal_todas_fontes_deflacionado",
      "renda_aposentadoria_deflacionado",
      "renda_pensao_deflacionado",
      "q1_id",
      # características individuais:
      "idade",
      "anos_estudo",
      "gender_id",
      "race_id",
      # trabalho:
      "trabalhou_semana",
      # "tinha_outro_trabalho",
      "possui_carteira_assinada",
      "contribui_previdencia",
      "horas_trabalhadas_todos_trabalhos",
      "entrep",
      # "posicao_ocupacao",
      # socioeconomia:
      "boss_id",
      # "condicao_familia"
    )
  ) %>% 
  dplyr::mutate_if(
    is.character, as.numeric
  ) %>% 
  mutate_at(
    vars(
      starts_with(c("renda",
                    "tinha",
                    "possui",
                    "contribui",
                    "horas",
                    "entrep"))), ~replace(., is.na(.), 0)) %>% 
  mutate_at(
    vars(
      q1_id), ~replace(., is.na(.), 0)) %>% 
  dplyr::`filter`(
    idade > 23
  )


## evaluating correlation matrix ----

dat <- na.omit(pnad.indv.2007)
mydata <- dat
summary(mydata)
colSums(is.na(mydata))
rcorr.adjust(mydata)
write.csv(cor(mydata)>0.8, file="outputs/pnad/regressions/PCA/corr/2007_Suspect_Correlations.csv")
write.csv(cor(mydata), file="outputs/pnad/regressions/PCA/corr/2007_Correlation_Values.csv")


## KMO test ----

sink(file = "outputs/pnad/regressions/PCA/reports/2007_PNAD_PCA.md", append = FALSE)
KMO(mydata)
round( KMO(mydata)$MSA, 2 )


## Bartlett’s test ----

cortest.bartlett(mydata)


## scree plot and parallel analysis ----

ev <- eigen(cor(mydata)) # get eigenvalues
ev$values
scree(mydata, pc = FALSE)  # Use pc=FALSE for factor analysis
fa.parallel(mydata, fa="fa")


## extracting and rotating factors ----

Nfacs <- 6  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="promax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)

Nfacs <- 6  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="varimax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)


## plotting factors ----

load <- fit$loadings[,1:2]
png(file = "outputs/pnad/regressions/PCA/f1_v_f2_2007_test.png",
    width = 800,
    height = 600,
    type = "cairo")
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7)
dev.off()


## factor analysis ----

loads <- fit$loadings
png(file = "outputs/pnad/regressions/PCA/plots/2007_factor_analysis.png",
    width = 800,
    height = 600,
    type = "cairo")
fa.diagram(loads)
dev.off()


## export factor loadings ----

dim(fit$loadings)
round(fit$loadings[ 1:15,], 6)
FactorLoadings <- round(fit$loadings[1:14, ], 5)
write.csv(FactorLoadings, file="outputs/pnad/regressions/PCA/loads/2007_FacLoads.csv")


# Cronbach's Alpha (just for the sake of it)

f1 <- mydata[ , c("possui_carteira_assinada", "trabalhou_semana", "contribui_previdencia",
                  "horas_trabalhadas_todos_trabalhos", "idade")]
f2 <- mydata[ , c("renda_mensal_ocupacao_principal_deflacionado", "renda_mensal_todas_fontes_deflacionado")]
f3 <- mydata[ , c("gender_id", "boss_id")]
# f4 <- mydata[ , c("entrep")]
f5 <- mydata[ , c("anos_estudo", "q1_id", "race_id")]
f6 <- mydata[ , c("renda_aposentadoria_deflacionado", "renda_pensao_deflacionado")]
alpha(f1, check.keys=TRUE)$total[1]
alpha(f2, check.keys=TRUE)$total[1]
alpha(f3, check.keys=TRUE)$total[1]
# alpha(f4, check.keys=TRUE)$total[1]
alpha(f5, check.keys=TRUE)$total[1]
alpha(f6, check.keys=TRUE)$total[1]

sink()




### 2008 ----

## loading db ----

pnad.indv.2008 <- readRDS("db/pnad/pnad_indv_2008.rds") %>% 
  dplyr::mutate(boss_id = case_when(
    cat_condicao_domicilio == "chefe" ~ 1,
    is.na(cat_condicao_domicilio) ~ NA_real_,
    TRUE ~ 0)) %>% 
  dplyr::select(
    c(
      # dinheiro:
      "renda_mensal_ocupacao_principal_deflacionado",
      "renda_mensal_todas_fontes_deflacionado",
      "renda_aposentadoria_deflacionado",
      "renda_pensao_deflacionado",
      "q1_id",
      # características individuais:
      "idade",
      "anos_estudo",
      "gender_id",
      "race_id",
      # trabalho:
      "trabalhou_semana",
      # "tinha_outro_trabalho",
      "possui_carteira_assinada",
      "contribui_previdencia",
      "horas_trabalhadas_todos_trabalhos",
      "entrep",
      # "posicao_ocupacao",
      # socioeconomia:
      "boss_id",
      # "condicao_familia"
    )
  ) %>% 
  dplyr::mutate_if(
    is.character, as.numeric
  ) %>% 
  mutate_at(
    vars(
      starts_with(c("renda",
                    "tinha",
                    "possui",
                    "contribui",
                    "horas",
                    "entrep"))), ~replace(., is.na(.), 0)) %>% 
  mutate_at(
    vars(
      q1_id), ~replace(., is.na(.), 0)) %>% 
  dplyr::`filter`(
    idade > 23
  )


## evaluating correlation matrix ----

dat <- na.omit(pnad.indv.2008)
mydata <- dat
summary(mydata)
colSums(is.na(mydata))
rcorr.adjust(mydata)
write.csv(cor(mydata)>0.8, file="outputs/pnad/regressions/PCA/corr/2008_Suspect_Correlations.csv")
write.csv(cor(mydata), file="outputs/pnad/regressions/PCA/corr/2008_Correlation_Values.csv")


## KMO test ----

sink(file = "outputs/pnad/regressions/PCA/reports/2008_PNAD_PCA.md", append = FALSE)
KMO(mydata)
round( KMO(mydata)$MSA, 2 )


## Bartlett’s test ----

cortest.bartlett(mydata)


## scree plot and parallel analysis ----

ev <- eigen(cor(mydata)) # get eigenvalues
ev$values
scree(mydata, pc = FALSE)  # Use pc=FALSE for factor analysis
fa.parallel(mydata, fa="fa")


## extracting and rotating factors ----

Nfacs <- 6  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="promax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)

Nfacs <- 6  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="varimax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)


## plotting factors ----

load <- fit$loadings[,1:2]
png(file = "outputs/pnad/regressions/PCA/f1_v_f2_2008_test.png",
    width = 800,
    height = 600,
    type = "cairo")
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7)
dev.off()


## factor analysis ----

loads <- fit$loadings
png(file = "outputs/pnad/regressions/PCA/plots/2008_factor_analysis.png",
    width = 800,
    height = 600,
    type = "cairo")
fa.diagram(loads)
dev.off()


## export factor loadings ----

dim(fit$loadings)
round(fit$loadings[ 1:15,], 6)
FactorLoadings <- round(fit$loadings[1:14, ], 5)
write.csv(FactorLoadings, file="outputs/pnad/regressions/PCA/loads/2008_FacLoads.csv")


# Cronbach's Alpha (just for the sake of it)

f1 <- mydata[ , c("possui_carteira_assinada", "trabalhou_semana", "contribui_previdencia",
                  "horas_trabalhadas_todos_trabalhos", "idade")]
f2 <- mydata[ , c("renda_mensal_ocupacao_principal_deflacionado", "renda_mensal_todas_fontes_deflacionado")]
f3 <- mydata[ , c("gender_id", "boss_id")]
# f4 <- mydata[ , c("entrep")]
f5 <- mydata[ , c("anos_estudo", "q1_id", "race_id")]
f6 <- mydata[ , c("renda_aposentadoria_deflacionado", "renda_pensao_deflacionado")]
alpha(f1, check.keys=TRUE)$total[1]
alpha(f2, check.keys=TRUE)$total[1]
alpha(f3, check.keys=TRUE)$total[1]
# alpha(f4, check.keys=TRUE)$total[1]
alpha(f5, check.keys=TRUE)$total[1]
alpha(f6, check.keys=TRUE)$total[1]

sink()





### 2009 ----

## loading db ----

pnad.indv.2009 <- readRDS("db/pnad/pnad_indv_2009.rds") %>% 
  dplyr::mutate(boss_id = case_when(
    cat_condicao_domicilio == "chefe" ~ 1,
    is.na(cat_condicao_domicilio) ~ NA_real_,
    TRUE ~ 0)) %>% 
  dplyr::select(
    c(
      # dinheiro:
      "renda_mensal_ocupacao_principal_deflacionado",
      "renda_mensal_todas_fontes_deflacionado",
      "renda_aposentadoria_deflacionado",
      "renda_pensao_deflacionado",
      "q1_id",
      # características individuais:
      "idade",
      "anos_estudo",
      "gender_id",
      "race_id",
      # trabalho:
      "trabalhou_semana",
      # "tinha_outro_trabalho",
      "possui_carteira_assinada",
      "contribui_previdencia",
      "horas_trabalhadas_todos_trabalhos",
      "entrep",
      # "posicao_ocupacao",
      # socioeconomia:
      "boss_id",
      # "condicao_familia"
    )
  ) %>% 
  dplyr::mutate_if(
    is.character, as.numeric
  ) %>% 
  mutate_at(
    vars(
      starts_with(c("renda",
                    "tinha",
                    "possui",
                    "contribui",
                    "horas",
                    "entrep"))), ~replace(., is.na(.), 0)) %>% 
  mutate_at(
    vars(
      q1_id), ~replace(., is.na(.), 0)) %>% 
  dplyr::`filter`(
    idade > 23
  )


## evaluating correlation matrix ----

dat <- na.omit(pnad.indv.2009)
mydata <- dat
summary(mydata)
colSums(is.na(mydata))
rcorr.adjust(mydata)
write.csv(cor(mydata)>0.8, file="outputs/pnad/regressions/PCA/corr/2009_Suspect_Correlations.csv")
write.csv(cor(mydata), file="outputs/pnad/regressions/PCA/corr/2009_Correlation_Values.csv")


## KMO test ----

sink(file = "outputs/pnad/regressions/PCA/reports/2009_PNAD_PCA.md", append = FALSE)
KMO(mydata)
round( KMO(mydata)$MSA, 2 )


## Bartlett’s test ----

cortest.bartlett(mydata)


## scree plot and parallel analysis ----

ev <- eigen(cor(mydata)) # get eigenvalues
ev$values
scree(mydata, pc = FALSE)  # Use pc=FALSE for factor analysis
fa.parallel(mydata, fa="fa")


## extracting and rotating factors ----

Nfacs <- 6  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="promax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)

Nfacs <- 6  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="varimax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)


## plotting factors ----

load <- fit$loadings[,1:2]
png(file = "outputs/pnad/regressions/PCA/f1_v_f2_2009_test.png",
    width = 800,
    height = 600,
    type = "cairo")
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7)
dev.off()


## factor analysis ----

loads <- fit$loadings
png(file = "outputs/pnad/regressions/PCA/plots/2009_factor_analysis.png",
    width = 800,
    height = 600,
    type = "cairo")
fa.diagram(loads)
dev.off()


## export factor loadings ----

dim(fit$loadings)
round(fit$loadings[ 1:15,], 6)
FactorLoadings <- round(fit$loadings[1:14, ], 5)
write.csv(FactorLoadings, file="outputs/pnad/regressions/PCA/loads/2009_FacLoads.csv")


# Cronbach's Alpha (just for the sake of it)

f1 <- mydata[ , c("possui_carteira_assinada", "trabalhou_semana", "contribui_previdencia",
                  "horas_trabalhadas_todos_trabalhos", "idade")]
f2 <- mydata[ , c("renda_mensal_ocupacao_principal_deflacionado", "renda_mensal_todas_fontes_deflacionado")]
f3 <- mydata[ , c("gender_id", "boss_id")]
# f4 <- mydata[ , c("entrep")]
f5 <- mydata[ , c("anos_estudo", "q1_id", "race_id")]
f6 <- mydata[ , c("renda_aposentadoria_deflacionado", "renda_pensao_deflacionado")]
alpha(f1, check.keys=TRUE)$total[1]
alpha(f2, check.keys=TRUE)$total[1]
alpha(f3, check.keys=TRUE)$total[1]
# alpha(f4, check.keys=TRUE)$total[1]
alpha(f5, check.keys=TRUE)$total[1]
alpha(f6, check.keys=TRUE)$total[1]

sink()




### 2011 - 2015 ----

### 2011 ----

## loading db ----

pnad.indv.2011 <- readRDS("db/pnad/pnad_indv_2011.rds") %>% 
  dplyr::mutate(boss_id = case_when(
    cat_condicao_domicilio == "chefe" ~ 1,
    is.na(cat_condicao_domicilio) ~ NA_real_,
    TRUE ~ 0)) %>% 
  dplyr::select(
    c(
      # dinheiro:
      "renda_mensal_ocupacao_principal_deflacionado",
      "renda_mensal_todas_fontes_deflacionado",
      "renda_aposentadoria_deflacionado",
      "renda_pensao_deflacionado",
      "q1_id",
      # características individuais:
      "idade",
      "anos_estudo",
      "gender_id",
      "race_id",
      # trabalho:
      "trabalhou_semana",
      # "tinha_outro_trabalho",
      "possui_carteira_assinada",
      "contribui_previdencia",
      "horas_trabalhadas_todos_trabalhos",
      "entrep",
      # "posicao_ocupacao",
      # socioeconomia:
      "boss_id",
      # "condicao_familia"
    )
  ) %>% 
  dplyr::mutate_if(
    is.character, as.numeric
  ) %>% 
  mutate_at(
    vars(
      starts_with(c("renda",
                    "tinha",
                    "possui",
                    "contribui",
                    "horas",
                    "entrep"))), ~replace(., is.na(.), 0)) %>% 
  mutate_at(
    vars(
      q1_id), ~replace(., is.na(.), 0)) %>% 
  dplyr::`filter`(
    idade > 23
  )


## evaluating correlation matrix ----

dat <- na.omit(pnad.indv.2011)
mydata <- dat
summary(mydata)
colSums(is.na(mydata))
rcorr.adjust(mydata)
write.csv(cor(mydata)>0.8, file="outputs/pnad/regressions/PCA/corr/2011_Suspect_Correlations.csv")
write.csv(cor(mydata), file="outputs/pnad/regressions/PCA/corr/2011_Correlation_Values.csv")


## KMO test ----

sink(file = "outputs/pnad/regressions/PCA/reports/2011_PNAD_PCA.md", append = FALSE)
KMO(mydata)
round( KMO(mydata)$MSA, 2 )


## Bartlett’s test ----

cortest.bartlett(mydata)


## scree plot and parallel analysis ----

ev <- eigen(cor(mydata)) # get eigenvalues
ev$values
scree(mydata, pc = FALSE)  # Use pc=FALSE for factor analysis
fa.parallel(mydata, fa="fa")


## extracting and rotating factors ----

Nfacs <- 6  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="promax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)

Nfacs <- 6  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="varimax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)


## plotting factors ----

load <- fit$loadings[,1:2]
png(file = "outputs/pnad/regressions/PCA/f1_v_f2_2011_test.png",
    width = 800,
    height = 600,
    type = "cairo")
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7)
dev.off()


## factor analysis ----

loads <- fit$loadings
png(file = "outputs/pnad/regressions/PCA/plots/2011_factor_analysis.png",
    width = 800,
    height = 600,
    type = "cairo")
fa.diagram(loads)
dev.off()


## export factor loadings ----

dim(fit$loadings)
round(fit$loadings[ 1:15,], 6)
FactorLoadings <- round(fit$loadings[1:14, ], 5)
write.csv(FactorLoadings, file="outputs/pnad/regressions/PCA/loads/2011_FacLoads.csv")


# Cronbach's Alpha (just for the sake of it)

f1 <- mydata[ , c("possui_carteira_assinada", "trabalhou_semana", "contribui_previdencia",
                  "horas_trabalhadas_todos_trabalhos", "idade")]
f2 <- mydata[ , c("renda_mensal_ocupacao_principal_deflacionado", "renda_mensal_todas_fontes_deflacionado")]
f3 <- mydata[ , c("gender_id", "boss_id")]
# f4 <- mydata[ , c("entrep")]
f5 <- mydata[ , c("anos_estudo", "q1_id", "race_id")]
f6 <- mydata[ , c("renda_aposentadoria_deflacionado", "renda_pensao_deflacionado")]
alpha(f1, check.keys=TRUE)$total[1]
alpha(f2, check.keys=TRUE)$total[1]
alpha(f3, check.keys=TRUE)$total[1]
# alpha(f4, check.keys=TRUE)$total[1]
alpha(f5, check.keys=TRUE)$total[1]
alpha(f6, check.keys=TRUE)$total[1]

sink()




### 2012 ----

## loading db ----

pnad.indv.2012 <- readRDS("db/pnad/pnad_indv_2012.rds") %>% 
  dplyr::mutate(boss_id = case_when(
    cat_condicao_domicilio == "chefe" ~ 1,
    is.na(cat_condicao_domicilio) ~ NA_real_,
    TRUE ~ 0)) %>% 
  dplyr::select(
    c(
      # dinheiro:
      "renda_mensal_ocupacao_principal_deflacionado",
      "renda_mensal_todas_fontes_deflacionado",
      "renda_aposentadoria_deflacionado",
      "renda_pensao_deflacionado",
      "q1_id",
      # características individuais:
      "idade",
      "anos_estudo",
      "gender_id",
      "race_id",
      # trabalho:
      "trabalhou_semana",
      # "tinha_outro_trabalho",
      "possui_carteira_assinada",
      "contribui_previdencia",
      "horas_trabalhadas_todos_trabalhos",
      "entrep",
      # "posicao_ocupacao",
      # socioeconomia:
      "boss_id",
      # "condicao_familia"
    )
  ) %>% 
  dplyr::mutate_if(
    is.character, as.numeric
  ) %>% 
  mutate_at(
    vars(
      starts_with(c("renda",
                    "tinha",
                    "possui",
                    "contribui",
                    "horas",
                    "entrep"))), ~replace(., is.na(.), 0)) %>% 
  mutate_at(
    vars(
      q1_id), ~replace(., is.na(.), 0)) %>% 
  dplyr::`filter`(
    idade > 23
  )


## evaluating correlation matrix ----

dat <- na.omit(pnad.indv.2012)
mydata <- dat
summary(mydata)
colSums(is.na(mydata))
rcorr.adjust(mydata)
write.csv(cor(mydata)>0.8, file="outputs/pnad/regressions/PCA/corr/2012_Suspect_Correlations.csv")
write.csv(cor(mydata), file="outputs/pnad/regressions/PCA/corr/2012_Correlation_Values.csv")


## KMO test ----

sink(file = "outputs/pnad/regressions/PCA/reports/2012_PNAD_PCA.md", append = FALSE)
KMO(mydata)
round( KMO(mydata)$MSA, 2 )


## Bartlett’s test ----

cortest.bartlett(mydata)


## scree plot and parallel analysis ----

ev <- eigen(cor(mydata)) # get eigenvalues
ev$values
scree(mydata, pc = FALSE)  # Use pc=FALSE for factor analysis
fa.parallel(mydata, fa="fa")


## extracting and rotating factors ----

Nfacs <- 5  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="promax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)

Nfacs <- 5  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="varimax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)


## plotting factors ----

load <- fit$loadings[,1:2]
png(file = "outputs/pnad/regressions/PCA/f1_v_f2_2012_test.png",
    width = 800,
    height = 600,
    type = "cairo")
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7)
dev.off()


## factor analysis ----

loads <- fit$loadings
png(file = "outputs/pnad/regressions/PCA/plots/2012_factor_analysis.png",
    width = 800,
    height = 600,
    type = "cairo")
fa.diagram(loads)
dev.off()


## export factor loadings ----

dim(fit$loadings)
round(fit$loadings[ 1:15,], 6)
FactorLoadings <- round(fit$loadings[1:14, ], 5)
write.csv(FactorLoadings, file="outputs/pnad/regressions/PCA/loads/2012_FacLoads.csv")


# Cronbach's Alpha (just for the sake of it)

# f1 <- mydata[ , c("possui_carteira_assinada", "trabalhou_semana", "contribui_previdencia",
#                   "horas_trabalhadas_todos_trabalhos", "idade")]
# f2 <- mydata[ , c("renda_mensal_ocupacao_principal_deflacionado", "renda_mensal_todas_fontes_deflacionado")]
# f3 <- mydata[ , c("gender_id", "boss_id")]
# # f4 <- mydata[ , c("entrep")]
# f5 <- mydata[ , c("anos_estudo", "q1_id", "race_id")]
# f6 <- mydata[ , c("renda_aposentadoria_deflacionado", "renda_pensao_deflacionado")]
# alpha(f1, check.keys=TRUE)$total[1]
# alpha(f2, check.keys=TRUE)$total[1]
# alpha(f3, check.keys=TRUE)$total[1]
# # alpha(f4, check.keys=TRUE)$total[1]
# alpha(f5, check.keys=TRUE)$total[1]
# alpha(f6, check.keys=TRUE)$total[1]

sink()




### 2013 ----

## loading db ----

pnad.indv.2013 <- readRDS("db/pnad/pnad_indv_2013.rds") %>% 
  dplyr::mutate(boss_id = case_when(
    cat_condicao_domicilio == "chefe" ~ 1,
    is.na(cat_condicao_domicilio) ~ NA_real_,
    TRUE ~ 0)) %>% 
  dplyr::select(
    c(
      # dinheiro:
      "renda_mensal_ocupacao_principal_deflacionado",
      "renda_mensal_todas_fontes_deflacionado",
      "renda_aposentadoria_deflacionado",
      "renda_pensao_deflacionado",
      "q1_id",
      # características individuais:
      "idade",
      "anos_estudo",
      "gender_id",
      "race_id",
      # trabalho:
      "trabalhou_semana",
      # "tinha_outro_trabalho",
      "possui_carteira_assinada",
      "contribui_previdencia",
      "horas_trabalhadas_todos_trabalhos",
      "entrep",
      # "posicao_ocupacao",
      # socioeconomia:
      "boss_id",
      # "condicao_familia"
    )
  ) %>% 
  dplyr::mutate_if(
    is.character, as.numeric
  ) %>% 
  mutate_at(
    vars(
      starts_with(c("renda",
                    "tinha",
                    "possui",
                    "contribui",
                    "horas",
                    "entrep"))), ~replace(., is.na(.), 0)) %>% 
  mutate_at(
    vars(
      q1_id), ~replace(., is.na(.), 0)) %>% 
  dplyr::`filter`(
    idade > 23
  )


## evaluating correlation matrix ----

dat <- na.omit(pnad.indv.2013)
mydata <- dat
summary(mydata)
colSums(is.na(mydata))
rcorr.adjust(mydata)
write.csv(cor(mydata)>0.8, file="outputs/pnad/regressions/PCA/corr/2013_Suspect_Correlations.csv")
write.csv(cor(mydata), file="outputs/pnad/regressions/PCA/corr/2013_Correlation_Values.csv")


## KMO test ----

sink(file = "outputs/pnad/regressions/PCA/reports/2013_PNAD_PCA.md", append = FALSE)
KMO(mydata)
round( KMO(mydata)$MSA, 2 )


## Bartlett’s test ----

cortest.bartlett(mydata)


## scree plot and parallel analysis ----

ev <- eigen(cor(mydata)) # get eigenvalues
ev$values
scree(mydata, pc = FALSE)  # Use pc=FALSE for factor analysis
fa.parallel(mydata, fa="fa")


## extracting and rotating factors ----

Nfacs <- 5  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="promax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)

Nfacs <- 5  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="varimax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)


## plotting factors ----

load <- fit$loadings[,1:2]
png(file = "outputs/pnad/regressions/PCA/f1_v_f2_2013_test.png",
    width = 800,
    height = 600,
    type = "cairo")
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7)
dev.off()


## factor analysis ----

loads <- fit$loadings
png(file = "outputs/pnad/regressions/PCA/plots/2013_factor_analysis.png",
    width = 800,
    height = 600,
    type = "cairo")
fa.diagram(loads)
dev.off()


## export factor loadings ----

dim(fit$loadings)
round(fit$loadings[ 1:15,], 6)
FactorLoadings <- round(fit$loadings[1:14, ], 5)
write.csv(FactorLoadings, file="outputs/pnad/regressions/PCA/loads/2013_FacLoads.csv")


# Cronbach's Alpha (just for the sake of it)

# f1 <- mydata[ , c("possui_carteira_assinada", "trabalhou_semana", "contribui_previdencia",
#                   "horas_trabalhadas_todos_trabalhos", "idade")]
# f2 <- mydata[ , c("renda_mensal_ocupacao_principal_deflacionado", "renda_mensal_todas_fontes_deflacionado")]
# f3 <- mydata[ , c("gender_id", "boss_id")]
# # f4 <- mydata[ , c("entrep")]
# f5 <- mydata[ , c("anos_estudo", "q1_id", "race_id")]
# f6 <- mydata[ , c("renda_aposentadoria_deflacionado", "renda_pensao_deflacionado")]
# alpha(f1, check.keys=TRUE)$total[1]
# alpha(f2, check.keys=TRUE)$total[1]
# alpha(f3, check.keys=TRUE)$total[1]
# # alpha(f4, check.keys=TRUE)$total[1]
# alpha(f5, check.keys=TRUE)$total[1]
# alpha(f6, check.keys=TRUE)$total[1]

sink()




### 2014 ----

## loading db ----

pnad.indv.2014 <- readRDS("db/pnad/pnad_indv_2014.rds") %>% 
  dplyr::mutate(boss_id = case_when(
    cat_condicao_domicilio == "chefe" ~ 1,
    is.na(cat_condicao_domicilio) ~ NA_real_,
    TRUE ~ 0)) %>% 
  dplyr::select(
    c(
      # dinheiro:
      "renda_mensal_ocupacao_principal_deflacionado",
      "renda_mensal_todas_fontes_deflacionado",
      "renda_aposentadoria_deflacionado",
      "renda_pensao_deflacionado",
      "q1_id",
      # características individuais:
      "idade",
      "anos_estudo",
      "gender_id",
      "race_id",
      # trabalho:
      "trabalhou_semana",
      # "tinha_outro_trabalho",
      "possui_carteira_assinada",
      "contribui_previdencia",
      "horas_trabalhadas_todos_trabalhos",
      "entrep",
      # "posicao_ocupacao",
      # socioeconomia:
      "boss_id",
      # "condicao_familia"
    )
  ) %>% 
  dplyr::mutate_if(
    is.character, as.numeric
  ) %>% 
  mutate_at(
    vars(
      starts_with(c("renda",
                    "tinha",
                    "possui",
                    "contribui",
                    "horas",
                    "entrep"))), ~replace(., is.na(.), 0)) %>% 
  mutate_at(
    vars(
      q1_id), ~replace(., is.na(.), 0)) %>% 
  dplyr::`filter`(
    idade > 23
  )


## evaluating correlation matrix ----

dat <- na.omit(pnad.indv.2014)
mydata <- dat
summary(mydata)
colSums(is.na(mydata))
rcorr.adjust(mydata)
write.csv(cor(mydata)>0.8, file="outputs/pnad/regressions/PCA/corr/2014_Suspect_Correlations.csv")
write.csv(cor(mydata), file="outputs/pnad/regressions/PCA/corr/2014_Correlation_Values.csv")


## KMO test ----

sink(file = "outputs/pnad/regressions/PCA/reports/2014_PNAD_PCA.md", append = FALSE)
KMO(mydata)
round( KMO(mydata)$MSA, 2 )


## Bartlett’s test ----

cortest.bartlett(mydata)


## scree plot and parallel analysis ----

ev <- eigen(cor(mydata)) # get eigenvalues
ev$values
scree(mydata, pc = FALSE)  # Use pc=FALSE for factor analysis
fa.parallel(mydata, fa="fa")


## extracting and rotating factors ----

Nfacs <- 6  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="promax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)

Nfacs <- 6  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="varimax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)


## plotting factors ----

load <- fit$loadings[,1:2]
png(file = "outputs/pnad/regressions/PCA/f1_v_f2_2014_test.png",
    width = 800,
    height = 600,
    type = "cairo")
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7)
dev.off()


## factor analysis ----

loads <- fit$loadings
png(file = "outputs/pnad/regressions/PCA/plots/2014_factor_analysis.png",
    width = 800,
    height = 600,
    type = "cairo")
fa.diagram(loads)
dev.off()


## export factor loadings ----

dim(fit$loadings)
round(fit$loadings[ 1:15,], 6)
FactorLoadings <- round(fit$loadings[1:14, ], 5)
write.csv(FactorLoadings, file="outputs/pnad/regressions/PCA/loads/2014_FacLoads.csv")


# Cronbach's Alpha (just for the sake of it)

f1 <- mydata[ , c("possui_carteira_assinada", "trabalhou_semana", "contribui_previdencia",
                  "horas_trabalhadas_todos_trabalhos", "idade")]
f2 <- mydata[ , c("renda_mensal_ocupacao_principal_deflacionado", "renda_mensal_todas_fontes_deflacionado")]
f3 <- mydata[ , c("gender_id", "boss_id")]
# f4 <- mydata[ , c("entrep")]
f5 <- mydata[ , c("anos_estudo", "q1_id", "race_id")]
f6 <- mydata[ , c("renda_aposentadoria_deflacionado", "renda_pensao_deflacionado")]
alpha(f1, check.keys=TRUE)$total[1]
alpha(f2, check.keys=TRUE)$total[1]
alpha(f3, check.keys=TRUE)$total[1]
# alpha(f4, check.keys=TRUE)$total[1]
alpha(f5, check.keys=TRUE)$total[1]
alpha(f6, check.keys=TRUE)$total[1]

sink()




### 2015 ----

## loading db ----

pnad.indv.2015 <- readRDS("db/pnad/pnad_indv_2015.rds") %>% 
  dplyr::mutate(boss_id = case_when(
    cat_condicao_domicilio == "chefe" ~ 1,
    is.na(cat_condicao_domicilio) ~ NA_real_,
    TRUE ~ 0)) %>% 
  dplyr::select(
    c(
      # dinheiro:
      "renda_mensal_ocupacao_principal",
      "renda_mensal_todas_fontes",
      "renda_aposentadoria",
      "renda_pensao",
      "q1_id",
      # características individuais:
      "idade",
      "anos_estudo",
      "gender_id",
      "race_id",
      # trabalho:
      "trabalhou_semana",
      # "tinha_outro_trabalho",
      "possui_carteira_assinada",
      "contribui_previdencia",
      "horas_trabalhadas_todos_trabalhos",
      "entrep",
      # "posicao_ocupacao",
      # socioeconomia:
      "boss_id",
      # "condicao_familia"
    )
  ) %>% 
  dplyr::mutate_if(
    is.character, as.numeric
  ) %>% 
  mutate_at(
    vars(
      starts_with(c("renda",
                    "tinha",
                    "possui",
                    "contribui",
                    "horas",
                    "entrep"))), ~replace(., is.na(.), 0)) %>% 
  mutate_at(
    vars(
      q1_id), ~replace(., is.na(.), 0)) %>% 
  dplyr::`filter`(
    idade > 23
  )


## evaluating correlation matrix ----

dat <- na.omit(pnad.indv.2015)
mydata <- dat
summary(mydata)
colSums(is.na(mydata))
rcorr.adjust(mydata)
write.csv(cor(mydata)>0.8, file="outputs/pnad/regressions/PCA/corr/2015_Suspect_Correlations.csv")
write.csv(cor(mydata), file="outputs/pnad/regressions/PCA/corr/2015_Correlation_Values.csv")


## KMO test ----

sink(file = "outputs/pnad/regressions/PCA/reports/2015_PNAD_PCA.md", append = FALSE)
KMO(mydata)
round( KMO(mydata)$MSA, 2 )


## Bartlett’s test ----

cortest.bartlett(mydata)


## scree plot and parallel analysis ----

ev <- eigen(cor(mydata)) # get eigenvalues
ev$values
scree(mydata, pc = FALSE)  # Use pc=FALSE for factor analysis
fa.parallel(mydata, fa="fa")


## extracting and rotating factors ----

Nfacs <- 5  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="promax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)

Nfacs <- 5  # This is for four factors. You can change this as needed.
fit <- factanal(mydata, Nfacs, rotation="varimax")
print(fit, digits=2, cutoff=0.3, sort=TRUE)


## plotting factors ----

load <- fit$loadings[,1:2]
png(file = "outputs/pnad/regressions/PCA/f1_v_f2_2015_test.png",
    width = 800,
    height = 600,
    type = "cairo")
plot(load,type="n") # set up plot
text(load,labels=names(mydata),cex=.7)
dev.off()


## factor analysis ----

loads <- fit$loadings
png(file = "outputs/pnad/regressions/PCA/plots/2015_factor_analysis.png",
    width = 800,
    height = 600,
    type = "cairo")
fa.diagram(loads)
dev.off()


## export factor loadings ----

dim(fit$loadings)
round(fit$loadings[ 1:15,], 6)
FactorLoadings <- round(fit$loadings[1:14, ], 5)
write.csv(FactorLoadings, file="outputs/pnad/regressions/PCA/loads/2015_FacLoads.csv")


# Cronbach's Alpha (just for the sake of it)

# f1 <- mydata[ , c("possui_carteira_assinada", "trabalhou_semana", "contribui_previdencia",
#                   "horas_trabalhadas_todos_trabalhos", "idade")]
# f2 <- mydata[ , c("renda_mensal_ocupacao_principal_deflacionado", "renda_mensal_todas_fontes_deflacionado")]
# f3 <- mydata[ , c("gender_id", "boss_id")]
# # f4 <- mydata[ , c("entrep")]
# f5 <- mydata[ , c("anos_estudo", "q1_id", "race_id")]
# f6 <- mydata[ , c("renda_aposentadoria_deflacionado", "renda_pensao_deflacionado")]
# alpha(f1, check.keys=TRUE)$total[1]
# alpha(f2, check.keys=TRUE)$total[1]
# alpha(f3, check.keys=TRUE)$total[1]
# # alpha(f4, check.keys=TRUE)$total[1]
# alpha(f5, check.keys=TRUE)$total[1]
# alpha(f6, check.keys=TRUE)$total[1]

sink()




