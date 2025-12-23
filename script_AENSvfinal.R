library(tidyverse)   # inclui dplyr, purrr, stringr, readr, ggplot2...
library(readxl)      # para ler ficheiros Excel
library(mice)        # imputação de valores ausentes
library(psych)       # estatísticas, PCA
library(corrplot)    # visualização de correlações
library(ggrepel)
library(VIM)
library(naniar)
library(writexl)
library(plotly)
library(sf)
library(cluster)
library(mclust)
library(RColorBrewer)

##################################
# CONFIGURAÇÃO DAS PASTAS DE DADOS
##################################

# Guardar pasta original
pasta_original <- getwd()

# Guardar caminhos das pastas com variaveis input e profile
caminho_pasta_ine_inputs <- file.path(getwd(), "variaveis input", "ine_xls")
caminho_pasta_ine_profile <- file.path(getwd(), "variaveis profile", "ine_xls")
caminho_pasta_profile <- file.path(getwd(), "variaveis profile")

##################################
# FUNÇÃO DE IMPORTAÇÃO E FILTRAGEM
##################################

processar_ine_xls <- function(nome_ficheiro) {
  
  message(paste("-> A processar:", substring(nome_ficheiro, 1, 50)))
  
  # Tenta ler o cabeçalho para detetar estrutura
  res_leitura <- tryCatch({
    list(
      dados = read_excel(nome_ficheiro, col_names = FALSE, n_max = 200, .name_repair = "minimal"),
      erro = NULL
    )
  }, error = function(e) {
    return(list(dados = NULL, erro = e))
  })
  
  if (!is.null(res_leitura$erro)) {
    warning(paste("ERRO DE FORMATO (HTML/XLS) em:", nome_ficheiro))
    return(NULL)
  }
  
  raw_data <- res_leitura$dados
  
  # 1. Encontrar a linha "Localização geográfica"
  linha_loc <- which(apply(raw_data, 1, function(x) any(str_detect(x, "Localização geográfica"), na.rm = TRUE)))[1]
  if (is.na(linha_loc)) return(NULL)
  
  # 2. Encontrar a coluna do código (Procurando "PT" ou códigos numéricos)
  coluna_codigo <- NA
  linha_inicio_dados <- NA
  linhas_busca <- (linha_loc + 1):nrow(raw_data)
  
  # Estratégia "PT"
  for (j in 1:ncol(raw_data)) {
    vals <- as.character(raw_data[[j]][linhas_busca])
    idx_pt <- which(vals == "PT")[1]
    if (!is.na(idx_pt)) {
      coluna_codigo <- j
      linha_inicio_dados <- linhas_busca[1] + idx_pt - 1
      break
    }
  }
  
  # Estratégia Numérica (Plano B)
  if (is.na(coluna_codigo)) {
    for (j in 1:min(10, ncol(raw_data))) {
      vals <- as.character(raw_data[[j]][linhas_busca])
      if (any(str_detect(vals, "^[0-9]{2,7}$"), na.rm = TRUE)) {
        coluna_codigo <- j
        linha_inicio_dados <- linhas_busca[which(str_detect(vals, "^[0-9]{2,7}$"))[1]]
        break
      }
    }
  }
  
  if (is.na(coluna_codigo) || is.na(linha_inicio_dados)) return(NULL)
  
  # Importação Final
  linha_cabecalho <- linha_inicio_dados - 1
  df <- read_excel(nome_ficheiro, skip = linha_cabecalho - 1, .name_repair = "unique")
  
  # Limpeza e FILTRAGEM DE MUNICÍPIOS
  idx_nome <- coluna_codigo - 1
  if (idx_nome < 1) idx_nome <- 1
  
  df_final <- df %>%
    rename(
      localizacao = !!names(.)[idx_nome],
      codigo = !!names(.)[coluna_codigo]
    ) %>%
    # Selecionar colunas relevantes
    select(localizacao, codigo, (coluna_codigo + 1):last_col()) %>%
    select(where(~ !all(is.na(.)))) %>%
    filter(!is.na(codigo)) %>%
    
    # Filtrar apenas códigos com >= 4 caracteres (Municípios têm 7)
    # Convertemos para character primeiro para garantir que nchar funciona bem
    filter(nchar(as.character(codigo)) >= 4) %>%
    
    # Criar coluna com os 4 dígitos da esquerda
    mutate(
      codigo = as.character(codigo),
      codigo_municipio = ifelse(
        nchar(codigo) <= 4,
        codigo,
        substr(codigo, nchar(codigo) - 3, nchar(codigo))
      )
    ) %>%
    
    # Remover a coluna original 'codigo' se já não quiseres manter
    select(-codigo) %>%
    
    # --- Remover Colunas Vazias (Limpeza Final) ---
    # Remove qualquer coluna onde TODOS os valores sejam NA
    # Isto é feito agora no fim, para limpar variáveis que ficaram vazias após removermos Portugal/NUTS
    select(where(~ !all(is.na(.)))) %>%
    
    # Limpar espaços no nome do município
    mutate(localizacao = str_trim(localizacao))
  
  return(df_final)
}

#####################
# IMPORTAÇÃO DE DADOS
#####################

# Importar dados variáveis input
ficheiros_input <- list.files(
  caminho_pasta_ine_inputs,
  pattern = "\\.(xls|xlsx)$",
  full.names = TRUE
)

if (length(ficheiros_input) > 0) {
  
  lista_dados_input <- ficheiros_input %>%
    set_names(basename(.)) %>%
    purrr::map(processar_ine_xls) %>%
    purrr::compact()
  
  message("--- Processamento Concluído ---")
  message("Ficheiros processados: ", length(lista_dados_input))
  
  if (length(lista_dados_input) > 0) {
    glimpse(lista_dados_input[[1]])
  }
  
} else {
  stop("Pasta input vazia.")
}

# Importar dados variáveis profile
ficheiros_profile <- list.files(
  caminho_pasta_ine_profile,
  pattern = "\\.(xls|xlsx)$",
  full.names = TRUE
)

if (length(ficheiros_profile) > 0) {
  
  lista_dados_profile <- ficheiros_profile %>%
    set_names(basename(.)) %>%
    purrr::map(processar_ine_xls) %>%
    purrr::compact()
  
  message("--- Processamento Concluído ---")
  message("Ficheiros processados: ", length(lista_dados_profile))
  
  if (length(lista_dados_profile) > 0) {
    glimpse(lista_dados_profile[[1]])
  }
  
} else {
  stop("Pasta profile vazia.")
}

# Importar dados variáveis profile restantes
dados_profile_extra <- read_delim(
  file.path(caminho_pasta_profile, "localizacao_cap_dist.csv"),
  delim = ";"
)

setwd(pasta_original)

#n_distinct(dados_profile_extra$HASC_2)

#n_distinct(dados_profile_extra$CCA_2)

# Mostrar nomes ficheiros importados
names(lista_dados_input)
names(lista_dados_profile)

##################
# LIMPEZA DE DADOS
##################

# INPUT - Converter colunas para numéricas
lista_dados_input <- lapply(lista_dados_input, function(df) {
  cols_para_converter <- setdiff(names(df), c("codigo_municipio", "localizacao"))
  df[cols_para_converter] <- lapply(df[cols_para_converter], function(x) as.numeric(as.character(x)))
  return(df)
})

# INPUT - somar colunas de populaçao por niveis de educacao
lista_dados_input[[19]] <- lista_dados_input[[19]] %>%
  
  mutate(
    
    `Até 1º ciclo` = rowSums(across(2:3), na.rm = TRUE),
    `Até 3º ciclo` = rowSums(across(4:5), na.rm = TRUE),
    `Ensino Superior` = rowSums(across(7:11), na.rm = TRUE)
  )

# INPUT - alterar nomes das colunas
names(lista_dados_input[[3]])[2] <- "Total"
names(lista_dados_input[[9]])[2] <- "Total"
names(lista_dados_input[[11]])[2] <- "Total"
names(lista_dados_input[[16]])[3] <- "Mediana"
names(lista_dados_input[[17]])[2] <- "Total"
names(lista_dados_input[[20]])[2] <- "Total"
names(lista_dados_input[[23]])[2] <- "Total"

# INPUT - remover colunas irrelevantes
lista_dados_input[[5]] <- lista_dados_input[[5]] %>% select(-3, -4, -5, -6, -7, -9, -10)
lista_dados_input[[8]] <- lista_dados_input[[8]] %>% select(-2, -5, -6, -7)
lista_dados_input[[10]] <- lista_dados_input[[10]] %>% select(-3, -4)
lista_dados_input[[15]] <- lista_dados_input[[15]] %>% select(-3, -4)
lista_dados_input[[16]] <- lista_dados_input[[16]] %>% select(-2, -4)
lista_dados_input[[18]] <- lista_dados_input[[18]] %>% select(-2)
lista_dados_input[[19]] <- lista_dados_input[[19]] %>% select(-2, -3, -4, -5, -7, -8, -9, -10, -11)

# PROFILE - adicionar dados_profile_extra A lista_dados_profile
lista_dados_profile[["localizacao_cap_dist"]] <- dados_profile_extra

# PROFILE - remover colunas irrelevantes
lista_dados_profile[[5]] <- lista_dados_profile[[5]] %>% 
  dplyr::select(5, 7, 8,10, 17, 18, 20)

# PROFILE - alterar nomes das colunas
names(lista_dados_profile[[5]])[1] <- "distrito"
names(lista_dados_profile[[5]])[2] <- "localizacao"
names(lista_dados_profile[[5]])[5] <- "interior_litoral"
names(lista_dados_profile[[5]])[6] <- "cap_distrito"
names(lista_dados_profile[[5]])[7] <- "codigo_municipio"
names(lista_dados_profile[[1]])[2] <- "densidade_populacional"
names(lista_dados_profile[[2]])[2] <- "ganho_medio_mensal"
names(lista_dados_profile[[3]])[2] <- "indice_envelhecimento"
names(lista_dados_profile[[4]])[2] <- "populacao"

# PROFILE - juntar todos os dados em dados_profile 
dados_profile <- reduce(lista_dados_profile, full_join, by = "codigo_municipio")

# PROFILE - remover colunas irrelevantes
dados_profile <- dados_profile %>% select(-4, -6, -8, -11)

# PROFILE - alterar dados necessários para numéricos
dados_profile <- dados_profile %>%
  mutate(across(c(2, 4, 5, 6), as.numeric))

# PROFILE - alterar nomes colunas
names(dados_profile)[1]<- "localizacao"

###############################################################
# CRIAR LISTA UNIFORMIZADA DE CODIGO MUNICIPIO E NOME MUNICIPIO
###############################################################

# Empilhar tudo
df_municipios <- bind_rows(lista_dados_input)

# Criar dataframe único, uma linha por código–nome distinto (retiramos casos problemáticos)
df_municipios <- df_municipios %>%
  distinct(codigo_municipio, localizacao)

df_municipios <- df_municipios %>%
  filter(
    !(codigo_municipio == "3101" & localizacao == "Calheta") &
      !(codigo_municipio == "4501" & localizacao == "Calheta") &
      !(codigo_municipio == "4201" & localizacao == "Lagoa")
  )

# Adicionamos os codigos HASC_2 e CCA_2 (útil na criação do mapa)
df_municipios <- df_municipios %>%
  left_join(
    dados_profile %>% select(codigo_municipio, HASC_2),
    by = c("codigo_municipio" = "codigo_municipio")
  )

df_municipios <- df_municipios %>%
  left_join(
    dados_profile %>% select(codigo_municipio, CCA_2),
    by = c("codigo_municipio" = "codigo_municipio")
  )

################################
# LIMPEZA DE DADOS (continuação)
################################

# PROFILE - remover colunas irrelevantes
dados_profile <- dados_profile %>% select(-8, -9)

# PROFILE - Uniformizar nomes de municipios nos dados profile
dados_profile <- dados_profile %>%
  left_join(df_municipios %>% select(codigo_municipio, localizacao), 
            by = "codigo_municipio")

# PROFILE - remover 1a coluna - desnecessária 
dados_profile <- dados_profile %>% select(-1)
names(dados_profile)[9]<- "localizacao"

# PROFILE - Passar colunas de codigo municipio e nome municipio para 1a e 2a coluna
dados_profile <- dados_profile %>%
  select(codigo_municipio, localizacao, everything())

# INPUT - Unir dados em tabela única 
# Criar prefixos com base no nome da lista
if (is.null(names(lista_dados_input))) {
  prefixos <- paste0("tabela", seq_along(lista_dados_input))
  names(lista_dados_input) <- prefixos
} else {
  prefixos <- names(lista_dados_input)
}

# INPUT- Adicionar prefixo a todas as colunas exceto codigo_municipio e localizacao
lista_renomeada <- imap(lista_dados_input, function(df, nome) {
  df %>%
    rename_with(~ paste0(nome, "___", .), 
                .cols = !c(codigo_municipio, localizacao))
})

# INPUT - Guardar coluna localizacao da primeira tabela como referência
localizacao_ref <- lista_renomeada[[1]] %>%
  select(codigo_municipio, localizacao)

# INPUT - Remover coluna localizacao das outras tabelas
outras_tabelas <- imap(lista_renomeada[-1], ~ select(.x, -localizacao))

# INPUT - Left join iterativo
dados_input <- reduce(outras_tabelas,
                      function(x, y) left_join(x, y, by = "codigo_municipio"),
                      .init = select(lista_renomeada[[1]], -localizacao))

# INPUT - Acrescentar a coluna localizacao da primeira tabela
dados_input <- left_join(dados_input, localizacao_ref, by = "codigo_municipio") %>%
  select(codigo_municipio, localizacao, everything())

# INPUT - Mudar tipo de valor da tabela
dados_input <- dados_input %>%
  mutate(
    across(
      .cols = -c(codigo_municipio, localizacao),  # colunas a converter
      .fns = ~ as.numeric(.)
    )
  )

# PROFILE - Mudar tipo de valor da tabela
dados_profile <- dados_profile %>%
  mutate(
    across(
      .cols = -c(codigo_municipio, localizacao, distrito, interior_litoral, cap_distrito),  # colunas a converter
      .fns = ~ as.numeric(.)
    )
  )

##############################################
# Agora já temos dados em formato 'utilizável'
##############################################

summary(dados_input)
summary(dados_profile)

#####################
# TRATAMENTO MISSINGS
#####################

# Ver quantos NAs temos por coluna
dados_input %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%  # conta NAs por coluna
  pivot_longer(everything(), names_to = "coluna", values_to = "n_NA") %>%  # transforma em tabela longa
  print(n = Inf)

# Seleciona colunas com pelo menos um NA
cols_com_na <- names(dados_input)[colSums(is.na(dados_input)) > 0]

df_com_na <- dados_input %>% select(all_of(cols_com_na))

md.pattern(dados_input)

md.pattern(df_com_na)

colnames(df_com_na) <- c("Capacidade\n Aloj. 1000 hab", "Capacidade\n Alojamento", "Despesas proteção\n biodiversidade", "Proporção\n Rede Natura", "Nº de quartos")
md.pattern(df_com_na)

aggr(df_com_na, col=c('white','red'), numbers=TRUE, sortVars=TRUE, cex.axis=.7, gap=3, ylab=c("Percentage of missing data","Missing Data Pattern"))

vis_miss(df_com_na) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

# Imputação pela média
data_imp_mean <- mice(dados_input, method="mean", m=1, maxit=1)

# Dados para utilização futura - com valores de média imputados
data_complete_mean <- complete(data_imp_mean)

########################
# COMPONENTES PRINCIPAIS
########################

#data_complete_mean[, -c(1, 2)]

#scatterplot
pdf("scatterplot_pairs.pdf", width = 15, height = 15)
pairs(data_complete_mean[, -c(1, 2,8)], pch = 19, lower.panel = NULL)
dev.off()

#corrplot
correlation <- cor(data_complete_mean[, -c(1, 2,8)])
pdf("correlation_plot.pdf", width = 15, height = 15)
corrplot.mixed(
  correlation,
  order = "hclust",
  upper = "ellipse",
  tl.pos = "n"   # REMOVE os nomes das variáveis
)
dev.off()

#corrplot melhorado
# limpa nomes (Remover ".xls", "__" e encurtar)
colnames(correlation) <- gsub("\\.xls.*__", ": ", colnames(correlation))
colnames(correlation) <- substr(colnames(correlation), 1, 30) # Corta em 30 caracteres
rownames(correlation) <- colnames(correlation)

pdf("correlation_plot_final.pdf", width = 18, height = 15)
par(mar = c(1, 1, 1, 10)) 
corrplot.mixed(
  correlation,
  order = "hclust",
  upper = "ellipse",
  lower = "number",
  tl.pos = "lt",      # "lt" coloca os nomes à Esquerda (Left) e no Topo (Top)
  tl.col = "black",
  tl.cex = 0.7,       # Diminuímos o tamanho da letra para caberem todos
  tl.srt = 45,        # Roda os nomes do topo em 45 graus
  diag = "u"          # Mantém a diagonal limpa para os nomes não baterem nos desenhos
)

dev.off()

# Bartlett test and KMO
# Input is the correlation matrix

cortest.bartlett(correlation)
# Bartlett tem p value = 0 ou seja, PCA é relevante 

KMO(correlation)
# MSA é 91, indicando grande relevância de PCA


# Extraction and number of components
# Scale the data
dataZ <- scale(data_complete_mean[, -c(1, 2)])

# Assume the number of components (nfactors) = number of variables, i.e., D = 34,
# Always a unrotated extraction
pc34 <- principal(dataZ, nfactors=34, rotate="none", scores=TRUE)  

#The output is a list object that contains too much information 

#Eigenvalues - Variances of the principal components 
round(pc34$values,3)

#Screeplot - Find the elbow
plot(pc34$values,
     type = "b",
     main = "Scree Plot for Competitividade Turística",
     xlab = "Number of Principal Components",
     ylab = "Eigenvalue",
     pch = 19)
# Screeplot aponta para solução de 3 componentes principais

# Eigenvectors - component loadings
# Each value is the correlation between original variable and component,
# It can be thought as the contribution of each original variable to the component (when squared)
# It summarizes the explained variance too.
pc34$loadings

# Critério de Kaiser aponta para 5 componentes (valor próprio > 1)
# Variância explicada aponta para 2 componentes

#Communalities - Sure it's 1! why?
pc34$communality

#Let's extract a 3 component solution
pc3 <- principal(dataZ, nfactors=3, rotate="none")
pc3

#Let's rotate the 3 components using varimax
pc3r <- principal(dataZ, nfactors=3, rotate="varimax")
pc3r$loadings

round(pc3r$communality,2)

#Compute the scores
pc3sc <- principal(dataZ, nfactors=3, rotate="none", scores=TRUE)  
round(pc3sc$scores,3)
mean(pc3sc$scores[,1])
sd(pc3sc$scores[,1])

#Add scores to the data set as new variables
data_complete_mean$pc1 <- pc3sc$scores[,1]
data_complete_mean$pc2 <- pc3sc$scores[,2]
data_complete_mean$pc3 <- pc3sc$scores[,3]

#Save the new data set in excel format

write_xlsx(
  list(PCAscores = data_complete_mean),
  path = "PCA_Turismo.xlsx")

#Depict the scatterplot of PC1 vs PC2
plot(data_complete_mean$pc1, data_complete_mean$pc2, pch = 19,
     xlab="PC1", ylab="PC2", main = "Scores: PC1 vs Pc2")
text(data_complete_mean$pc1, data_complete_mean$pc2-0.1, data_complete_mean[,2]) #(x,y,labels)

#Depict the scatterplot of PC1 vs PC3
plot(data_complete_mean$pc1, data_complete_mean$pc3, pch = 19,
     xlab="PC1", ylab="PC3", main = "Scores: PC1 vs Pc3")
text(data_complete_mean$pc1, data_complete_mean$pc3-0.1, data_complete_mean[,2]) #(x,y,labels)

#Depict the scatterplot of PC2 vs PC3
plot(data_complete_mean$pc2, data_complete_mean$pc3, pch = 19,
     xlab="PC2", ylab="PC3", main = "Scores: PC2 vs Pc3")
text(data_complete_mean$pc2, data_complete_mean$pc3-0.1, data_complete_mean[,2]) #(x,y,labels)

# Criar um dataframe limpo para o ggplot
df_plot <- data.frame(
  PC1 = data_complete_mean$pc1,
  PC2 = data_complete_mean$pc2,
  PC3 = data_complete_mean$pc3,
  Nome = data_complete_mean[,2] # Assumindo que a coluna 2 é o nome do concelho
)

### Gráficos melhorados

# Criar o gráfico PC1 vs PC2
ggplot(df_plot, aes(x = PC1, y = PC2, label = Nome)) +
  geom_point(color = "steelblue", alpha = 0.7) + # Pontos azuis
  
  # Linhas de referência (média zero)
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  
  # A magia do ggrepel: Evita sobreposição de texto
  geom_text_repel(
    max.overlaps = 15,      # Esconde nomes se houver muita confusão
    box.padding = 0.5,
    point.padding = 0.3,
    size = 4
  ) +
  
  labs(
    title = "Económico-Turística e Cultural vs. Intensidade Turística/Vocação Balnear",
    x = "PC1: Dimensão e Capacidade Económico-Turística e Cultural",
    y = "PC2: Intensidade Turística Relativa e a Vocação Balnear"
  ) +
  theme_minimal()

# Criar o gráfico PC1 vs PC3
ggplot(df_plot, aes(x = PC1, y = PC3, label = Nome)) +
  geom_point(color = "steelblue", alpha = 0.7) + # Pontos azuis
  
  # Linhas de referência (média zero)
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  
  # A magia do ggrepel: Evita sobreposição de texto
  geom_text_repel(
    max.overlaps = 15,      # Esconde nomes se houver muita confusão
    box.padding = 0.5,
    point.padding = 0.3,
    size = 4
  ) +
  
  labs(
    title = "Económico-Turística e Cultural vs. Perfil Territorial",
    x = "PC1: Dimensão e Capacidade Económico-Turística",
    y = "PC3: Perfil Socioeconómico e Complementar do Turismo"
  ) +
  theme_minimal()

# Criar o gráfico PC2 vs PC3
ggplot(df_plot, aes(x = PC2, y = PC3, label = Nome)) +
  geom_point(color = "steelblue", alpha = 0.7) + # Pontos azuis
  
  # Linhas de referência (média zero)
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  
  # A magia do ggrepel: Evita sobreposição de texto
  geom_text_repel(
    max.overlaps = 15,      # Esconde nomes se houver muita confusão
    box.padding = 0.5,
    point.padding = 0.3,
    size = 4
  ) +
  
  labs(
    title = "Intensidade Turística/Vocação Balnear vs. Perfil Territorial",
    x = "PC2: Intensidade Turística e Vocação Balnear",
    y = "PC3: Perfil Socioeconómico e Complementar do Turismo"
  ) +
  theme_minimal()


#p <- plot_ly(
#  data = df_plot, # Usando o dataframe criado acima
#  x = ~PC1,
#  y = ~PC2,
#  text = ~Nome,
#  type = 'scatter',
#  mode = 'markers',
#  marker = list(size = 8, color = 'rgba(50, 100, 250, .7)')
#) %>%
#  layout(
#    title = "Económico-Turística e Cultural vs. Intensidade Turística/Vocação Balnear - Interativo",
#    xaxis = list(title = "PC1 - Dimensão e Capacidade Económico-Turística e Cultural"),
#    yaxis = list(title = "PC2 - Intensidade Turística Relativa e a Vocação Balnear")
#  )

#p # Isto vai abrir um gráfico interativo no Viewer

#p2 <- plot_ly(
#  data = df_plot, # Usando o dataframe criado acima
#  x = ~PC1,
#  y = ~PC3,
#  text = ~Nome,
#  type = 'scatter',
#  mode = 'markers',
#  marker = list(size = 8, color = 'rgba(50, 100, 250, .7)')
#) %>%
#  layout(
#    title = "PC1 vs PC3 Interativo",
#    xaxis = list(title = "PC1 - Dimensão e Capacidade Económico-Turística e Cultural"),
#    yaxis = list(title = "PC3 - Perfil Socioeconómico e Complementar do Turismo")
#  )

#p2 # Isto vai abrir um gráfico interativo no Viewer

#################################
# MAPA COM SCORES DOS COMPONENTES
#################################

mapa <- st_read("gadm41_PRT_2.json")

names(mapa)

unique(mapa$NAME_2)

unique(mapa$HASC_2)

df_plot <- df_plot %>%
  left_join(
    df_municipios %>% select(localizacao, HASC_2),
    by = c("Nome" = "localizacao")
  )

mapa_join <- merge(
  mapa,
  df_plot,
  by.x = "HASC_2",
  by.y = "HASC_2",
  all.x = TRUE
)
mapa_cont <- mapa_join %>%
  filter(!NAME_1 %in% c("Azores", "Madeira"))

mapa_madeira <- mapa_join %>%
  filter(NAME_1 %in% c("Madeira"))

mapa_acores <- mapa_join %>%
  filter(NAME_1 %in% c("Azores"))

# Limites globais (calculados com TODAS as regiões)
pc1_lim <- range(mapa_join$PC1, na.rm = TRUE)
pc2_lim <- range(mapa_join$PC2, na.rm = TRUE)
pc3_lim <- range(mapa_join$PC3, na.rm = TRUE)

# -------------------------
# PC1 — Continente / Madeira / Açores
# -------------------------

ggplot(mapa_cont) +
  geom_sf(aes(fill = PC1), color = "white", size = 0.1) +
  scale_fill_viridis_c(limits = pc1_lim, name = "PC1") +
  labs(title = "Dimensão e Capacidade Económico-Turística e Cultural — Continente") +
  theme_minimal()

ggplot(mapa_madeira) +
  geom_sf(aes(fill = PC1), color = "white", size = 0.1) +
  scale_fill_viridis_c(limits = pc1_lim, name = "PC1") +
  coord_sf(
    xlim = c(-17.3, -16.2),
    ylim = c(32.3, 33.2),
    expand = FALSE
  ) +
  labs(
    title = "Dimensão e Capacidade Económico-Turística e Cultural — Madeira"
  ) +
  theme_minimal()

ggplot(mapa_acores) +
  geom_sf(aes(fill = PC1), color = "white", size = 0.1) +
  scale_fill_viridis_c(limits = pc1_lim, name = "PC1") +
  labs(title = "Dimensão e Capacidade Económico-Turística e Cultural — Açores") +
  theme_minimal()

# -------------------------
# PC2 — Continente / Madeira / Açores
# -------------------------

ggplot(mapa_cont) +
  geom_sf(aes(fill = PC2), color = "white", size = 0.1) +
  scale_fill_viridis_c(limits = pc2_lim, name = "PC2") +
  labs(title = "Intensidade Turística Relativa e a Vocação Balnear — Continente") +
  theme_minimal()

ggplot(mapa_madeira) +
  geom_sf(aes(fill = PC2), color = "white", size = 0.1) +
  scale_fill_viridis_c(limits = pc2_lim, name = "PC2") +
  coord_sf(
    xlim = c(-17.3, -16.2),
    ylim = c(32.3, 33.2),
    expand = FALSE
  ) +
  labs(title = "Intensidade Turística Relativa e a Vocação Balnear — Madeira") +
  theme_minimal()

ggplot(mapa_acores) +
  geom_sf(aes(fill = PC2), color = "white", size = 0.1) +
  scale_fill_viridis_c(limits = pc2_lim, name = "PC2") +
  labs(title = "Intensidade Turística Relativa e a Vocação Balnear — Açores") +
  theme_minimal()

# -------------------------
# PC3 — Continente / Madeira / Açores
# -------------------------

ggplot(mapa_cont) +
  geom_sf(aes(fill = PC3), color = "white", size = 0.1) +
  scale_fill_viridis_c(limits = pc3_lim, name = "PC3") +
  labs(title = "Perfil Socioeconómico e Complementar do Turismo — Continente") +
  theme_minimal()

ggplot(mapa_madeira) +
  geom_sf(aes(fill = PC3), color = "white", size = 0.1) +
  scale_fill_viridis_c(limits = pc3_lim, name = "PC3") +
  coord_sf(
    xlim = c(-17.3, -16.2),
    ylim = c(32.3, 33.2),
    expand = FALSE
  ) +
  labs(title = "Perfil Socioeconómico e Complementar do Turismo — Madeira") +
  theme_minimal()

ggplot(mapa_acores) +
  geom_sf(aes(fill = PC3), color = "white", size = 0.1) +
  scale_fill_viridis_c(limits = pc3_lim, name = "PC3") +
  labs(title = "Perfil Socioeconómico e Complementar do Turismo — Açores") +
  theme_minimal()

#############################################
# CLUSTERING (após PCA) - MUNICÍPIOS
#############################################

# Dados para clustering (scores das PCs)
data_clust <- df_plot %>%
  select(PC1, PC2, PC3) %>%
  drop_na()

labels_mun <- df_plot %>%
  filter(complete.cases(PC1, PC2, PC3)) %>%
  pull(Nome)

idx_ok <- which(complete.cases(df_plot$PC1, df_plot$PC2, df_plot$PC3))

std_data <- scale(data_clust)
demodist <- dist(std_data)

head(data_clust)
head(std_data)

# Euclidiana + complete
hc_complete <- hclust(demodist, method = "complete")
plot(hc_complete, labels = labels_mun, hang = -1, main = "Hclust (complete) - PCs")

# Euclidiana + single
hc_single <- hclust(demodist, method = "single")
plot(hc_single, labels = labels_mun, hang = -1, main = "Hclust (single) - PCs")

# Euclidiana + Ward.D2 (recomendado)
hc_ward <- hclust(demodist, method = "ward.D2")
plot(hc_ward, labels = labels_mun, hang = -1, main = "Hclust (ward.D2) - PCs")

### K = 4
# Cortar dendrograma (ex.: k=4 como no exemplo)
k <- 4
groups.k4 <- cutree(hc_ward, k = k)
rect.hclust(hc_ward, k = k, border = "red")
table(groups.k4)

# Perfil médio por cluster (nas PCs)
aggregate(data_clust, list(cluster = groups.k4), mean)

# Silhouette (hclust)
plot(silhouette(groups.k4, demodist), main = "Silhouette - Hclust Ward (k=4)")

### K = 5
# Cortar dendrograma
k <- 5
groups.k5 <- cutree(hc_ward, k = k)
table(groups.k5)

# Perfil médio por cluster (nas PCs)
aggregate(data_clust, list(cluster = groups.k5), mean)

# Silhouette (hclust)
plot(silhouette(groups.k5, demodist), main = "Silhouette - Hclust Ward (k=5)")

### K = 3
# Cortar dendrograma
k <- 3
groups.k3 <- cutree(hc_ward, k = k)
table(groups.k3)

# Perfil médio por cluster (nas PCs)
aggregate(data_clust, list(cluster = groups.k3), mean)

# Silhouette (hclust)
plot(silhouette(groups.k3, demodist), main = "Silhouette - Hclust Ward (k=3)")


# ----------------------------
# ELBOW METHOD (WSS) - KMEANS
# ----------------------------

wssplot <- function(xx, nc = 10, seed = 1234) {
  wss <- numeric(nc)
  # WSS para k=1
  wss[1] <- (nrow(xx) - 1) * sum(apply(xx, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(xx, centers = i, nstart = 50)$withinss)
  }
  plot(1:nc, wss, type = "b",
       xlab = "Number of clusters (k)",
       ylab = "Within-cluster sum of squares (WSS)",
       main = "Elbow method (K-means) - PCs")
}

wssplot(std_data, nc = 10)

# ----------------------------
# K-MEANS (k=4)
# ----------------------------
k <- 4
set.seed(123)
std_data <- scale(data_clust)
kmeans.k4 <- kmeans(std_data, centers = k, nstart = 200)

kmeans.k4$centers
kmeans.k4$size

# Comparar hclust vs kmeans
table(groups.k4, kmeans.k4$cluster)

# Silhouette (kmeans)
plot(silhouette(kmeans.k4$cluster, dist(std_data)), main = "Silhouette - Kmeans (k=4)")

# Lista de atribuições (ordenada)
o <- order(kmeans.k4$cluster)
data.frame(Municipio = labels_mun[o], cluster = kmeans.k4$cluster[o])

# ----------------------------
# K-MEANS (k=5)
# ----------------------------
k <- 5
set.seed(123)
std_data <- scale(data_clust)
kmeans.k5 <- kmeans(std_data, centers = k, nstart = 200)

kmeans.k5$centers
kmeans.k5$size

# Comparar hclust vs kmeans
table(groups.k5, kmeans.k5$cluster)

# Silhouette (kmeans)
plot(silhouette(kmeans.k5$cluster, dist(std_data)), main = "Silhouette - Kmeans (k=5)")

# Lista de atribuições (ordenada)
o <- order(kmeans.k5$cluster)
data.frame(Municipio = labels_mun[o], cluster = kmeans.k5$cluster[o])

# ----------------------------
# PAM (k=4) - mais robusto
# ----------------------------
k <- 4
pam.k4 <- pam(std_data, k = k)
table(groups.k4, pam.k4$clustering)
table(pam.k4$clustering)
aggregate(std_data, list(cluster = pam.k4$clustering), mean)

# Silhouette PAM
plot(silhouette(pam.k4$clustering, dist(std_data)), main = "Silhouette - PAM (k=4)")

# -------------------------
# GMM (k=4)
# -------------------------
k <- 4
set.seed(123)
gmm_k4 <- Mclust(data_clust, G = k)

summary(gmm_k4, parameters = TRUE)

# Plot results
plot(gmm_k4, what = "density")
plot(gmm_k4, what = "uncertainty")

# Some results
results.kG <- Mclust(data_clust) # No specification of the number of clusters
results.kG$modelName          # Optimal selected model
results.kG$G  			          # Optimal number of cluster 
head(results.kG$z, 5)         # Probability to belong to a given cluster
head(results.kG$classification, 5) # Cluster assignment of each observation

# Visualizar a decisão do BIC
plot(results.kG, what = "BIC")

# -------------------------
# GMM (k=6)
# -------------------------
k <- 6
set.seed(123)
gmm_k6 <- Mclust(data_clust, G = k)

summary(gmm_k6, parameters = TRUE)

# Plot results
plot(gmm_k6, what = "density")
plot(gmm_k6, what = "uncertainty")


# -------------------------
# Guardar resultados no df_plot
# -------------------------

cluster_levels4 <- c("1", "2", "3", "4")
cores_clusters4 <- setNames(brewer.pal(4, "Set2"), cluster_levels4)

cluster_levels6 <- c("1", "2", "3", "4", "5","6")
cores_clusters6 <- setNames(brewer.pal(6, "Set2"), cluster_levels6)

df_plot$cluster_hc4 <- NA
df_plot$cluster_km4 <- NA
df_plot$cluster_pam4 <- NA
df_plot$cluster_gmm4 <- NA
df_plot$cluster_gmm6 <- NA
df_plot$gmm_uncertainty4 <- NA
df_plot$gmm_uncertainty6 <- NA

df_plot$cluster_hc4[idx_ok]  <- groups.k4
df_plot$cluster_km4[idx_ok]  <- kmeans.k4$cluster
df_plot$cluster_pam4[idx_ok] <- pam.k4$clustering
df_plot$cluster_gmm4[idx_ok] <- gmm_k4$classification
df_plot$cluster_gmm6[idx_ok] <- gmm_k6$classification
df_plot$gmm_uncertainty4[idx_ok] <- gmm_k4$uncertainty
df_plot$gmm_uncertainty6[idx_ok] <- gmm_k6$uncertainty

df_plot <- df_plot %>%
  mutate(
    cluster_hc4  = factor(cluster_hc4, levels = cluster_levels4),
    cluster_km4  = factor(cluster_km4, levels = cluster_levels4),
    cluster_pam4 = factor(cluster_pam4, levels = cluster_levels4),
    cluster_gmm4 = factor(cluster_gmm4, levels = cluster_levels4),
    cluster_gmm6 = factor(cluster_gmm6, levels = cluster_levels6)
  )


#############################################
# MAPAS DE CLUSTERS — 3 REGIÕES
#############################################

# Função auxiliar
mapa_cluster4 <- function(cluster_col, titulo) {
  mapa_tmp <- mapa %>%
    left_join(df_plot %>% select(HASC_2, !!sym(cluster_col)), by = "HASC_2")
  
  list(
    ggplot(mapa_tmp %>% filter(!NAME_1 %in% c("Azores", "Madeira"))) +
      geom_sf(aes(fill = !!sym(cluster_col)), color = "white", linewidth = 0.1) +
      scale_fill_manual(values = cores_clusters4, drop = FALSE,
                        na.value = "grey90", name = "Cluster") +
      labs(title = paste(titulo, "— Continente")) +
      theme_minimal(),
    
    ggplot(mapa_tmp %>% filter(NAME_1 == "Madeira")) +
      geom_sf(aes(fill = !!sym(cluster_col)), color = "white", linewidth = 0.1) +
      scale_fill_manual(values = cores_clusters4, drop = FALSE,
                        na.value = "grey90", name = "Cluster") +
      coord_sf(
        xlim = c(-17.3, -16.2),
        ylim = c(32.3, 33.2),
        expand = FALSE
      ) +
      labs(title = paste(titulo, "— Madeira")) +
      theme_minimal(),
    
    ggplot(mapa_tmp %>% filter(NAME_1 == "Azores")) +
      geom_sf(aes(fill = !!sym(cluster_col)), color = "white", linewidth = 0.1) +
      scale_fill_manual(values = cores_clusters4, drop = FALSE,
                        na.value = "grey90", name = "Cluster") +
      labs(title = paste(titulo, "— Açores")) +
      theme_minimal()
  )
}

mapa_cluster6 <- function(cluster_col, titulo) {
  mapa_tmp <- mapa %>%
    left_join(df_plot %>% select(HASC_2, !!sym(cluster_col)), by = "HASC_2")
  
  list(
    ggplot(mapa_tmp %>% filter(!NAME_1 %in% c("Azores", "Madeira"))) +
      geom_sf(aes(fill = !!sym(cluster_col)), color = "white", linewidth = 0.1) +
      scale_fill_manual(values = cores_clusters6, drop = FALSE,
                        na.value = "grey90", name = "Cluster") +
      labs(title = paste(titulo, "— Continente")) +
      theme_minimal(),
    
    ggplot(mapa_tmp %>% filter(NAME_1 == "Madeira")) +
      geom_sf(aes(fill = !!sym(cluster_col)), color = "white", linewidth = 0.1) +
      scale_fill_manual(values = cores_clusters6, drop = FALSE,
                        na.value = "grey90", name = "Cluster") +
      coord_sf(
        xlim = c(-17.3, -16.2),
        ylim = c(32.3, 33.2),
        expand = FALSE
      ) +
      labs(title = paste(titulo, "— Madeira")) +
      theme_minimal(),
    
    ggplot(mapa_tmp %>% filter(NAME_1 == "Azores")) +
      geom_sf(aes(fill = !!sym(cluster_col)), color = "white", linewidth = 0.1) +
      scale_fill_manual(values = cores_clusters6, drop = FALSE,
                        na.value = "grey90", name = "Cluster") +
      labs(title = paste(titulo, "— Açores")) +
      theme_minimal()
  )
}

# -------------------------
# MAPAS hclust
# -------------------------
mapa_cluster4("cluster_hc4", "Clusters hclust (k = 4)")

# -------------------------
# MAPAS k-means
# -------------------------
mapa_cluster4("cluster_km4", "Clusters k-means (k = 4)")

# -------------------------
# MAPAS PAM
# -------------------------
mapa_cluster4("cluster_pam4", "Clusters PAM (k = 4)")

# -------------------------
# MAPAS GMM
# -------------------------
mapa_cluster4("cluster_gmm4", "Clusters GMM (k = 4)")
mapa_cluster6("cluster_gmm6", "Clusters GMM (k = 6)")


#left join da tabela df_plot e df_municipios
df_plot <- df_plot %>%
  left_join(
    df_municipios %>% distinct(HASC_2, codigo_municipio),
    by = "HASC_2"
  )

# verificação rápida
sum(is.na(df_plot$codigo_municipio))


#criar tabela com dados profile e cluster id
df_profile_km4 <- dados_profile %>%
  mutate(codigo_municipio = as.character(codigo_municipio)) %>%
  left_join(
    df_plot %>%
      transmute(
        codigo_municipio = as.character(codigo_municipio),
        cluster_km4
      ),
    by = "codigo_municipio"
  ) %>%
  filter(!is.na(cluster_km4)) %>%
  mutate(cluster_km4 = factor(cluster_km4))

#estatisticas das variaveis numericas profile
stats_num_profile_km4 <- df_profile_km4 %>%
  group_by(cluster_km4) %>%
  summarise(
    across(
      3:6,
      list(
        min = ~ min(.x, na.rm = TRUE),
        mean = ~ mean(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    ),
    .groups = "drop"
  )

#contagem valores distintos por categoria para as variaveis categoricas profile
counts_cat_profile_km4 <- df_profile_km4 %>%
  select(cluster_km4, interior_litoral, cap_distrito) %>%
  pivot_longer(
    cols = c(interior_litoral, cap_distrito),
    names_to = "variavel",
    values_to = "categoria"
  ) %>%
  filter(!is.na(categoria)) %>%
  dplyr::count(cluster_km4, variavel, categoria, name = "n") %>%
  arrange(variavel, cluster_km4, desc(n))

counts_cat_profile_km4

stats_pcas_km4 <- df_plot %>%
  filter(!is.na(cluster_km4)) %>%
  group_by(cluster_km4) %>%
  summarise(
    across(
      c(PC1, PC2, PC3),
      list(
        min     = ~ min(.x, na.rm = TRUE),
        max     = ~ max(.x, na.rm = TRUE),
        mean    = ~ mean(.x, na.rm = TRUE),
        median  = ~ median(.x, na.rm = TRUE)
      ),
      .names = "{.col}__{.fn}"
    ),
    n_municipios = n(),
    .groups = "drop"
  )

stats_pcas_km4
