###################### MANUSEIO DE DADOS DA POF ################################

#### DEFINIR DIRET�RIO EM QUE EST�O OS ARQUIVOS DA POF
# Neste script ser�o usados o banco do Morador e de Consumo Alimentar

setwd('...')

# Ler inicialmente os arquivos Rds da POF

CONSUMO_ALIMENTAR <- readRDS('CONSUMO_ALIMENTAR.rds') # leitura de base de dados em formato Rds
MORADOR <- readRDS('MORADOR.rds')

#### ETAPA IMPORTANTE: CRIAR ID PARA MERGIR BANCOS

CONSUMO_ALIMENTAR$id <- (CONSUMO_ALIMENTAR$COD_UPA * 100000) + 
  (CONSUMO_ALIMENTAR$NUM_DOM * 1000) + (CONSUMO_ALIMENTAR$NUM_UC) + 
  (CONSUMO_ALIMENTAR$COD_INFOR.MANTE)

MORADOR$id <- (MORADOR$COD_UPA * 100000) + 
  (MORADOR$NUM_DOM * 1000) + (MORADOR$NUM_UC) + 
  (MORADOR$COD_INFORMANTE)

##################### APLICANDO NOMES AOS ALIMENTOS#############################

#### TBCA COM NOMES

library(readxl) # Ler arquivos em excel

tbca <- read_xlsx('...TBCA.xlsx') # ponha o caminho ou defina diret�rio

tbca_cat <- tbca[,1:2] # puxando apenas c�digo e nome do alimento

#### DADOS POR ALIMENTOS - como identificar alimentos espec�ficos

consumo.ali <- merge(CONSUMO_ALIMENTAR, tbca_cat, by.x = 'COD_TBCA', 
                     by.y = 'ID')

## Exemplo com abacate

table(consumo.ali$COD_TBCA[consumo.ali$COD_TBCA == 'C0001C']) # c�digo do abacate
table(consumo.ali$ALIMENTO[consumo.ali$COD_TBCA == 'C0001C']) # verificando se batem as quantidades

## Criando colunas espec�ficas para cada alimento (ex: abacate)

# A fun��o "grepl" retorna um valor TRUE caso em algum trecho do string da vari�vel
# designada, contenha o nome em aspas que estamos procurando

consumo.ali$abacate <- ifelse(grepl('Abacate', consumo.ali$ALIMENTO) == TRUE, 1, 0)

table(consumo.ali$abacate)

################ TESTANDO SEGUNDO ALIMENTOS DO WEBIN�RIO #######################

## ARROZ

consumo.ali$arroz <- ifelse(grepl('Arroz', consumo.ali$ALIMENTO) == TRUE, 1, 0)

table(consumo.ali$arroz) 

## FEIJ�O

consumo.ali$feijao <- ifelse(grepl('Feij�o', consumo.ali$ALIMENTO) == TRUE, 1, 0)

table(consumo.ali$feijao)

## REFRIGERANTE

# No caso do refrigerante, existem bebidas alcoolicas na tabela que possuem
# refrigerante na composi��o. Para efeito de exemplo, considerei como refri tamb�m,
# por�m caso n�o se aplique, sugiro procurar pelos c�digos especificamente

consumo.ali$refri <- ifelse(grepl('Refrigerante', consumo.ali$ALIMENTO) == TRUE, 1, 0)

table(consumo.ali$refri)

#### MERGE PARA CONSUMO E MORADOR (USAR BANCO DE CONSUMO COMO REF)

### Designando vari�veis do morador que desejo

morador2 <- MORADOR[,c(13:15,58)] # idade, sexo, cor de pele e id

dados <- merge(consumo.ali, morador2, by = 'id') # banco com consumo por alimento e informa��es do morador

####### MANIPULANDO DADOS PARA AN�LISE POSTERIOR (opcional)

dados$sel <- NA
dados$sel <- as.factor(ifelse(dados$V0403 < 20 & dados$V0404 == 1, 'Homem adolescente',
                        ifelse(dados$V0403 >= 20 & dados$V0403 < 60 & dados$V0404 == 1, 'Homem adulto',
                        ifelse(dados$V0403 >= 60 & dados$V0404 == 1, 'Homem idoso', 
                        ifelse(dados$V0403 < 20 & dados$V0404 == 2, 'Mulher adolescente',
                        ifelse(dados$V0403 >= 20 & dados$V0403 < 60 & dados$V0404 == 2, 'Mulher adulta',
                               'Mulher idosa'))))))

#### Procedendo table para verificar se as vari�veis foram criadas adequadamente

table(dados$sel)

#### CRIANDO VARI�VEIS DE ENERGIA E PER CAPITA PARA OS ALIMENTOS EXEMPLO

# Energia

dados$arroz.e <- ifelse(dados$arroz == 1, dados$ENERGIA_KCAL, NA)
dados$feijao.e <- ifelse(dados$feijao == 1, dados$ENERGIA_KCAL, NA)
dados$refri.e <- ifelse(dados$refri == 1, dados$ENERGIA_KCAL, NA)

# Quantidade

dados$arroz.q <- ifelse(dados$arroz == 1, dados$QTD, NA)
dados$feijao.q <- ifelse(dados$feijao == 1, dados$QTD, NA)
dados$refri.q <- ifelse(dados$refri == 1, dados$QTD, NA)


###### SUBAMOSTRA DE DADOS COM OS ALIMENTOS ESCOLHIDOS #########################

dados.ex <- dados[,c(1,4,10,34,35,65,71,72,73,77:82)] # vari�veis amostrais e de interesse

######## CRIANDO DESIGN COM PLANO AMOSTRAL PARA AN�LISES DE EXPANS�O ###########

# Aten��o, se n�o tiver instalado pacote Survey, primeiro o fazer

install.packages('survey')
library(survey)

#### Criando design para expandir

pofdesign <- svydesign(id = ~id, strata = ~ESTRATO_POF, 
                       weights = ~PESO_FINAL, data = dados.ex,
                       nest = TRUE)

### Testes

# Exemplo 1 - M�dia de calorias de arroz segundo dia do recordat�rio (QUADRO) e idade+sexo (sel)

svyby(~arroz.e, by = ~interaction(QUADRO, sel), design = pofdesign,
      FUN = svymean, na.rm = T)

# Exemplo 1 - Per capita (gramas) segundo consumo de arroz, feij�o ou refri

svyby(~arroz.q, by = ~interaction(QUADRO, sel), design = pofdesign,
      FUN = svymean, na.rm = TRUE)

#### PARA DEMAIS AN�LISES � IMPORTANTE VERIFICAR AS FUN��ES EXISTENTES NO PACOTE
#### SURVEY. 

####### contato: victorncsilveira@gmail.com