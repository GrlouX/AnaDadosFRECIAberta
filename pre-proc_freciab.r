# Pré-processamento dos dados das companhias abertas no Brasil em 2021

# Pacote para manipulação de strings
library(stringr) 

# Carregamento dos dados das tabelas relevantes para a análise

# Descompacta todos os arquivos extraídos da fonte dos dados
fl<-unzip("./dados_brutos/fre_cia_aberta_2021.zip", exdir = "./dados_brutos")

# Tabela com as variáveis descritivas básicas das empresas
df1<-read.csv("./dados_brutos/fre_cia_aberta_2021.csv",
                sep=";", dec=".")

# Tabela referente à estrutura de capital das empresas
df2<-read.csv("./dados_brutos/fre_cia_aberta_distribuicao_capital_2021.csv",
                sep=";", dec=".")

# Tabela referente às relações das empresas: 6 linhas não carregadas de 10.211*
df3<-read.csv("./dados_brutos/fre_cia_aberta_relacao_subordinacao_2021.csv",
                sep=";", dec=".")

# Tabela referente às remunerações dos dirigentes nas empresas
# Necessário eliminar todas as aspas duplas do arquivo antes de carregar!
df4<-read.csv("./dados_brutos/fre_cia_aberta_remuneracao_total_orgao_2021.csv",
                sep=";", dec=".")

# Limpeza dos dados

# Data frame 1

str(df1) # estrutura com 3.060 observações de 9 variáveis

head(df1)  

# Variáveis básicas de interesse (todas carregadas como tipo texto)
#CNPJ_CIA: CNPJ da companhia (coluna 1)
#DT_REFER: Data de referência do documento (coluna 2)
#DENOM_CIA: Nome empresarial da companhia (coluna 4)

colSums(is.na(df1[ ,c(1,2,4)]))

colSums(df1[ ,c(1,2,4)]=="") # Não há valores ausentes

# Verificação da consistência dos dados (1 CNPJ_CIA : 1 DENOM_CIA)

sum(str_length(df1[ ,1])==18) # Todos os CNPJs possuem comprimento igual a 18

length(unique(df1$CNPJ_CIA))

length(unique(df1$DENOM_CIA))

length(unique(df1$CNPJ_CIA, df1$DENOM_CIA)) # 721 CIAs com CPNJs distintos

DF_bas <- df1

# Data frame 2

str(df2) # estrutura com 731 observações de 14 variáveis

head(df2)

# Variáveis de interesse (todas do tipo numérico, exceto CNPJ_Companhia - texto)
#CNPJ_Companhia (coluna 1) 
#Quantidade_Acionistas_PF (coluna 5)
#Quantidade_Acionistas_Investidores_Institucionais (coluna 7)
#Quantidade_Acionistas_PJ (coluna 6) 
#Quantidade_Acoes_Ordinarias_Circulacao (coluna 8)
#Quantidade_Acoes_Preferenciais_Circulacao (coluna 10) 
#Quantidade_Total_Acoes_Circulacao (coluna 12)

# Verificação de valores ausentes
colSums(is.na(df2[ ,c(1,5:8,10,12)])) # Não há

# Verificação da consistência e integridade dos dados (CNPJ_Companhia)

sum(str_length(df2[ ,1])==18) # Consistência garantida

length(unique(df2$CNPJ_Companhia)) # Garantia da integridade referencial

# Verificação de outliers nas quantidades de acionistas (pelo critério de Tukey)

DIQ = quantile(df2[ ,5],0.75)[[1]] - quantile(df2[ ,5],0.25)[[1]]

sum(df2[ ,5]>quantile(df2[ ,5],0.75)[[1]]+1.5*DIQ)/nrow(df2)*100 #16.7% superior

sum(df2[ ,5]<quantile(df2[ ,5],0.25)[[1]]-1.5*DIQ)/nrow(df2)*100 #0% inferior

DIQ = quantile(df2[ ,6],0.75)[[1]] - quantile(df2[ ,6],0.25)[[1]]

sum(df2[ ,6]>quantile(df2[ ,6],0.75)[[1]]+1.5*DIQ)/nrow(df2)*100 #14,0% superior

sum(df2[ ,6]<quantile(df2[ ,6],0.25)[[1]]-1.5*DIQ)/nrow(df2)*100 #0% inferior

DIQ = quantile(df2[ ,7],0.75)[[1]] - quantile(df2[ ,7],0.25)[[1]]

sum(df2[ ,7]>quantile(df2[ ,7],0.75)[[1]]+1.5*DIQ)/nrow(df2)*100 #17,6% superior

sum(df2[ ,7]<quantile(df2[ ,7],0.25)[[1]]-1.5*DIQ)/nrow(df2)*100 #0% inferior

# Verificação de outliers nos tipos de ações (pelo critério de Tukey)

DIQ = quantile(df2[ ,8],0.75)[[1]] - quantile(df2[ ,8],0.25)[[1]]

sum(df2[ ,8]>quantile(df2[ ,8],0.75)[[1]]+1.5*DIQ)/nrow(df2)*100 #16.4% superior

sum(df2[ ,8]<quantile(df2[ ,8],0.25)[[1]]-1.5*DIQ)/nrow(df2)*100 #0% inferior

DIQ = quantile(df2[ ,10],0.75)[[1]] - quantile(df2[ ,10],0.25)[[1]]

sum(df2[ ,10]>quantile(df2[ ,10],0.75)[[1]]+1.5*DIQ)/nrow(df2)*100 #22,3% superior

sum(df2[ ,10]<quantile(df2[ ,10],0.25)[[1]]-1.5*DIQ)/nrow(df2)*100 #0% inferior

DIQ = quantile(df2[ ,12],0.75)[[1]] - quantile(df2[ ,12],0.25)[[1]]

sum(df2[ ,12]>quantile(df2[ ,12],0.75)[[1]]+1.5*DIQ)/nrow(df2)*100 #17,1% superior

sum(df2[ ,12]<quantile(df2[ ,12],0.25)[[1]]-1.5*DIQ)/nrow(df2)*100 #0% inferior

rm(DIQ)

# Observação: os outlliers não serão eliminados, pois é de interessse estudá-los
# no contexto da estrutura do mercado financeiro nacional.

DF_cap <- df2

# Data frame 3

str(df3) # estrutura com 10.205* observações de 16 variáveis

head(df3) 

# Variáveis de interesse (todas carregadas como tipo texto)
#CNPJ_Companhia (coluna 1)
#Tipo_Pessoa_Relacionada (coluna 11)
#Documento_Pessoa_Relacionada (coluna 12)
#Cargo_Pessoa_Relacionada (coluna 13)
#Categoria_Pessoa_Relacionada (coluna 14)
#Tipo_Relacao (coluna 15)

colSums(is.na(df3[ ,c(1,11:15)]))

colSums(df3[ ,c(1,11:15)]=="")

# Percentuais de valores ausentes: 0%, 0.41%, 6.37%, 6.22%, 0.73%, 0.67%
round(colSums(df3[ ,c(1,11:15)]=="")/nrow(df3[ ,c(1,11:15)])*100, 6) 

# Como as linhas com valores ausentes representam pouco mais de 10% do total e a
# imputação de valores poderia gerar vieses indesejados de representação,
# opta-se pela exclusão dos registros encontrados.
df3 <- df3[-c(union(union(union(which(df3[ ,11]==""),which(df3[ ,12]=="")),
     union(which(df3[ ,13]==""),which(df3[ ,14]==""))),which(df3[ ,15]==""))), ]

# Eliminação de 1.303 linhas não criou novas inconsistências, 
# gerando resultado com 8.902 observações das 16 variáveis
colSums(df3=="")

# Verificação da consistência e integridade nos novos dados (CNPJ_Companhia)

sum(str_length(df3[ ,1])==18) # Consistência garantida

length(unique(df3$CNPJ_Companhia)) # Integridade referencial(287 CIAs distintas)

DF_rel <- df3

# Data frame 4

str(df4) # estrutura com 6.036 observações de 26 variáveis

head(df4)

# Variáveis de interesse:
#CNPJ_Companhia - texto (coluna 1)
#Total_Remuneracao - numérica com 2 casas decimais (coluna 7),
#Orgao_Administracao - texto (coluna 8)
#Numero_Membros - numérica com 2 casas decimais** (coluna 9), 
#Total_Remuneracao_Orgao - numérica com 2 casas decimais (coluna 10)
#Numero_Membros_Remunerados - numérica com 2 casas decimais** (coluna 11)
#**Valores provavelmente resultantes do cálculo de médias 

colSums(is.na(df4[ ,c(1,7:11)]))

colSums(df4[ ,c(1,7:11)]=="")

# Como há apenas 0.33% de valores ausentes da variável do tipo texto em questão,
# opta-se pela exclusão dos registros pelo mesmo argumento do caso anterior.
round(sum(df4$Orgao_Administracao=="")/length(df4$Orgao_Administracao)*100, 6)

df4 <- df4[-c(which(df4$Orgao_Administracao=="")), ]

# Eliminação das 20 linhas não criou novas inconsistências, 
# gerando resultado com 6016 observações das 26 variáveis
colSums(df4=="")

# Verificação da consistência e integridade dos dados (CNPJ_Companhia)

sum(str_length(df4[ ,1])==18) # Consistência garantida

length(unique(df4$CNPJ_Companhia)) # Integridade referencial(651 CIAs distintas)

# Verificação de outliers nos números de membros (pelo critério de Tukey)

DIQ = quantile(df4[ ,9],0.75)[[1]] - quantile(df4[ ,9],0.25)[[1]]

sum(df4[ ,9]>quantile(df4[ ,9],0.75)[[1]]+1.5*DIQ)/nrow(df4)*100 #4.1% superior

sum(df4[ ,9]<quantile(df4[ ,9],0.25)[[1]]-1.5*DIQ)/nrow(df4)*100 #0% inferior

DIQ = quantile(df4[ ,11],0.75)[[1]] - quantile(df4[ ,11],0.25)[[1]]

sum(df4[ ,11]>quantile(df4[ ,11],0.75)[[1]]+1.5*DIQ)/nrow(df4)*100 #1.4% superior

sum(df4[ ,11]<quantile(df4[ ,11],0.25)[[1]]-1.5*DIQ)/nrow(df4)*100 #0% inferior

# Verificação de outliers nas remunerações (pelo critério de Tukey)

DIQ = quantile(df4[ ,7],0.75)[[1]] - quantile(df4[ ,7],0.25)[[1]]

sum(df4[ ,7]>quantile(df4[ ,7],0.75)[[1]]+1.5*DIQ)/nrow(df4)*100 #9.5% superior

sum(df4[ ,7]<quantile(df4[ ,7],0.25)[[1]]-1.5*DIQ)/nrow(df4)*100 #0% inferior

DIQ = quantile(df4[ ,10],0.75)[[1]] - quantile(df4[ ,10],0.25)[[1]]

sum(df4[ ,10]>quantile(df4[ ,10],0.75)[[1]]+1.5*DIQ)/nrow(df4)*100 #12.4% superior

sum(df4[ ,10]<quantile(df4[ ,10],0.25)[[1]]-1.5*DIQ)/nrow(df4)*100 #0% inferior

rm(DIQ)

# Observação: os outliers não serão eliminados, pois é de interessse estudá-los
# no contexto do mercado financeiro no Brasil.

DF_rem <- df4

# Conjuntos de dados preparados após as modificações efetuadas

write.csv(DF_bas, "./dados_prep/dfbas_ciab.csv", row.names=F)
write.csv(DF_cap, "./dados_prep/dfcap_ciab.csv", row.names=F)
write.csv(DF_rel, "./dados_prep/dfrel_ciab.csv", row.names=F)
write.csv(DF_rem, "./dados_prep/dfrem_ciab.csv", row.names=F)

rm(fl,df1,df2,df3,df4)

save.image('var-prep_freciab.RData')
