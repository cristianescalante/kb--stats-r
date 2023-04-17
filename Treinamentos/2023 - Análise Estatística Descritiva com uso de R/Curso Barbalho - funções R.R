

Comunidades no Telegram:
https://t.me/rbrasiloficial
https://t.me/R_humanidades

Bases de dados
https://basedosdados.org/
https://basedosdados.github.io/mais/access_data_bq/

Planilha de escolas:
https://docs.google.com/spreadsheets/d/16FfPxPn4b9TH2q0B_oRJDi_yso_55S5LL4jNMlSstlc/edit#gid=11017332

Goolge Classroom
https://classroom.google.com/c/NTYzNTY2NDY3MzA5

	- criar sua assinatura no RStudio Cloud, você conseguirá acessar este link: https://rstudio.cloud/project/2496836. 


Laying out multiple plots on a page
https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html



	# Conteúdo
	- 



# Sobre o Collapse no R
library(doBy)
library(janitor)
dados_socio_economicos <- clean_names(dados_socio_economicos)
dm <- summaryBy(populacao_estimada + pib_per_capita~ uf, FUN=c(mean,sd),  data=dados_socio_economicos)


#Antes, vamos arrumar as tabelas
library(janitor)
names(dados_socio_economicos)
dados_socio_economicos<- clean_names(dados_socio_economicos)


mun_maior_25_pop <-
  dados_socio_economicos %>%
  filter(populacao_estimada > quantile(dados_socio_economicos$populacao_estimada,probs = .25))

#Aprofundando  análise e alterando e alterando uma variável
# dados_socio_economicos %>%
#   filter(uf == "SP") %>%
  slice_max(order_by = populacao_estimada, n=10) %>%
  mutate(nome_do_municipio= reorder(nome_do_municipio,populacao_estimada)) %>%
#  ggplot()+
#  geom_col(aes(y= nome_do_municipio, x= populacao_estimada))

dados_socio_economicos %>%
  filter(uf == "RS") %>%
  slice_max(order_by = pib_per_capita, n=10) %>%
  mutate(nome_do_municipio= reorder(nome_do_municipio,pib_per_capita)) %>%
  ggplot()+
  geom_col(aes(y= nome_do_municipio, x= pib_per_capita))  


df %>%
  filter(sg_ente == "CE" ) %>% 
  filter(funcao == "Educação") %>% 
  group_by(no_ente) %>% 
  summarise(despesa_saude = sum(value)) %>% 
  slice_max(order_by = despesa_saude, n=10) %>% 
  mutate(no_ente = reorder(no_ente, despesa_saude)) %>%
  ggplot() +
  # geom_boxplot(aes(x=no_ente, y= value, fill= no_ente))  
  geom_col(aes(y = no_ente, x = despesa_saude))
  


# ######################################################
# TRANSFORMAÇÃO DE ESCALAS PARA MELHORAR A INTERPRETAÇÃO 

# Distribuição da população estimada dos municípios brasileiros. A variável não foi transformada
dados_socio_economicos %>%
  ggplot() +
  geom_histogram(aes(x=populacao_estimada))+
  scale_x_continuous(labels=function(x) format(x,decimal.mark=",", big.mark = ".", scientific = FALSE))

#No gráfico a escala do eixo x está em log10. Internamente o R calcula o log10 dos valores, mas exibe os valores originais na escola logaritimica
options(scipen = 999)

dados_socio_economicos %>%
  ggplot() +
  geom_histogram(aes(x= populacao_estimada), color= "white") +
  scale_x_log10(labels=function(x) format(x,decimal.mark=",", big.mark = ".", scientific = FALSE))



