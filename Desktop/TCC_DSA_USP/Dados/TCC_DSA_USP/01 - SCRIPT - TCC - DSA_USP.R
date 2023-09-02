################################################################################
#               INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","correlation","see",
             "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
             "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp",
             "equatiomatic")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Instala pacotes
install.packages("cluster", dependencies = TRUE) 

# Pacote para cluster
library(cluster)

# Cria clusters de do cliente 
cluster <- kmeans(
  x = base_scale2,
  centers = 4
)

base$cluster <- as.factor (cluster$cluster) 

# Visualiza cliente e grupo
gruposClientes

################################################################################
#                                CLUSTER                                       #
#                                                                              #
################################################################################
#Carregamento dos dados
library(readxl)
base <- read_excel("base.xls")

base_corr <- read_excel("base_corr.xls")

base_scale = scale(base_corr)
head(base_scale, n=3)

# Número ótimo de clusters
library(factoextra)
fviz_nbclust(base_scale, kmeans, method = "wss", linecolor = "blue", barcolor = "green")+
  geom_vline(xintercept = 4, linetype = 2)

# Clusterização k-means
set.seed(123)
km.res=kmeans(base_scale, 4, nstart=25)
print(km.res)

aggregate(base_scale, by=list(cluster=km.res$cluster), mean)

base_scale2 = cbind(base_scale, cluster=km.res$cluster)
head(base_scale2)

km.res$centers

# Vizualizando os clusters
library(ggplot2)
library(factoextra)

fviz_cluster(km.res, data=base_scale2,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme = theme_bw()
)

D = dist(base_scale) # matriz de distancias
m = as.matrix(D)
fviz_dist(D, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

################################################################################
#                             REGRESSÃO LINEAR                                 #
#                                                                              #
################################################################################
library(readxl)
base <- read_excel("base.xls")

################################################################################
#                OBSERVANDO OS DADOS CARREGADOS DO DATASET                     #
################################################################################
base %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 22)

#Estatísticas univariadas
summary(base)

################################################################################
#                            REGRESSÃO LINEAR MÚLTIPLA                         #
#                                                                              #
################################################################################

################################################################################
#                             ESTUDO DAS CORRELAÇÕES                           #
################################################################################
#Visualizando a base de dados
library(readxl)
base_corr <- read_excel("base_corr.xls")
View(base_corr)

base_corr %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 24)

# install.packages("metan")
library(metan)

corr_plot(base_corr, col.up.panel = "red", col.smooth = "red", col.point = "black", col.diag = "yellow", col.sign = "green", col.lw.panel = "blue")

################################################################################
#    ESTIMANDO UM MODELO MÚLTIPLO COM AS VARIÁVEIS DA BASE DE DADOS            #
################################################################################
#Estimando a Regressão Múltipla

modelo <- lm(formula = base$taxa_mag_pop ~ + base$tempo_medio_decisao + base$tcltarget + base$ftt + base$g1 + base$porte + base$h2 + base$cluster + base$taxa_escolarizacao + base$taxa_nasc_vivos + base$pib,
                    data = base)

#Parâmetros do modelo
summary(modelo)
confint(modelo, level = 0.95) # siginificância de 5%

#Estimando a Regressão Múltipla
modelo_step <- step(object = modelo)

#Parâmetros do modelo
summary(modelo_step)

options(scipen = 0)

# install.packages("stargazer")
library(stargazer)

stargazer(modelo, type = "text", align = TRUE, title = "Resultados da Regressão Linear Múltipla", dep.var.labels = "Magistrados por 100 mil habitantes", digits = 2, out = "Modelo.html")

#Outro modo de apresentar os outputs do modelo - função 'summ' do pacote 'jtools'
summ(modelo, confint = T, digits = 3, ci.width = .95)
export_summs(modelo, scale = F, digits = 5)

#Salvando os fitted values na base de dados
base$fitted <- modelo_step$fitted.values

base %>%
  mutate(residuos = modelo$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo$residuals),
                            sd = sd(modelo$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  labs(title = "Histograma dos Resíduos")+
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

###############################################
#Exploração visual do Corruption Perception Index para cada um dos países
base %>%
  group_by(porte) %>%
  ggplot(aes(x = as.numeric(porte), y = taxa_mag_pop, label = porte)) +
  geom_point(aes(x = porte, y = taxa_mag_pop), color = "#FDE725FF", alpha = 0.5, size = 5) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  labs(x = "porte",
       y = "Corruption Perception Index") +
  geom_text_repel() +
  theme_bw()


################################################################################
#        TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE          #
#                             SHAPIRO-FRANCIA                                  #
################################################################################
#Shapiro-Wilk: n <= 30
## shapiro.test(modelo_linear$residuals)

#Shapiro-Francia: n > 30
sf.test(modelo$residuals) #função 'sf.test' do pacote 'nortest'

#Histograma dos resíduos do modelo OLS linear
base %>%
  mutate(residuos = modelo$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo$residuals),
                            sd = sd(modelo$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

#Visualização do comportamento dos resíduos em função dos fitted values do
#do modelo linear, com destaque para as distribuições das variáveis
#(pacote 'ggside')
base %>%
  ggplot(aes(x = modelo$fitted.values, y = modelo$residuals)) +
  geom_point(color = "#FDE725FF", size = 2.5) +
  geom_smooth(aes(color = "Fitted Values"),
              method = "lm", formula = y ~ x, se = F, size = 2) +
  geom_xsidedensity(aes(y = after_stat(density)),
                    alpha = 0.5,
                    size = 1,
                    position = "stack") +
  geom_ysidedensity(aes(x = after_stat(density)),
                    alpha = 0.5,
                    size = 1,
                    position = "stack") +
  xlab("Fitted Values") +
  ylab("Resíduos") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() +
  theme(ggside.panel.scale.x = 0.4,
        ggside.panel.scale.y = 0.4)

#Visualizando os fitted values dos dois modelos no dataset
base %>%
  select(taxa_mag_pop, fitted) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

grafico <- ggplot(base, aes(x = Tribunal)) +
  geom_point(aes(y = taxa_mag_pop), color = "green", size = 3) +
  geom_point(aes(y = fitted), color = "red", size = 3) +
  labs(title = "Valores Reais vs. Previsões",
       x = NULL,  # Remover rótulo do eixo x
       y = "Valor") +
  scale_color_manual(values = c("green", "red"),
                     labels = c("Valores Reais", "Previsões")) +
  guides(color = guide_legend(title = "Legenda")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotacionar eixo x


# Exibir o gráfico
print(grafico)
  
#Ajustes dos modelos: valores previstos (fitted values) X valores reais

################################################################################
#        TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE          #
#                             SHAPIRO-FRANCIA                                  #
################################################################################
#Shapiro-Francia: n > 30
sf.test(modelo_step$residuals) #função 'sf.test' do pacote 'nortest'

##################################################################################
#            DIAGNÓSTICO DE MULTICOLINEARIDADE EM MODELOS DE REGRESSÃO           #
#                   EXEMPLO 06 - CARREGAMENTO DA BASE DE DADOS                   #
##################################################################################
#Diagnóstico de multicolinearidade (Variance Inflation Factor e Tolerance)
ols_vif_tol(modelo_step)

##################################################################################
#           DIAGNÓSTICO DE HETEROCEDASTICIDADE EM MODELOS DE REGRESSÃO           #
#                   EXEMPLO 07 - CARREGAMENTO DA BASE DE DADOS                   #
##################################################################################

##################################################################################
#                       ESTIMAÇÃO DO MODELO DE REGRESSÃO E                       #
#                       DIAGNÓSTICO DE HETEROCEDASTICIDADE                       #                                                            
##################################################################################
#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(modelo_step)
#função 'ols_test_breusch_pagan' do pacote 'olsrr'
#Presença de heterocedasticidade -> omissão de variável(is) explicativa(s) relevante(s)

#H0 do teste: ausência de heterocedasticidade.
#H1 do teste: heterocedasticidade, ou seja, correlação entre resíduos e uma ou mais
#variáveis explicativas, o que indica omissão de variável relevante!
