# Projeto YOCHI Company
# Data: 11/05/2022
# DSc: Luiz Almeida Júnior


########## Entendimento dos dados

########## 0.Importação de pacotes
library(readxl)
library(readr)
library(skimr)
library(tidymodels)
library(knitr)
library(kableExtra)
library(correlation) #para estudo de correlacoes

########## 1.Importação da base de dados

base_y <- read_excel("yochi_base.xlsx")
skim_without_charts(base_y )%>% kable %>%  kable_styling(bootstrap_options = "striped",
                              full_width = TRUE,
                              font_size = 12)

#* Podemos observar na coluna n_missing, a contagem desse tipo de valor em cada variável. 


########## 2.Verificação da qualidade dos dados

#* Exceto as variáveis BAD e LOAN, toda as demais possuem valores missing. Não ha indicativo de valores vazios e espaço em branco.
#* 
#* Foi dito pelo gerente Reginaldo que a variável VALUE quando apresenta valores missing, significa que não não há valores em garantia, portanto
#* vamos substituir esse valores por zero.
#* 
#* As demais variáveis serão substituídas por valores criados por ums função.
#* 
#* ###### 2.1. Substituição dos valores NA da variável VALUE

noNA <- function(x){                                              # Criar função para substituir os valores NA pelo valor zero
  ifelse(is.na(x),0,x) 
}                                                                 

base_yt <- base_y %>% mutate(VALUE_T = noNA(base_y$VALUE)) %>%    # Substituir valores NA, criar nova variável VALUE_T em nova base de dados
  select(-VALUE)                                                  # com valores NA transformados

skim(base_yt)%>% kable_styling(bootstrap_options = "striped",
                               full_width = TRUE,
                               font_size = 12)  

#* ###### 2.2. Substituição dos demais valores por uso de função

rec_yoc <- recipe(BAD ~ ., data = base_yt) %>% 
  step_impute_bag(all_predictors()) %>%                           # insere valores usando várias árvores (bagging)
  step_string2factor(all_nominal_predictors())                    # transforma variáveis em nominais em fatores. 

base_final <- bake(prep(rec_yoc), new_data = NULL)

base_final$DELINQ <- round(base_final$DELINQ,0)                   # a função gerou valores contínuos, essa foi uma forma de voltar para discretos.
base_final$NINQ <- round(base_final$NINQ,0)
base_final$YOJ <- round(base_final$YOJ,2)
base_final$DEROG <- round(base_final$DEROG,0)
base_final$CLNO <- round(base_final$CLNO,0)
skim_without_charts(base_final) %>% kable() %>% kable_styling(bootstrap_options = "striped",
                                               full_width = TRUE,
                                               font_size = 12)

#* ###### 2.3. Gravando a nova base de dados.

# write.csv(base_final,"base_final.csv")

#* ###### 2.4. Verificando inconsistência em unidades de medidas nominais

var_fac <- base_final %>% select(is.factor)
var_fac$REASON %>% levels()
var_fac$JOB %>% levels()

#* Não existem inconsistências em relação a padronização dos níveis.
#* Até aqui os problemas com a qualidade dos dados  está resolvido. Alguma inconsistência pode aparecer na fase exploratória
#* dos dados.

########## 3.Explorar os dados

########## 3.1 Análise univariada

#* A análise univariada consiste em analisar as variáveis individualmente em relação aos seguintes aspectos:

#* Classificar a variável quanto a seu tipo: qualitativa (nominal ou ordinal) ou quantitativa (discreta ou contínua)
#* Obter tabelas, gráficos e/ou medidas que resumam a variável

#* Quando se estuda uma variável, o maior intersse do pesquisador é conhecer o comportamento dessa variável, 
#* analisando a ocorrência de suas possíveis realizações. Nesse sentido, as distribuições de frequência serão
#* o principal recurso que utilizaremos para resumir uma única variável.

#* 3.1.1. Variável Qualitativa Nominal

#* Para a variável qualitativa podemos utilizar:
  
#* Uma tabela de frequências (absolutas e/ou relativas)
#* Um gráfico de barras ou de setores
#* A “moda”, i.e. o valor que ocorre com maior frequência
#* Por ser uma variável qualitativa, para obter a distribuição de frequência desta variável, basta contarmos 
#* quantas vezes ocorre cada categoria (ou nível), e organizar em uma tabela.

#* 3.1.1.A. Variável REASON
#* 
rea_t <- table(base_final$REASON)
rea_tp <- prop.table(rea_t)
cbind(n = rea_t, Fr = rea_tp)

base_final %>% select(REASON) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value))+
  geom_bar()

#* 3.1.1.B. Variável JOB
#* 
job_t <- table(base_final$JOB)
job_tp <- prop.table(job_t)
cbind(n = job_t, Fr = round(job_tp,4))

base_final %>% select(JOB) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value))+
  geom_bar()

#* A categoria "Outros" parece muito concentrada a princípio. Dependendo do seu impacto na variável resposta, 
#* pode ser interessante desmembrar em outros valores e enxergar o impacto de cada categoria na variável resposta.
#* De outro lado, pode ser que sejam tantas que individualmente sejam irrelevantes. De qualquer forma é interessante
#* conversar com o gerente e entender a visão dele sobre esse fato. Se vendas entrou em categoria única, será que não
#* existem outras profissões com esse mesmo nível frequência ou até maior?
 
#* 3.1.1.C. Variáveis Numéricas contínuas 
#*CLAGE, DEBTINC, LOAN, MORTDUE, VALUE, YOJ
#*

skim_without_charts(base_final %>% select(where(is.numeric), -c(BAD, CLNO, DELINQ, DEROG, NINQ)))

base_final %>% select(where(is.numeric), -c(BAD, CLNO, DELINQ, DEROG, NINQ)) %>% # where(is.numeric) retorna somente as variáveis numéricas.
  pivot_longer(everything()) %>%                                                 # -c(BAD, DELINQ, NINQ, YOJ) elimina exatamente as variáveis discretas
  ggplot(aes(x = value))+                                                        # ao final temos somente as variáveis numéricas contínuas.
  geom_histogram(bins = 40, fill = "#2e294e") + 
  facet_wrap(~name, scales = "free")

#* 3.1.1.D. Variáveis Numéricas discretas
#* BAD, DELINQ, CLNO DEROG, NINQ 
#* BAD - Sem problemas com valores missing ou variação na 
bad_t <- table(base_final$BAD)
bad_tp <- prop.table(bad_t)
cbind(n = bad_t, Fr = bad_tp)
#*
#* DELINQ 
del_t <- table(base_final$DELINQ)
del_tp <- prop.table(del_t)
cbind(n = del_t, Fr = del_tp)
#*
#* CLNO
cln_t <- table(base_final$CLNO)
cln_tp <- prop.table(cln_t )
cbind(n = cln_t, Fr = cln_tp)
#*
#* DEROG
der_t <- table(base_final$DEROG)
der_tp <- prop.table(der_t )
cbind(n = der_t, Fr = der_tp)

#* NINQ
nin_t <- table(base_final$NINQ)
nin_tp <- prop.table(nin_t )
cbind(n = nin_t, Fr = nin_tp)

base_final %>% select(BAD, CLNO, DELINQ, DEROG, NINQ) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value))+
  geom_histogram(bins = 40, fill = "#CA28E0") + 
  facet_wrap(~name, scales = "free")
#*
#*
########## 3.2 Análise bivariada
library(PerformanceAnalytics)
install.packages("PerformanceAnalytics")
chart.Correlation(base_final %>% select(where(is.numeric)), histogram = TRUE)

cor(base_final$MORTDUE, base_final$BAD)
cor(base_final$VALUE_T, base_final$BAD)
cor(base_final$CLNO, base_final$BAD)
cor(base_final$MORTDUE, base_final$VALUE_T)

#* As correlações com a variável y são baixa, destacando-se a quantidade de relatórios depreciativos (DEROG) e o número de linhas de crédito inadimplentes (DELINQ).
#* A Correlação entre montante devido nas hipotecas ativas (MORTDUE) e o valor da propriedade atual transformada (VALUE_T) é extremamente alta. Como o risco de 
#* correlação entre estas variáveis é grande (multicolinearidade), e delas para as demais variáveis é muito similar, devemos escolher uma para retirar da base de dados.
#* Apenas por preferir usar variáveis originais, selecionarei retirar VALUE_T da base de dados.

########## 4.Modelagem

########## 4.0 Atualização da base de dados
base_modelo0 <- base_final %>% select(!VALUE_T) # Base retirando a variável VALUE_T.
base_modelo0$BAD <- factor(base_modelo0$BAD, levels = c(0,1))

########## 4.1 Separando base em treino e teste

data_split <- base_modelo0 %>% initial_split(strata = BAD, prop = 3/4)
treino_split <- training(data_split)
teste_split <- testing(data_split)

########## 4.2 Processamento baseado em dados de treinamento

#* 4.2.1. Receita para o grupo 1 com os modelos mlp, svm, logistic_reg
#* 
receita_g1 <- recipe(BAD ~ ., data = treino_split) %>% 
  step_normalize(c(CLAGE,DEBTINC,LOAN,MORTDUE,YOJ)) %>% 
  step_scale(c(CLNO,DELINQ,DEROG,NINQ)) %>% 
  step_other(c(JOB,REASON), threshold = 0.05) %>% 
  step_dummy(c(JOB,REASON), -all_outcomes()) %>% 
  step_zv(all_predictors())

juice(prep(receita_g1))

#* 4.2.2. Receita para o grupo 2 com os modelos rand_forest, boost_tree, decision_tree
#* 
receita_g2 <- recipe(BAD ~ ., data = treino_split) %>%
  step_other(c(JOB,REASON), threshold = 0.05) %>%
  step_dummy(c(JOB,REASON), -all_outcomes()) %>% 
  step_zv(all_predictors())

juice(prep(receita_g2))  

#* 4.3. Criação da estrutura dos modelos
#* 
#* 4.3.1. Grupo 1: modelos mlp, svm, logistic_reg
#* 
#* #* Modelo SVM
#* 
modsvm <- svm_rbf(cost = tune(),
                  rbf_sigma = tune(),
                  margin = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification")

#* Modelo mlp
#* 
modmlp <- mlp(hidden_units = tune(),
              penalty = tune(),
              epochs = tune()) %>% 
  set_engine("nnet", num.threads = 4) %>% 
  set_mode("classification") %>% 
  translate()

#* Modelo regressão logística
#* 
modreglog <- logistic_reg(penalty = tune(),
                          mixture = tune()) %>% 
  set_engine("glm") %>% 
  set_mode("classification")

#* 4.3.2. Grupo 2: modelos rand_forest, boost_tree, decision_tree
#* 
#* Modelo decision_tree
#* 
modtree <- decision_tree(cost_complexity = tune(),
                         min_n = tune()) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

#* Modelo rand_forest
#* 
modforest <- rand_forest(mtry = tune(),
                         min_n = tune(),
                         trees = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

#* Modelo boost_tree
#* 
modxgbst <- boost_tree(tree_depth = tune(),
                       learn_rate = tune(),
                       loss_reduction = tune(),
                       min_n = tune(),
                       sample_size = tune(),
                       trees = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

#* 4.4. Montagem do workflow
#* Primeiro vamos criar um workflow para cada grupo de modelos, depois vamos juntar todos em um só.
#* 
#* 4.4.1. Workflow do Grupo 1: modelos mlp, svm, logistic_reg e nearest_neighbor
#* 
wk_g1 <- workflow_set(preproc = list(grupo_1 = receita_g1),
                      models = list(rede_neural = modmlp,
                                    svm = modsvm,
                                    reg_log = modreglog))

#* 4.4.2. Workflow do Grupo 2: modelos rand_forest, boost_tree, decision_tree
#* 
wk_g2 <- workflow_set(preproc = list(grupo_2 = receita_g2),
                      models = list(rd_forest = modforest,
                                    xgb = modxgbst,
                                    d_tree = modtree))
#* 4.4.3. Workflow global, união dos dois workflows
#* 
wk_global <- bind_rows(wk_g1,wk_g2) %>% 
  mutate(wflow_id = gsub("(grupo_1_)|(grupo_2_)","",wflow_id))

#* 4.5. Selecionando parâmetros usando cross validation
#* 
cv_split <- vfold_cv(treino_split, v = 3, strata = "BAD")
#* 
#* 
#* 4.6. Treinamento do modelo usando o grid
#* 
grid_ctrl <- control_grid(
  save_pred = TRUE,
  parallel_over = "everything",
  save_workflow = TRUE
)
#*
grid_result <- wk_global %>% 
  workflow_map(
    resamples = cv_split,
    grid = 5,
    control = grid_ctrl,
    metrics = metric_set(accuracy,roc_auc))
#* 
#*












