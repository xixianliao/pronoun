## This is our adaptation of the Patterson et al. (2022) method, 
## which involved minor modifications to the model parameters to suit our experimental design. 
## Please refer to https://bitbucket.org/bnicenboim/kce/src/master/ for a comprehensive explanation of the method.

## Global options
options(max.print="75",
        width = 80,
        tibble.width = 160,
        digits = 2)
knitr::opts_chunk$set(echo=FALSE,
                      cache=TRUE,
                      prompt=FALSE,
                      tidy=TRUE,
                      comment=NA,
                      message=FALSE,
                      warning=FALSE)
knitr::opts_knit$set(width=80)
knitr::opts_chunk$set(cache.extra = knitr::rand_seed)
set.seed(42) 

# libraries
library(loo)
library(rstudioapi)
library(purrr)
library(dplyr)
library(tidyr)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(ggplot2)
library(matrixStats)
library(loo)
library(readr)
library(knitr)  
# set the source file direction as working directory
setwd(dirname(getSourceEditorContext()$path))

####build dataframes###########
df_whole <- read_csv('../data/allResponses.csv')  
str(df_whole)
sum(is.nan(as.matrix(df_whole))) # 0

####### dataframe for the main analysis ######
## only responses where subjects have re-mentioned main characters (0/1), and they have used an explicit referring expression (except possessive pronouns)
df_main <- df_whole[(df_whole$annotated_nm %in% list("0","1"))  & (df_whole$pronominalization %in% list('pro','non_pro')), ]

## predictors: centered
df_main = df_main %>%
  mutate(
    subject_id = as.factor(subject_id),
    itemID = as.factor(itemID),
    subjectOrNotSum = ifelse(annotated_nm=="0",1,-1) , # predictor: centered    
    pronounOrNot = ifelse(pronominalization=="pro",1,0), # outcome- reference level: non-pronoun
    promptSum = ifelse(prompt=="pronoun",1,-1),  # predictor: centered, pronoun 1, bare -1
    reMention = ifelse(annotated_nm=="0",1,0), # outcome - reference level: NP2
  )


## subset dataframe by prompt type
df_main.free = subset(df_main, prompt== 'bare') # prompt type = "bare" 1,773 datapoints
# str(df_main.free)

df_main.pronoun = subset(df_main, prompt == 'pronoun') # 2,815 datapoints
# str(df_main.pronoun)

## code adapted from Patterson et al.
free <- df_main.free %>% filter(annotated_nm %in% c(0,1)) %>%
  mutate(NP1 = (annotated_nm ==0) %>% as.numeric()) %>%
  mutate(item = paste(itemID, relation)) %>%
  mutate(item = item %>% as.factor() %>% as.numeric())

pron <- df_main.pronoun %>% filter(annotated_nm %in% c(0,1)) %>%
  mutate(NP1 = (annotated_nm ==0) %>% as.numeric()) %>%
  mutate(item = paste(itemID, relation)) %>%
  mutate(item = item %>% as.factor() %>% as.numeric())

# relation type by item:
rtypej1 <- free %>%
  distinct(item, relation) %>%
  arrange(item) %>%
  mutate(relation = recode(relation, 
                           "Narration" = 1,
                           "Contrast" = 0,
                           "Result" = -1)) %>%
  pull(relation)

rtypej2 <- free %>%
  distinct(item, relation) %>%
  arrange(item) %>%
  mutate(relation = recode(relation, 
                           "Narration" = 0,
                           "Contrast" = 1,
                           "Result" = -1)) %>%
  pull(relation)


data <- list(
  #data for training the model, and estimating the parameters
  N_free = nrow(free),
  NP1_free = free$NP1, # 1 = NP1, 0 = NP2
  N_subj = length(unique(free$subject_id)),
  subj_free = as.numeric(as.factor(free$subject_id)),
  N_item = max(free$item),
  item_free = free$item,
  rtype1 = case_when(free$relation=="Narration" ~ 1,
                    free$relation =="Contrast" ~ 0,
                    free$relation =="Result" ~ -1),  
  rtype2 = case_when(free$relation=="Narration" ~ 0,
                     free$relation =="Contrast" ~ 1,
                     free$relation =="Result" ~ -1), 
  #rtype = ifelse(free$relation=="Narration", 1, -1) ,
  ref_free = ifelse(free$NP1==1, 1, -1) ,
  pron_free = ifelse(free$pronominalization=='pro', 1, 0),
  # pron_free = case_when(free$pronominalization=='pro' ~ 1,
  #                       free$pronominalization=='non_pro' ~ 2),
  #data for evaluating the predictions
  N_pron = nrow(pron),
  NP1_pron = pron$NP1,
  item_pron = pron$item,
  rtype1_pron = case_when(pron$relation=="Narration" ~ 1,
                    pron$relation =="Contrast" ~ 0,
                    pron$relation =="Result" ~ -1),  
  rtype2_pron = case_when(pron$relation=="Narration" ~ 0,
                         pron$relation =="Contrast" ~ 1,
                         pron$relation =="Result" ~ -1),  
  #rtype_pron = ifelse(pron$relation=="Narration", 1, -1),
  # pp_pron= pron$prompt == "pronoun",
  ref_pron = ifelse(pron$NP1==1, 1, -1) ,
  subj_pron = as.numeric(as.factor(pron$subject_id)),
  rtypej1 = rtypej1,
  rtypej2 = rtypej2
  )


#### Expectancy Model
m_expectancy <- cmdstan_model("expect_full.stan")
dir.create("output", showWarnings = FALSE)
f_expectancy <- m_expectancy$sample(data=data, parallel_chains = 4, output_dir = "output",adapt_delta = 0.95)

f_expectancy$summary(c("P_ref_NP1","beta_rtype1","beta_rtype2", "P_expect_narration", "P_expect_result", "P_expect_contrast"))

P_ref <- f_expectancy$summary(c("P_ref_NP1")) %>%
  select(mean, q5, q95) %>% unlist()
beta1 <- f_expectancy$summary(c("beta_rtype1")) %>%
  select(mean, q5, q95) %>% unlist()
beta2 <- f_expectancy$summary(c("beta_rtype2")) %>%
  select(mean, q5, q95) %>% unlist()
P_expect <- f_expectancy$summary(c("P_expect_narration", "P_expect_result","P_expect_contrast")) %>%
  select(variable, mean, q5, q95) %>%
  mutate(variable = c("$P(NP1 \\mid relation = Narration)$","$P(NP1 \\mid relation = Result)$", "$P(NP1 \\mid relation = Contrast)$"))


#predictive density
pd_expectancy <- f_expectancy$draws("loglik")
elpd_expectancy <- elpd(pd_expectancy)


#### Mirror model
m_mirror <- cmdstan_model("mirror_full.stan")
f_mirror <- m_mirror$sample(data=data, parallel_chains = 4, output_dir = "output", adapt_delta = 0.95)


P_mirror <- f_mirror$summary(c("P_mirror_narration" ,"P_mirror_result", "P_mirror_contrast")) %>% select(variable, mean, q5, q95) %>%
  mutate(variable =
           c("$P(NP1\\mid relation = Narration)$",
             "$P(NP1\\mid relation = Result)$",
             "$P(NP1\\mid relation = Contrast)$")
            )

beta_rtype <- f_mirror$summary(c("beta_rtype")) %>%
  select(mean, q5, q95) %>% unlist()


#predictive density
pd_mirror <- f_mirror$draws("loglik")
elpd_mirror <- elpd(pd_mirror)


#### Bayesian model
m_bayesian <- cmdstan_model("bayesian_full.stan")
f_bayesian <- m_bayesian$sample(data=data, parallel_chains = 4, output_dir = "output", adapt_delta = 0.95)

f_bayesian$summary(c("P_bayes_pp_narration" ,"P_bayes_pp_result", "P_bayes_pp_contrast"))
f_bayesian$summary(c("beta_rtype" ,"beta_ref", "beta_int"))

P_bayesian <- f_bayesian$summary(c("P_bayes_pp_narration" ,"P_bayes_pp_result", "P_bayes_pp_contrast")) %>% select(variable, mean, q5, q95) %>%
  mutate(variable =
           c("$P(NP1\\mid relation = Narration)$",
             "$P(NP1\\mid relation = Result)$",
             "$P(NP1\\mid relation = Contrast)$"))

beta_rtype <- f_bayesian$summary(c("beta_rtype1", "beta_rtype2")) %>%
  select(mean, q5, q95) %>% unlist()


#predictive density
pd_bayesian <- f_bayesian$draws("loglik")
elpd_bayesian <- elpd(pd_bayesian)



##### Model comparison #####

loo_table <- function(..., model_names){
  elpds <- list(...)
  #elpds <- list(elpd_expectancy, elpd_mirror)
  #model_names <- c("Expectancy","Mirror")
  #binding pointwise predictions into a N x number of models:
  pp <- do.call(cbind, lapply(elpds, function(x) x$pointwise[,1]))
  w <- stacking_weights(pp)
  names(w) <- model_names
  w <- w %>% as.matrix()
  colnames(w) <- c("weigth")
  names(elpds) <- model_names
  loo_res <- loo_compare(elpds)
  cbind(loo_res, w[row.names(loo_res),, drop=FALSE]) %>%  knitr::kable(caption = "The table is ordered by the expected log-predictive density
(elpd) score of the models, with a higher score indicating better pre-
dictive accuracy. The highest scored model is used as a baseline for
the difference in elpd and the difference standard error (SE). The
column weight represents the weights of the individual models that
maximize the total elpd score of all the models.")
}

loo_table(elpd_expectancy, elpd_mirror, elpd_bayesian,model_names= c("Expectancy","Mirror",
                                                                     "Bayesian"))

loo::loo_table(elpd_expectancy, elpd_mirror, elpd_bayesian, 
               model_names= c("Expectancy","Mirror", "Bayesian"))

# "Expectancy" vs."Mirror"
loo_table(elpd_expectancy, elpd_mirror,model_names= c("Expectancy","Mirror"))


pron <- pron %>% mutate(group = case_when(relation=="Narration" ~ "Narration",
                                          relation=="Contrast" ~ "Contrast",
                                          relation=="Result"  ~ "Result"))

four_comp <- map_dfr(unique(pron$group), function(g){
  #  cat(g,"\n")
  elpd_g_expectancy <- elpd_expectancy
  elpd_g_mirror <- elpd_mirror
  elpd_g_bayesian <- elpd_bayesian
  elpd_g_expectancy$pointwise <- elpd_g_expectancy$pointwise[pron$group == g,]
  elpd_g_mirror$pointwise <- elpd_g_mirror$pointwise[pron$group == g,]
  elpd_g_bayesian$pointwise <- elpd_g_bayesian$pointwise[pron$group == g,]
  
  w <- stacking_weights(cbind(Expectancy = elpd_g_expectancy$pointwise[,1], Mirror = elpd_g_mirror$pointwise[,1], Bayesian = elpd_g_bayesian$pointwise[,1]))
  names(w) <- c("Expectancy","Mirror", "Bayesian")
  w <- w %>% as.matrix()
  colnames(w) <- c("weigth")
  
  loo_res <-  loo_compare(list(Expectancy = elpd_g_expectancy, Mirror = elpd_g_mirror, Bayesian = elpd_g_bayesian))
  
  cbind(loo_res, w[row.names(loo_res),, drop=FALSE])   %>%
    as.data.frame() %>%
    cbind(model = rownames(.),.,group = g) %>%
    `rownames<-`(NULL) 
})

kable(four_comp %>% select( -elpd, -se_elpd))




