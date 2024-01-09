######################################################################
# This script contains codes for (1) the statistical analyses; 
# (2) plots; and (3) model comparisons in 
# Section 5: Experimental evaluation of Weak and Strong Bayes
######################################################################

# library 
library(readr)
library(dplyr)
library(nnet)
library(lme4)
library(car)
library(emmeans)
library(plotrix)
library(MCMCglmm)
library(dfoptim)
library(optimx)
library(performance)
library(brms)
library(ggplot2)
library(ggeffects)
library(rstudioapi)
library(tidyverse)
library(xtable)
library(gridExtra)
library(bridgesampling)

#### brms settings####
chns <- 6
iters <- 2000

# set the source file direction as working directory
setwd(dirname(getSourceEditorContext()$path))

#### function: add names to different levels of each factor after centering ####
named.contr.sum<-function(x, ...) {
  if (is.factor(x)) {
    x <- levels(x)
  } else if (is.numeric(x) & length(x)==1L) {
    stop("cannot create names with integer value. Pass factor levels")
  }
  x<-contr.sum(x, ...)
  colnames(x) <- apply(x,2,function(x) 
    names(x[x>0])
  )
  x
}

#### read the dataframe of all responses (6000) ####
df_whole <- read_csv('../data/allResponses.csv')  
str(df_whole)
sum(is.nan(as.matrix(df_whole))) # 0


###### check proportion of zero/null subject in responses with bare prompt######
df_bare <- df_whole[df_whole$prompt == 'bare',]

df_bare = df_bare %>%
  mutate(
    subject_id = as.factor(subject_id),
    itemID = as.factor(itemID),
    nullOrNot = ifelse(pronominalization=="zero",1,0), # reference level: non zero
  )
str(df_bare)

# sumContrast
df_bare$relationSum <- factor(df_bare$relation, levels = c("Narration", "Contrast", "Result"))
contrasts(df_bare$relationSum) = contr.sum(3)



null.full <- glmer(nullOrNot ~ relationSum + 
                     (relationSum | subject_id) + 
                     (relationSum | itemID), 
                   data = df_bare, family="binomial",
                   glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

summary(null.full)
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)   -3.2498     0.2138 -15.198   <2e-16 ***
#   relationSum1  -0.2510     0.3018  -0.831    0.406    
#   relationSum2   0.2783     0.2714   1.025    0.305   
#ggpredict(null.full, "relationSum")
#ggpredict(null.full, "relationSum") %>% plot()



#### dataframe for the main analysis ####
## only responses where subjects have re-mentioned main characters (0/1), and they have used an explicit referring expression (except possessive pronouns)
df_main <- df_whole[(df_whole$annotated_nm %in% list("0","1"))  & (df_whole$pronominalization %in% list('pro','non_pro')), ]
# str(df_main)

## center, relevel and set contrasts for predictors
df_main = df_main %>%
  mutate(
    subject_id = as.factor(subject_id),
    itemID = as.factor(itemID),
    subjectOrNotSum = ifelse(annotated_nm=="0",1,-1) , # predictor: centered    
    pronounOrNot = ifelse(pronominalization=="pro",1,0), # outcome- reference level: non-pronoun
    promptSum = ifelse(prompt=="pronoun",1,-1),  # predictor: centered, pronoun 1, bare -1
    reMention = ifelse(annotated_nm=="0",1,0), # outcome - reference level: NP2
  )

df_main$relationSum <- factor(df_main$relation, levels = c("Narration", "Result", "Contrast"))
contrasts(df_main$relationSum) = named.contr.sum(levels(df_main$relationSum))

df_main$subjectOrNotSum <- factor(df_main$subjectOrNotSum)
contrasts(df_main$subjectOrNotSum) <- rbind(-1, 1)

df_main$promptSum <- factor(df_main$promptSum)
contrasts(df_main$promptSum) <- rbind(-1, 1)


##### subset dataframe by prompt type #####
df_main.bare = subset(df_main, promptSum== -1) # prompt type = "bare" 1,773 datapoints
# str(df_main.bare)
df_main.pronoun = subset(df_main, promptSum == 1) # 2,815 datapoints
# str(df_main.pronoun)


#### Analysis 1: next mention biases (frequentist method)####

# prior.full.singular <- glmer(reMention ~ relationSum +
#                       (1 + relationSum | subject_id) +
#                       (1+ relationSum | itemID),
#                     data = df_main.bare, family="binomial",
#                     glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
# 
# isSingular(prior.full.singular) # TRUE
# check_convergence(prior.full.singular, tolerance = 0.00001) #TRUE, gradient: 1.024748e-06
# summary(prior.full.singular)


# --- alternative model with no singular warning ---
prior.full <- glmer(reMention ~ relationSum +
                       (1 | subject_id) +
                       (1 + relationSum |itemID),
                     data = df_main.bare, family="binomial",
                     glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

check_convergence(prior.full, tolerance = 0.00001) #TRUE
# anova(prior.full, prior.full.singular) # p = 0.9983

##### Table 11: Summary of logit mixed effect models of next mention with a fixed effect for the 3-level discourse relation type #####
summary(prior.full)
Anova(prior.full)
# Chisq Df Pr(>Chisq)    
# relationSum 22.475  2  1.317e-05 ***

# --- post hoc comparison ---
emmeans(prior.full, pairwise ~ relationSum)
# $contrasts
# contrast             estimate    SE  df z.ratio p.value
# Narration - Result      0.948 0.254 Inf   3.728  0.0006
# Narration - Contrast    0.932 0.217 Inf   4.293  0.0001
# Result - Contrast      -0.016 0.249 Inf  -0.064  0.9977


####Figure 6: Proportion of subject references by discourse relations and prompt types####
df_nm_barplot <- data.frame (relation = df_main$relation,
                             prompt = df_main$prompt,
                             next_mention = df_main$reMention,
                             subject_id = df_main$subject_id
                              )

by.participant.means <- df_nm_barplot %>%
  group_by(subject_id, relation, prompt) %>% 
  summarize(pt_mean = mean(next_mention))


df_plot_nm <- by.participant.means %>%
  group_by(relation, prompt) %>%
  summarize(mean = mean(pt_mean),
            SE = std.error(pt_mean))  %>%
  mutate(Upper = mean+SE, Lower = mean-SE)

# --- plot ---
ggplot(df_plot_nm, aes(x = factor(relation, 
                          levels = c("Narration", "Result", "Contrast")), 
               y = mean, fill = prompt, label = round(mean, 2))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(x = factor(relation, 
                               levels = c("Narration", "Result", "Contrast")), 
                    ymin = Lower, ymax = Upper), width = 0.4, alpha = 0.9, 
                position = position_dodge(width = .9)) +
  labs(y = "% of subject references", x = "Relation", fill = "Prompt type") +
  geom_text(position = position_dodge(width = .9), vjust = -1, size = 5) +
  theme(axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.background = element_rect(fill = "transparent",colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        plot.margin = margin(t = 10, l= 10),
        ) 


#### Analysis 2: pronoun production biases (Bayesian method) ####
# --- lmer failed to converge ---
# likelihood.full.freq <- glmer(pronounOrNot ~ relationSum*subjectOrNotSum +
#                                  (1 + relationSum*subjectOrNotSum | subject_id) +
#                                  (1 + relationSum*subjectOrNotSum | itemID),
#                                data =df_main.bare, family="binomial",
#                                glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 300000)))
# summary(likelihood.full.freq)   
# check_convergence(likelihood.full.freq, tolerance = 2e-3) # False

# --- use brms instead ---
# define weakly informative prior: Cauchy distribution with center 0 and scale 2.5
myPrior <- c(prior(cauchy(0,2.5), class = b))
likelihood.full.bayes <-  brm(pronounOrNot ~ relationSum * subjectOrNotSum +
                                (1 +  relationSum*subjectOrNotSum | subject_id) +
                                (1 + relationSum*subjectOrNotSum | itemID),
                              data = df_main.bare, family = 'bernoulli',
                              iter = iters,
                              chains = chns,
                              prior = myPrior
                              )
#saveRDS(likelihood.full.bayes, file="saved_models/likelihood.full.bayes.cauchyPrior.rda")
#likelihood.full.bayes = readRDS("saved_models/likelihood.full.bayes.cauchyPrior.rda")

##### model diagnosis #####
# to check divergences, saturated tree depth, E-BFMI
rstan::check_hmc_diagnostics(likelihood.full.bayes$fit)
# to get rhat, check for split R-hat values < 1.1
rhat_vals = rhat(likelihood.full.bayes)
print(rhat_vals)
# Check if any R-hat value is above 1.1
if (any(!is.na(rhat_vals) & rhat_vals > 1.1)) {
  # Print the index of the parameter(s) with an R-hat value above 1.1
  print(which(rhat_vals > 1.1))
}
# to get neff
neff_vals = neff_ratio(likelihood.full.bayes)
print(neff_vals)

mcmc_plot(likelihood.full.bayes, type = "trace")
# investigate model fit
pp_check(likelihood.full.bayes)



##### Table 12: Summary of logit mixed effect models of pronoun production (with all predictors centered) #####
summary(likelihood.full.bayes)

# plot marginal effects
# conditional_effects(likelihood.full.bayes)
# # modify the default plots by brms
# c_eff <- conditional_effects(likelihood.full.bayes)
# ugly_plot <- plot(c_eff, plot = FALSE)[[3]]  +
#   # scale_x_discrete(breaks = c(-1, 1), 
#                     # labels = c("non-subject", "subject")) +
#   labs (x= 'Relation', y='Posterior estimation % of pronouns')  + 
#   scale_colour_discrete(name="Grammatical role", 
#                         labels=c("non-subject", "subject")) +
#  scale_fill_discrete(name="Grammatical role", 
#   labels=c("non-subject", "subject"))  +
#   theme(axis.title.x = element_text(size =20),
#         axis.text.x = element_text(size =20),
#         axis.title.y = element_text(size =20),
#         axis.text.y = element_text(size =20),
#         legend.text = element_text(size =20),
#         legend.title = element_text(size=20),
#         )
# ugly_plot

# # --- post hoc comparison ---
# ems <- emmeans(likelihood.full.bayes, ~relationSum, by = c("subjectOrNotSum"))
# summary(pairs(ems), point.est = mean)

##### Bayes Factors #####
likelihood.full.bayes.alternative <-  brm(pronounOrNot ~ relationSum * subjectOrNotSum +
                                (1 | subject_id) +
                                (1  | itemID),
                              data = df_main.bare, family = 'bernoulli',
                              iter = 4000,
                              chains = chns,
                              prior = myPrior,
                              save_pars = save_pars(all = TRUE),
                              cores = 6,
                              control = list(adapt_delta = 0.9)
)


likelihood.full.bayes.null <-  brm(pronounOrNot ~ subjectOrNotSum +
                                     (1 | subject_id) +
                                     (1 | itemID),
                                   data = df_main.bare, family = 'bernoulli',
                                   iter = 4000,
                                   chains = chns,
                                   prior = myPrior,
                                   save_pars = save_pars(all = TRUE),
                                   cores = 6,
                                   control = list(adapt_delta = 0.9)
)



alternative <- bridgesampling::bridge_sampler(likelihood.full.bayes.alternative, silent = TRUE)
null <- bridgesampling::bridge_sampler(likelihood.full.bayes.null, silent = TRUE)

bf <- bayes_factor(alternative, null)
1/bf[[1]]
bf




#### Figure 7: Pronominalization rates by grammatical roles and relation types ####
df_pro_barplot <- data.frame (relation = df_main.bare$relation,
                             next_mention = ifelse(df_main.bare$reMention==1, 'subject', 'non-subject'),
                             form= df_main.bare$pronounOrNot,
                             subject_id = df_main.bare$subject_id)

by.participant.means <- df_pro_barplot %>%
  group_by(subject_id, relation, next_mention) %>% 
  summarize(pt_mean = mean(form))


df_plot_pro <- by.participant.means %>%
  group_by(relation, next_mention) %>%
  summarize(mean = mean(pt_mean),
            SE = std.error(pt_mean))  %>%
  mutate(Upper = mean+SE, Lower = mean-SE)

# --- plot ---
# Define custom color values
colors <- c("#DAD4C0", "#A4B494", "#73877B")     

ggplot(df_plot_pro, aes(x = next_mention, y = mean, fill = factor(relation, levels = c("Narration", "Result", "Contrast")), label = round(mean, 2))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(x = next_mention, ymin = Lower, ymax = Upper), width = 0.4, alpha = 0.7, position = position_dodge(width = .9)) +
  labs(y = "% of pronouns", x = "Grammatical role of antecedent", fill = "Relation") +
  geom_text(position = position_dodge(width = .9), vjust = -1.3, size = 5.5) +
  scale_fill_manual(values = colors) +
  theme(axis.title = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text = element_text(size = 20),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        plot.margin = margin(t = 10, l = 10)) +
  ylim(0,1)



#### Analysis 3: Pronoun interpretation biases (frequentist method) ####
###     (model of next mention with the fully crossed factors) ###

## maximal model gave singular warning
# post.full.freq <- glmer(reMention ~ relationSum * promptSum +
#                      (1 +  relationSum*promptSum | subject_id) +
#                      (1 + relationSum*promptSum | itemID),
#                    data = df_main, family="binomial",
#                    glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 200000)))

# isSingular(post.full.freq) # TRUE

# final model
post.full.noSingular <- glmer(reMention ~ relationSum * promptSum  +
                          (1 + promptSum  | subject_id) +
                          (1 + promptSum + relationSum | itemID),
                        data = df_main, family="binomial",
                        glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 200000)))

check_convergence(post.full.noSingular, tolerance = 2e-3) # TRUE
isSingular(post.full.noSingular) # FALSE

# --- Anova test ---
car::Anova(post.full.noSingular)
#                           Chisq Df Pr(>Chisq)    
#   relationSum           45.4318  2  1.363e-10 ***
#   promptSum             72.5029  1  < 2.2e-16 ***
#   relationSum:promptSum  7.6824  2    0.02147 *  

# --- post hoc test ---
pairs(emmeans(post.full.noSingular, ~  relationSum))
# contrast             estimate    SE  df z.ratio p.value
# Narration - Result     1.2186 0.210 Inf   5.792  <.0001
# Narration - Contrast   1.2024 0.184 Inf   6.524  <.0001
# Result - Contrast     -0.0162 0.200 Inf  -0.081  0.9964

# relation pair comparison in bare-prompt condition and pronoun-prompt condition separately
m <- emmeans(post.full.noSingular, ~  relationSum|promptSum)
pairs(m)
# promptSum = -1 (bare):
#   contrast             estimate    SE  df z.ratio p.value
# Narration - Result     0.9144 0.230 Inf   3.973  0.0002
# Narration - Contrast   0.8847 0.204 Inf   4.344  <.0001
# Result - Contrast     -0.0298 0.229 Inf  -0.130  0.9907
# 
# promptSum = 1 (pronoun):
#   contrast             estimate    SE  df z.ratio p.value
# Narration - Result     1.5228 0.260 Inf   5.853  <.0001
# Narration - Contrast   1.5201 0.239 Inf   6.372  <.0001
# Result - Contrast     -0.0027 0.226 Inf  -0.012  0.9999


##### Table 13: Summary of logit mixed effect models of next mention with the fully crossed factors of Relation Type*Prompt Type #####
summary(post.full.noSingular)
#xtable(coef(summary(post.full.noSingular)))
#                                 Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                      1.59292    0.23193   6.868 6.51e-12 ***
#   relationSumNarration             0.80700    0.11378   7.093 1.32e-12 ***
#   relationSumResult               -0.41162    0.12224  -3.367 0.000759 ***
#   promptSum1                       0.96596    0.10947   8.824  < 2e-16 ***
#   relationSumNarration:promptSum1  0.20729    0.07502   2.763 0.005726 ** 
#   relationSumResult:promptSum1    -0.09688    0.06729  -1.440 0.149928    




####  Section 5.4 Quantitative model comparisons ####
df_model_pred <- read_csv('../data/model_predictions.csv', col_select = -1)
# subset of predictions for the subject referents
df_model_pred_subj <- df_model_pred[df_model_pred$ref=='subject',]

#--- r-squared ---
r_squared_BAYESIAN_smoothed <- summary(lm(df_model_pred_subj$observedPosterior ~ df_model_pred_subj$BAYESIAN_smoothed , data = df_model_pred_subj))$r.squared
r_squared_EXPECTANCY_smoothed <- summary(lm(df_model_pred_subj$observedPosterior ~ df_model_pred_subj$EXPECTANCY_smoothed , data = df_model_pred_subj))$r.squared
r_squared_MIRROR_smoothed <- summary(lm(df_model_pred_subj$observedPosterior ~ df_model_pred_subj$MIRROR_smoothed , data = df_model_pred_subj))$r.squared

r_squared_BAYESIAN_smoothed 
r_squared_EXPECTANCY_smoothed 
r_squared_MIRROR_smoothed   

# model       r-squared
# Bayesian 	  0.5009164
# Expectancy 	0.4277144
# Mirror 		  0.02918429

#--- mean-squared error ---
MSE_BAYESIAN = mean((df_model_pred_subj$observedPosterior-df_model_pred_subj$BAYESIAN_smoothed)^2)
MSE_EXPECTANCY = mean((df_model_pred_subj$observedPosterior-df_model_pred_subj$EXPECTANCY_smoothed)^2)
MSE_MIRROR = mean((df_model_pred_subj$observedPosterior-df_model_pred_subj$MIRROR_smoothed)^2)

MSE_BAYESIAN
MSE_EXPECTANCY 
MSE_MIRROR 

# model        		MSE
# Bayesian 	      0.03089681
# Expectancy 	    0.09660211
# Mirror 		      0.06312082


#--- Average Cross Entropy (ACE)---
ACE_EXPECTANCY = mean(-(df_model_pred_subj$observedPosterior * log2(df_model_pred_subj$EXPECTANCY_smoothed) + (1 - df_model_pred_subj$observedPosterior) * log2(1 - df_model_pred_subj$EXPECTANCY_smoothed)))
ACE_BAYESIAN = mean(-(df_model_pred_subj$observedPosterior * log2(df_model_pred_subj$BAYESIAN_smoothed) + (1 - df_model_pred_subj$observedPosterior) * log2(1 - df_model_pred_subj$BAYESIAN_smoothed)))
ACE_MIRROR = mean(-(df_model_pred_subj$observedPosterior * log2(df_model_pred_subj$MIRROR_smoothed) + (1 - df_model_pred_subj$observedPosterior) * log2(1 - df_model_pred_subj$MIRROR_smoothed)))

ACE_EXPECTANCY 
ACE_BAYESIAN
ACE_MIRROR 

# model        		ACE
# Bayesian 	      0.5769360132
# Expectancy 	    0.8175563123
# Mirror 		      0.708791092


##### Table 15: Results of statistical metrics for model comparisons #####
metrics_df <- data.frame(
  Metric = c("R squared", "MSE", "ACE"),
  Bayes = round(c(r_squared_BAYESIAN_smoothed, MSE_BAYESIAN, ACE_BAYESIAN), 2),
  Expectancy = round(c(r_squared_EXPECTANCY_smoothed, MSE_EXPECTANCY, ACE_EXPECTANCY), 2),
  Mirror = round(c(r_squared_MIRROR_smoothed, MSE_MIRROR, ACE_MIRROR), 2)
)
metrics_df
# Metric    Bayes Expectancy Mirror
# R squared  0.50       0.43   0.03
#       MSE  0.03       0.10   0.06
#       ACE  0.58       0.82   0.71

##### Figures 8 & 9: scatter plot #####

df_long_subj <- df_model_pred_subj %>%
  pivot_longer(cols = c(BAYESIAN_smoothed, EXPECTANCY_smoothed, MIRROR_smoothed),
               names_to = "Model",
               values_to = "Prediction")


# --- Figure 8 plot --- 
ggplot(df_long_subj, aes(x = Prediction, y = observedPosterior)) +
  geom_point(aes(color = Model, shape = Model), alpha = 1, size =2) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c"), labels = c("Bayes", "Expectancy", "Mirror")) +
  scale_shape_manual(values = c(16, 17, 18), labels = c("Bayes", "Expectancy", "Mirror")) +
  guides(color = guide_legend(override.aes = list(shape = c(16, 17, 18)))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(x = "Model prediction", y = "Human rate of subject interpretation", color = "Model") +
  theme_minimal() +
  theme(
    text = element_text(size = 25, face = "bold"),
    legend.text = element_text(size = 25, face = "bold"),
    legend.position = "top",  # Move the legend to the top
    plot.margin = margin(l=20, r = 20)
  )


#--- Figure 9 ---
# Subset the data for each model
df_model_bayes <- subset(df_long_subj, Model == "BAYESIAN_smoothed")
df_model_expectancy <- subset(df_long_subj, Model == "EXPECTANCY_smoothed")
df_model_mirror <- subset(df_long_subj, Model == "MIRROR_smoothed")

# Create a common x-axis range
x_range <- range(0, 1)

# Plot for Bayes model
plot_bayes <- ggplot(df_model_bayes, aes(x = Prediction, y = observedPosterior)) +
  geom_point(shape = 16, color = "#1f77b4", alpha = 1, size =2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  xlim(x_range) +
  labs(x = "Bayes Model prediction", y = "Human rate of subject interpretation") +
  theme_minimal() +
  theme(
    text = element_text(size = 25, face = "bold")
  )

# Plot for Expectancy model
plot_expectancy <- ggplot(df_model_expectancy, aes(x = Prediction, y = observedPosterior)) +
  geom_point(shape = 17, color = "#ff7f0e", alpha = 1, size =2 ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  xlim(x_range) +
  labs(x = "Expectancy Model prediction", y = "") +
  theme_minimal() +
  theme(
    text = element_text(size = 25, face = "bold")
  )

# Plot for Mirror model
plot_mirror <- ggplot(df_model_mirror, aes(x = Prediction, y = observedPosterior)) +
  geom_point(shape = 18, color = "#2ca02c", alpha = 1,  size =2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  xlim(x_range) +
  labs(x = "Mirror Model prediction", y = "") +
  theme_minimal() +
  theme(
    text = element_text(size = 25, face = "bold")
  )

# plot Figure 9 
grid.arrange(plot_bayes, plot_expectancy, plot_mirror, nrow = 1, ncol = 3)



