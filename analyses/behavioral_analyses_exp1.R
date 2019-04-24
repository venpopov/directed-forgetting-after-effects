# -------------------------------------------------------------------
# GOAL: Analyze data from Popov, Marevic, Rummel & Reder (2018), Exp 1.
# AUTHOR: Ven Popov
# DATE UPDATED: 04-APRIL-2019
# -------------------------------------------------------------------


#############################################################################
# SETUP
#############################################################################

rm(list=ls())
library(tidyverse)
library(here)
library(brms)
library(cowplot)
setwd(here())
source('analyses/prior_item_functions.R')
theme_set(theme_classic(base_size=9))

#############################################################################
# DATA
#############################################################################

# load and preprocess data
raw <- read.csv('data/exp1_raw.csv')
names(raw) <- c('subject','item','cue','cued_recall_acc','free_recall_acc')
dat <- raw %>% 
  mutate(cue = ifelse(cue == "**VVV**", 'TBF','TBR')) %>% 
  group_by(subject) %>% 
  mutate(trial = 1:length(subject)) %>% 
  prior_item_analysis('cue', 'cue', 'TBF', 3, 4) %>% 
  select(-cue_prioritem) %>% 
  arrange(subject, desc(trial)) %>% 
  prior_item_analysis('cue', 'postcue', 'TBF', 3, 4) %>% 
  arrange(subject, trial) %>% 
  select(-postcue_prioritem)

# load sac model fit
sac_fit <- read.csv('output/exp1_sac_model_fit.csv')
names(sac_fit)[grepl('type', names(sac_fit))] <- gsub('_type_','',names(sac_fit)[grepl('type', names(sac_fit))])
sac_fit <- select(sac_fit, subject, trial, cued_recall_acc_pred, free_recall_acc_pred)
sac_fit$trial <- sac_fit$trial-48

dat <- left_join(dat, sac_fit)

# save processed data
# write.csv(dat, 'data/exp1_processed.csv', row.names=FALSE)

# -------------------------------------------------------------------
# MODEL FIT STATISTIC
# -------------------------------------------------------------------

fit <- dat %>% 
  filter(!is.na(cue_consec_lab)) %>% 
  group_by(cue_consec_lab, cue) %>% 
  summarise(cued_recall_acc=mean(cued_recall_acc), 
            cued_recall_acc_pred=mean(cued_recall_acc_pred), 
            free_recall_acc=mean(free_recall_acc), 
            free_recall_acc_pred=mean(free_recall_acc_pred))

sqrt(mean((fit$cued_recall_acc-fit$cued_recall_acc_pred)^2))
sqrt(mean((fit$free_recall_acc-fit$free_recall_acc_pred)^2))
cor(fit$cued_recall_acc,fit$cued_recall_acc_pred)^2
cor(fit$free_recall_acc,fit$free_recall_acc_pred)^2

# -------------------------------------------------------------------
# PLOTS WITH SAC FITS
# -------------------------------------------------------------------

# basic cued recall
(f1 <- dat %>% 
   filter(!is.na(cue_prioritem1)) %>% 
   group_by(subject, cue, cue_prioritem1) %>%
   summarise(acc = mean(cued_recall_acc),
             acc_pred = mean(cued_recall_acc_pred)) %>%
   Rmisc::normDataWithin('subject', 'acc') %>% 
   gather(data,acc1,accNormed,acc_pred) %>% 
   mutate(data = recode(data, accNormed = 'Observed', acc_pred = 'SAC model fit')) %>% 
   ggplot(aes(cue, acc1, shape=cue_prioritem1, linetype=data, group=interaction(data,cue_prioritem1))) +
   stat_summary(geom='pointrange') +
   stat_summary(geom='line') +
   stat_summary(data=function(x) filter(x, data=='SAC model fit'), color='white', geom='point', size=1) +
   coord_cartesian(ylim=c(0,0.6)) +
   scale_x_discrete('Instructions for the current item\n') +
   scale_y_continuous('a) Cued recall accuracy\nfor the current item') +
   scale_linetype_discrete('Data vs model') +
   scale_shape_discrete("Instructions for\nthe previous item\n"))

# basic free recall
(f2 <- dat %>% 
    filter(!is.na(cue_prioritem1)) %>% 
    group_by(subject, cue, cue_prioritem1) %>%
    summarise(acc = mean(free_recall_acc),
              acc_pred = mean(free_recall_acc_pred)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    gather(data,acc1,accNormed,acc_pred) %>% 
    mutate(data = recode(data, accNormed = 'Observed', acc_pred = 'SAC model fit')) %>% 
    ggplot(aes(cue, acc1, shape=cue_prioritem1, linetype=data, group=interaction(data,cue_prioritem1))) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    stat_summary(data=function(x) filter(x, data=='SAC model fit'), color='white', geom='point', size=1) +
    coord_cartesian(ylim=c(0,0.6)) +
    scale_x_discrete('Instructions for the current item\n') +
    scale_y_continuous('d) Free recall accuracy\nfor the current item') +
    scale_linetype_discrete('Data vs model') +
    scale_shape_discrete("Instructions for\nthe previous item\n"))

# cumulative effect, cued recall
(f3 <- dat %>% 
    filter(!is.na(cue_prioritem1)) %>% 
    group_by(subject, cue_consec_lab, cue_consec_lab, cue_prioritem1, cue) %>%
    summarise(acc = mean(cued_recall_acc),
              acc_pred = mean(cued_recall_acc_pred)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    gather(data,acc1,accNormed,acc_pred) %>% 
    mutate(data = recode(data, accNormed = 'Observed', acc_pred = 'SAC model fit')) %>% 
    ggplot(aes(cue_consec_lab, acc1, group=interaction(data,cue), color=cue, linetype=data, shape=cue_prioritem1)) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    stat_summary(data=function(x) filter(x, data=='SAC model fit'), color='white', geom='point', size=1) +
    coord_cartesian(ylim=c(0.05,0.6)) +
    scale_x_discrete('# of immediately preceding\n TBF or TBR items') +
    scale_color_discrete('Instructions for the current item\n') +
    scale_linetype_discrete('Data vs model') +
    scale_y_continuous('b) Cued recall accuracy\nfor the current item'))

# cumulative effect, free recall
(f4 <- dat %>% 
    filter(!is.na(cue_prioritem1)) %>% 
    group_by(subject, cue_consec_lab, cue_consec_lab, cue_prioritem1, cue) %>%
    summarise(acc = mean(free_recall_acc),
              acc_pred = mean(free_recall_acc_pred)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    gather(data,acc1,accNormed,acc_pred) %>% 
    mutate(data = recode(data, accNormed = 'Observed', acc_pred = 'SAC model fit')) %>% 
    ggplot(aes(cue_consec_lab, acc1, group=interaction(data,cue), color=cue, linetype=data, shape=cue_prioritem1)) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    stat_summary(data=function(x) filter(x, data=='SAC model fit'), color='white', geom='point', size=1) +
    coord_cartesian(ylim=c(0.05,0.6)) +
    scale_x_discrete('# of immediately preceding\n TBF or TBR items') +
    scale_color_discrete('Instructions for the current item\n') +
    scale_linetype_discrete('Data vs model') +
    scale_y_continuous('e) Free recall accuracy\nfor the current item'))

(f5 <- dat %>% 
    gather(lag, cue_prioritem, cue_prioritem1, cue_prioritem2, cue_prioritem3, cue_prioritem4) %>% 
    filter(!is.na(cue_prioritem)) %>% 
    group_by(subject, lag, cue_prioritem, cue) %>%
    summarise(acc = mean(cued_recall_acc),
              acc_pred = mean(cued_recall_acc_pred)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    gather(data,acc1,accNormed,acc_pred) %>% 
    mutate(data = recode(data, accNormed = 'Observed', acc_pred = 'SAC model fit')) %>% 
    Rmisc::normDataWithin('subject', 'acc') %>% 
    ggplot(aes(lag, acc1, color=cue, shape=cue_prioritem, group=interaction(cue, cue_prioritem, data), linetype=data)) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    stat_summary(data=function(x) filter(x, data=='SAC model fit'), color='white', geom='point', size=1) +
    scale_x_discrete('Lag between current and\nprior study item', labels=1:4) +
    scale_y_continuous('c) Cued recall accuracy\nfor the current item') +
    scale_color_discrete('Instructions for\nthe current item\n')  +
    scale_shape_discrete("Instructions for\nthe previous item\n") +
    scale_linetype_discrete('Data vs model'))

(f6 <- dat %>% 
    gather(lag, cue_prioritem, cue_prioritem1, cue_prioritem2, cue_prioritem3, cue_prioritem4) %>% 
    filter(!is.na(cue_prioritem)) %>% 
    group_by(subject, lag, cue_prioritem, cue) %>%
    summarise(acc = mean(free_recall_acc),
              acc_pred = mean(free_recall_acc_pred)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    gather(data,acc1,accNormed,acc_pred) %>% 
    mutate(data = recode(data, accNormed = 'Observed', acc_pred = 'SAC model fit')) %>% 
    Rmisc::normDataWithin('subject', 'acc') %>% 
    ggplot(aes(lag, acc1, color=cue, shape=cue_prioritem, group=interaction(cue, cue_prioritem, data), linetype=data)) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    stat_summary(data=function(x) filter(x, data=='SAC model fit'), color='white', geom='point', size=1) +
    scale_x_discrete('Lag between current and\nprior study item', labels=1:4) +
    scale_y_continuous('f) Free recall accuracy\nfor the current item') +
    scale_color_discrete('Instructions for\nthe current item\n') +
    scale_shape_discrete("Instructions for\nthe previous item\n") +
    scale_linetype_discrete('Data vs model'))

legend <- g_legend(f5 + theme(legend.position='right'))
no_legend <- theme(legend.position = 'none')
(all_plots <- plot_grid(f1+no_legend, f2+no_legend, NULL, NULL,
                        f3+no_legend, f4+no_legend, NULL, legend,
                        f5+no_legend, f6+no_legend, NULL, NULL,
                        nrow = 3,
                        rel_widths = c(0.375,0.375, 0.05, 0.2)))
ggsave('figures/exp1_results_fit.tiff', all_plots, width=6.75, height=8, units='in', compression='lzw')

#############################################################################
# CUED RECALL ANALYSES
#
# these take a long time, so they can be loaded directly from the pre-saved 
# RData objects that are stored on the OSF repository (https://osf.io/5qd94/files/) 
# under the OSF Storage > analysis_output folder
# Download the final..RData files and store them in the output folder

# NOTE ON THE CHOICE OF RANDOM EFFECTS:

# Since the Bayesian models run by brms take a long time, we initially ran the
# equivalent frequentist hierarchical models using lme4. This is a common
# approach, since the results from the two methods typically converge on the
# same conclusions, and lme4 tends to be many times faster than brms. Within
# lme4, the random effects were determined through restricted likelihood ratio
# tests (Bates et al, 2015). The removal of the correlations among random
# effects improved the model convergence within lme4, which failed to converge
# otherwise. After determining the maximal random effect structure that could be
# supported by the data within lme4 (Bates et al, 2015), we reran the models
# within brms focusing on tests for the fixed effects of interest.

#############################################################################

load('output/exp1_cued_recall_models_final.RData')
load('output/exp1_cued_recall_models_consec_value_final.RData')
load('output/exp1_cued_recall_models_lag_final.RData')

## run bayesian multilevel logistic regression of cued recall as a function of current and preceding cue type

# run bayesian multilevel logistic regression of cued recall as a function of current and preceding cue type
# ml0 <- brm(cued_recall_acc ~ cue + (cue + cue_prioritem1||subject) + (1||item), 
#           data=dat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#           prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# ml1 <- brm(cued_recall_acc ~ cue + cue_prioritem1 + (cue + cue_prioritem1||subject) + (1||item), 
#           data=dat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#           prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# ml2 <- brm(cued_recall_acc ~ cue*cue_prioritem1 + (cue + cue_prioritem1||subject) + (1||item), 
#           data=dat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#           prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))

# bf10 <- bayes_factor(ml1, ml0)
# bf21 <- bayes_factor(ml2, ml1)
# save(ml0, ml1, ml2, bf10, bf21, file='output/exp1_cued_recall_models_final.RData')


## run bayesian multilevel logistic regression of cued recall as a function of number of consecutive cue type

#mla_0 <- brm(cued_recall_acc ~ cue + (cue + cue_consec_value||subject) + (cue + cue_consec_value||item), 
#             data=dat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#             prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
#mla_1 <- brm(cued_recall_acc ~ cue + cue_consec_value + (cue + cue_consec_value||subject) + (cue + cue_consec_value||item), 
#             data=dat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#             prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
#mla_2 <- brm(cued_recall_acc ~ cue*cue_consec_value + (cue + cue_consec_value||subject) + (cue + cue_consec_value||item), 
#             data=dat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#             prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))

#bfa_10 <- bayes_factor(mla_1, mla_0)
#bfa_21 <- bayes_factor(mla_2, mla_1)
#save(mla_0, mla_1, mla_2, bfa_10, bfa_21, file='output/exp1_cued_recall_models_consec_value_final.RData')

# run bayesian multilevel logistic regression of cued recall as a function of preceding cue type and lag
#mlc_1 <- brm(cued_recall_acc ~ cue + cue_prioritem2 + cue_prioritem3 + cue_prioritem4 + (cue + cue_prioritem1||subject) + (1||item), 
#             data=filter(dat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#             cores = 3, chains = 3, prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
#mlc_2 <- brm(cued_recall_acc ~ cue + cue_prioritem1 + cue_prioritem2 + cue_prioritem4 + (cue + cue_prioritem1||subject) + (1||item), 
#             data=filter(dat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#             cores = 3, chains = 3, prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
#mlc_3 <- brm(cued_recall_acc ~ cue + cue_prioritem1 + cue_prioritem3 + cue_prioritem4 + (cue + cue_prioritem1||subject) + (1||item), 
#             data=filter(dat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#             cores = 3, chains = 3, prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
#mlc_4 <- brm(cued_recall_acc ~ cue + cue_prioritem1 + cue_prioritem2 + cue_prioritem3 + (cue + cue_prioritem1||subject) + (1||item), 
#             data=filter(dat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#             cores = 3, chains = 3, prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
#mlc_5 <- brm(cued_recall_acc ~ cue + cue_prioritem1 + cue_prioritem2 + cue_prioritem3 + cue_prioritem4 + (cue + cue_prioritem1||subject) + (1||item), data=filter(dat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), cores = 3, chains = 3, prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))

#bfc_51 <- bayes_factor(mlc_5,mlc_1)
#bfc_52 <- bayes_factor(mlc_5,mlc_2)
#bfc_53 <- bayes_factor(mlc_5,mlc_3)
#bfc_54 <- bayes_factor(mlc_5,mlc_4)

#save(mlc_1, mlc_2, mlc_3, mlc_4, mlc_5, bfc_51, bfc_52, bfc_53, bfc_54, file='output/exp1_cued_recall_models_lag_final.RData')



#hypothesis(mlc_5, 'cue_prioritem1TBR < cue_prioritem2TBR')
#hypothesis(mlc_5, 'cue_prioritem2TBR < cue_prioritem3TBR')
#hypothesis(mlc_5, 'cue_prioritem3TBR < cue_prioritem4TBR')

# save(mlc_1, mlc_2, mlc_3, mlc_4, mlc_5, bfc_51, bfc_52, bfc_53, bfc_54, file='output/exp1_cued_recall_models_lag_final.RData')


#############################################################################
# FREE RECALL ANALYSES
#
# these take a long time, so they can be loaded directly from the pre-saved 
# RData objects that are stored on the OSF repository (https://osf.io/5qd94/files/) 
# under the OSF Storage > analysis_output folder
# Download the .RData files and store them in the output folder
#############################################################################

load('output/exp1_free_recall_models_final.RData')
load('output/exp1_free_recall_models_consec_value_final.RData')
load('output/exp1_free_recall_models_lag_final_final.RData')

# fdat <- dat %>% mutate(free_recall_acc = round(free_recall_acc))  # count as correct only if both words in a pair are recalled
# 
# # run bayesian multilevel logistic regression of free recall as a function of current and preceding cue type
# fml0 <- brm(free_recall_acc ~ cue + (cue + cue_prioritem1||subject) + (1||item), 
#             data=fdat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#             prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# fml1 <- brm(free_recall_acc ~ cue + cue_prioritem1 + (cue + cue_prioritem1||subject) + (1||item), 
#             data=fdat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#             prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# fml2 <- brm(free_recall_acc ~ cue*cue_prioritem1 + (cue + cue_prioritem1||subject) + (1||item), 
#             data=fdat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#             prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# 
# fbf10 <- bayes_factor(fml1, fml0)
# fbf21 <- bayes_factor(fml2, fml1)
# save(fml0, fml1, fml2, fbf10, fbf21, file='output/free_recall_models_final.Rdata')
# 
# 
# # run bayesian multilevel logistic regression of free recall as a function of number of consecutive cue type
# fmla_0 <- brm(free_recall_acc ~ cue + (cue + cue_consec_value||subject) + (cue + cue_consec_value||item), 
#               data=fdat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#               prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# fmla_1 <- brm(free_recall_acc ~ cue + cue_consec_value + (cue + cue_consec_value||subject) + (cue + cue_consec_value||item), 
#               data=fdat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#               prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# fmla_2 <- brm(free_recall_acc ~ cue*cue_consec_value + (cue + cue_consec_value||subject) + (cue + cue_consec_value||item), 
#               data=fdat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#               prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# 
# 
# fbfa_10 <- bayes_factor(fmla_1, fmla_0)
# fbfa_21 <- bayes_factor(fmla_2, fmla_1)
# save(fmla_0, fmla_1, fmla_2, fbfa_10, fbfa_21, file='output/free_recall_models_consec_value_final.Rdata')
# 
# # run bayesian multilevel logistic regression of free recall as a function of preceding cue type and lag
# fmlc_1 <- brm(free_recall_acc ~ cue + cue_prioritem2 + cue_prioritem3 + cue_prioritem4 + (cue + cue_prioritem1||subject) + (1||item), 
#               data=filter(fdat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#               cores = 3, chains = 3, prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# fmlc_2 <- brm(free_recall_acc ~ cue + cue_prioritem1 + cue_prioritem2 + cue_prioritem4 + (cue + cue_prioritem1||subject) + (1||item), 
#               data=filter(fdat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#               cores = 3, chains = 3, prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# fmlc_3 <- brm(free_recall_acc ~ cue + cue_prioritem1 + cue_prioritem3 + cue_prioritem4 + (cue + cue_prioritem1||subject) + (1||item), 
#               data=filter(fdat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#               cores = 3, chains = 3, prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# fmlc_4 <- brm(free_recall_acc ~ cue + cue_prioritem1 + cue_prioritem2 + cue_prioritem3 + (cue + cue_prioritem1||subject) + (1||item), 
#               data=filter(fdat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#               cores = 3, chains = 3, prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# fmlc_5 <- brm(free_recall_acc ~ cue + cue_prioritem1 + cue_prioritem2 + cue_prioritem3 + cue_prioritem4 + (cue + cue_prioritem1||subject) + (1||item), 
#               data=filter(fdat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#               cores = 3, chains = 3, prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# 
# fbfc_51 <- bayes_factor(fmlc_5,fmlc_1)
# fbfc_52 <- bayes_factor(fmlc_5,fmlc_2)
# fbfc_53 <- bayes_factor(fmlc_5,fmlc_3)
# fbfc_54 <- bayes_factor(fmlc_5,fmlc_4)
# save(fmlc_1, fmlc_2, fmlc_3, fmlc_4, fmlc_5, fbfc_51, fbfc_52, fbfc_53, fbfc_54, file='output/free_recall_models_lag_final.Rdata')
# 
# hypothesis(fmlc_5, 'cue_prioritem1TBR < cue_prioritem2TBR')
# hypothesis(fmlc_5, 'cue_prioritem2TBR < cue_prioritem3TBR')
# hypothesis(fmlc_5, 'cue_prioritem3TBR < cue_prioritem4TBR')


#############################################################################
# EFFECT OF SUBSEQUENT ITEM TYPE
#############################################################################

#############################################################################
# PLOTS
#############################################################################

# basic cued recall
(f1 <- dat %>% 
   filter(!is.na(postcue_prioritem1)) %>% 
   group_by(subject, cue, postcue_prioritem1) %>%
   summarise(acc = mean(cued_recall_acc),
             acc_pred = mean(cued_recall_acc_pred)) %>%
   Rmisc::normDataWithin('subject', 'acc') %>% 
   ggplot(aes(cue, accNormed, shape=postcue_prioritem1, group=postcue_prioritem1)) +
   stat_summary(geom='pointrange') +
   stat_summary(geom='line') +
   coord_cartesian(ylim=c(0,0.6)) +
   scale_x_discrete('Instructions for the current item\n') +
   scale_y_continuous('a) Cued recall accuracy\nfor the current item') +
   scale_shape_discrete("Instructions for\nthe subsequent item\n"))

# basic free recall
(f2 <- dat %>% 
    filter(!is.na(postcue_prioritem1)) %>% 
    group_by(subject, cue, postcue_prioritem1) %>%
    summarise(acc = mean(free_recall_acc),
              acc_pred = mean(free_recall_acc_pred)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    ggplot(aes(cue, accNormed, shape=postcue_prioritem1, group=postcue_prioritem1)) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    coord_cartesian(ylim=c(0,0.6)) +
    scale_x_discrete('Instructions for the current item\n') +
    scale_y_continuous('d) Free recall accuracy\nfor the current item') +
    scale_shape_discrete("Instructions for\nthe subsequent item\n"))

# cumulative effect, cued recall
(f3 <- dat %>% 
    filter(!is.na(postcue_prioritem1)) %>% 
    group_by(subject, postcue_consec_lab, postcue_consec_lab, postcue_prioritem1, cue) %>%
    summarise(acc = mean(cued_recall_acc),
              acc_pred = mean(cued_recall_acc_pred)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    ggplot(aes(postcue_consec_lab, accNormed, group=cue, color=cue)) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    coord_cartesian(ylim=c(0.05,0.6)) +
    scale_x_discrete('# of immediately subsequent\n TBF or TBR items') +
    scale_color_discrete('Instructions for the current item\n') +
    scale_linetype_discrete('Data vs model') +
    scale_y_continuous('b) Cued recall accuracy\nfor the current item'))

# cumulative effect, free recall
(f4 <- dat %>% 
    filter(!is.na(postcue_prioritem1)) %>% 
    group_by(subject, postcue_consec_lab, postcue_consec_lab, postcue_prioritem1, cue) %>%
    summarise(acc = mean(free_recall_acc),
              acc_pred = mean(free_recall_acc_pred)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    ggplot(aes(postcue_consec_lab, accNormed, group=cue, color=cue)) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    coord_cartesian(ylim=c(0.05,0.6)) +
    scale_x_discrete('# of immediately subsequent\n TBF or TBR items') +
    scale_color_discrete('Instructions for the current item\n') +
    scale_linetype_discrete('Data vs model') +
    scale_y_continuous('e) Free recall accuracy\nfor the current item'))

(f5 <- dat %>% 
    gather(lag, postcue_prioritem, postcue_prioritem1, postcue_prioritem2, postcue_prioritem3, postcue_prioritem4) %>% 
    filter(!is.na(postcue_prioritem)) %>% 
    group_by(subject, lag, postcue_prioritem, cue) %>%
    summarise(acc = mean(cued_recall_acc),
              acc_pred = mean(cued_recall_acc_pred)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    ggplot(aes(lag, accNormed, color=cue, shape=postcue_prioritem, group=interaction(cue, postcue_prioritem))) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    scale_x_discrete('Lag between current and\nsubsequent study item', labels=1:4) +
    scale_y_continuous('c) Cued recall accuracy\nfor the current item') +
    scale_color_discrete('Instructions for\nthe current item\n')  +
    scale_shape_discrete("Instructions for\nthe subsequent item\n") +
    scale_linetype_discrete('Data vs model'))

(f6 <- dat %>% 
    gather(lag, postcue_prioritem, postcue_prioritem1, postcue_prioritem2, postcue_prioritem3, postcue_prioritem4) %>% 
    filter(!is.na(postcue_prioritem)) %>% 
    group_by(subject, lag, postcue_prioritem, cue) %>%
    summarise(acc = mean(free_recall_acc),
              acc_pred = mean(free_recall_acc_pred)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    ggplot(aes(lag, accNormed, color=cue, shape=postcue_prioritem, group=interaction(cue, postcue_prioritem))) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    scale_x_discrete('Lag between current and\nsubsequent study item', labels=1:4) +
    scale_y_continuous('f) Free recall accuracy\nfor the current item') +
    scale_color_discrete('Instructions for\nthe current item\n') +
    scale_shape_discrete("Instructions for\nthe subsequent item\n"))

legend <- g_legend(f5 + theme(legend.position='right'))
no_legend <- theme(legend.position = 'none')
(all_plots <- plot_grid(f1+no_legend, f2+no_legend, NULL, NULL,
                        f3+no_legend, f4+no_legend, NULL, legend,
                        f5+no_legend, f6+no_legend, NULL, NULL,
                        nrow = 3,
                        rel_widths = c(0.375,0.375, 0.05, 0.2)))
ggsave('figures/exp1_subsequent.tiff', all_plots, width=6.75, height=8, units='in', compression='lzw')


#############################################################################
# SUBSEQUENT ITEM - ANALYSIS
#############################################################################

dat %>% 
  filter(!is.na(postcue_prioritem1)) %>% 
  group_by(subject, postcue_prioritem1) %>% 
  summarise(c_acc_m = mean(cued_recall_acc),
            f_acc_m = mean(free_recall_acc)) %>% 
  group_by(postcue_prioritem1) %>% 
  summarise(c_acc_sd = sd(c_acc_m),
            c_acc_m = mean(c_acc_m),
            f_acc_sd = sd(f_acc_m),
            f_acc_m = mean(f_acc_m))


pml0 <- brm(cued_recall_acc ~ cue + (cue + postcue_prioritem1||subject) + (1||item), 
            data=dat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores=3, chains=3, 
            prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
pml1 <- brm(cued_recall_acc ~ cue + postcue_prioritem1 + (cue + postcue_prioritem1||subject) + (1||item), 
            data=dat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores=3, chains=3, 
            prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))

pbf10 <- bayes_factor(pml1, pml0)
save(pml0, pml1, pbf10, file='output/exp1_postcued_recall_models.RData')


fpml0 <- brm(free_recall_acc ~ cue + (cue + postcue_prioritem1||subject) + (1||item), 
             data=fdat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores=3, chains=3, 
             prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
fpml1 <- brm(free_recall_acc ~ cue + postcue_prioritem1 + (cue + postcue_prioritem1||subject) + (1||item), 
             data=fdat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores=3, chains=3, 
             prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))

fpbf10 <- bayes_factor(fpml1, fpml0)
save(fpml0, fpml1, fpbf10, file='output/exp1_postfree_recall_models.RData')