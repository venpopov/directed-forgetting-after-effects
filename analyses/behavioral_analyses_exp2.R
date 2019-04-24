# -------------------------------------------------------------------
# GOAL: Analyze data from Popov, Marevic, Rummel & Reder (2018), Exp 2.
# AUTHOR: Ven Popov
# DATE UPDATED: 04-APRIL-2019
# -------------------------------------------------------------------

#############################################################################
# SETUP
#############################################################################

rm(list=ls())
library(tidyverse)
library(here)
library(lme4)
library(brms)
library(cowplot)
setwd(here())
source('analyses/prior_item_functions.R')
theme_set(theme_classic(base_size=9))

#############################################################################
# DATA
#############################################################################

# load and preprocess data
raw <- read.csv('data/exp2_raw.csv')
names(raw) <- c('subject','group','cue','condition','list','stim','free_recall_pair','free_recall_single','free_recall_acc','cued_recall_acc')
dat <- raw %>% 
  mutate(cue = ifelse(cue == "**VVV**", 'TBF','TBR')) %>% 
  group_by(subject, list) %>% 
  mutate(trial = 1:length(subject)) %>% 
  prior_item_analysis('cue', 'cue', 'TBF', 3, 4) %>% 
  select(-cue_prioritem)  %>% 
  arrange(subject, desc(trial)) %>% 
  prior_item_analysis('cue', 'postcue', 'TBF', 3, 4) %>% 
  arrange(subject, trial) %>% 
  select(-postcue_prioritem) %>% 
  mutate(cue_consec_value_binary = ifelse(cue_consec_value >0 , 0.5, -0.5))



dat <- filter(dat, cue != 'TBF')
dat <- dat %>% 
  mutate(free_recall_acc = ifelse(is.na(free_recall_acc), 0, free_recall_acc)) %>% 
  mutate(condition = recode(condition, Control='Control',Rehearsal='Reh',Attention='Att',RehearsalAttention='Reh+Att'),
         condition = as.factor(condition),
         condition = factor(condition, levels=c('Control','Att', 'Reh','Reh+Att'))) %>% 
  separate(stim, c('stim1','stim2'), sep=' - ') %>% 
  mutate(stim1 = tolower(stim1))

# load sac fit
sac_fit <- read.csv('output/marevic2018_results_fit.csv')
names(sac_fit)[grepl('type', names(sac_fit))] <- gsub('_type_','',names(sac_fit)[grepl('type', names(sac_fit))])
sac_fit <- select(sac_fit,subject, list, stim1, cued_recall_acc_pred, free_recall_acc_pred)
sac_fit$list <- gsub('context','', sac_fit$list) %>% toupper()
dat <- left_join(dat, sac_fit)

# set up df for free recall analyses
fdat <- dat %>% mutate(free_recall_acc = round(free_recall_acc))

# save processed data
# write.csv(dat, 'data/exp2_processed.csv', row.names=FALSE)


# -------------------------------------------------------------------
# MODEL FIT STATISTIC
# -------------------------------------------------------------------

fit <- dat %>% 
  filter(!is.na(cue_consec_lab)) %>% 
  group_by(cue_consec_lab) %>% 
  summarise(cued_recall_acc=mean(cued_recall_acc), 
            cued_recall_acc_pred=mean(cued_recall_acc_pred, na.rm=T), 
            free_recall_acc=mean(free_recall_acc), 
            free_recall_acc_pred=mean(free_recall_acc_pred, na.rm=T))

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
   group_by(subject, condition, cue_prioritem1) %>%
   summarise(acc = mean(cued_recall_acc),
             acc_pred = mean(cued_recall_acc_pred)) %>%
   Rmisc::normDataWithin('subject', 'acc') %>% 
   # gather(data,acc1,accNormed,acc_pred) %>% 
   # mutate(data = recode(data, accNormed = 'Observed', acc_pred = 'SAC model fit')) %>% 
   ggplot(aes(condition, accNormed, shape=cue_prioritem1, group=cue_prioritem1)) +
   stat_summary(geom='pointrange') +
   stat_summary(geom='line') +
   coord_cartesian(ylim=c(0.1,0.6)) +
   scale_x_discrete('Dual-task condition\n') +
   scale_y_continuous('a) Cued recall accuracy\nfor the current item') +
   scale_linetype_discrete('Data vs model') +
   scale_shape_discrete("Instructions for\nthe previous item"))

# basic free recall
(f2 <- dat %>% 
    filter(!is.na(cue_prioritem1)) %>% 
    group_by(subject, condition, cue_prioritem1) %>%
    summarise(acc = mean(free_recall_acc),
              acc_pred = mean(free_recall_acc_pred)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    # gather(data,acc1,accNormed,acc_pred) %>% 
    # mutate(data = recode(data, accNormed = 'Observed', acc_pred = 'SAC model fit')) %>% 
    ggplot(aes(condition, accNormed, shape=cue_prioritem1, group=cue_prioritem1)) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    coord_cartesian(ylim=c(0.1,0.6)) +
    scale_x_discrete('Dual-task condition\n') +
    scale_y_continuous('d) Free recall accuracy\nfor the current item') +
    scale_linetype_discrete('Data vs model') +
    scale_shape_discrete("Instructions for\nthe previous item"))

# cumulative effect, cued recall
(f3 <- dat %>% 
    filter(!is.na(cue_prioritem1)) %>% 
    group_by(subject, cue_consec_lab, cue_consec_lab, cue_prioritem1, cue) %>%
    summarise(acc = mean(cued_recall_acc),
              acc_pred = mean(cued_recall_acc_pred)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    gather(data,acc1,accNormed,acc_pred) %>% 
    mutate(data = recode(data, accNormed = 'Observed', acc_pred = 'SAC model fit')) %>% 
    ggplot(aes(cue_consec_lab, acc1, group=data, linetype=data, fill=data, shape=cue_prioritem1)) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    scale_x_discrete('# of immediately preceding\n TBF or TBR items') +
    scale_linetype_discrete('Data vs model') +
    scale_fill_manual('Data vs model', values=c('black','white')) +
    scale_y_continuous('b) Cued recall accuracy\nfor the current item') +
    scale_shape_manual('Instructions for\nthe previous item', values=c(21,24)))

# cumulative effect, free recall
(f4 <- dat %>% 
    filter(!is.na(cue_prioritem1)) %>% 
    group_by(subject, cue_consec_lab, cue_consec_lab, cue_prioritem1, cue) %>%
    summarise(acc = mean(free_recall_acc),
              acc_pred = mean(free_recall_acc_pred)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    gather(data,acc1,accNormed,acc_pred) %>% 
    mutate(data = recode(data, accNormed = 'Observed', acc_pred = 'SAC model fit')) %>% 
    ggplot(aes(cue_consec_lab, acc1, group=data, linetype=data, fill=data, shape=cue_prioritem1)) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    scale_x_discrete('# of immediately preceding\n TBF or TBR items') +
    scale_linetype_discrete('Data vs model') +
    scale_fill_manual('Data vs model', values=c('black','white')) +
    scale_y_continuous('e) Free recall accuracy\nfor the current item') +
    scale_shape_manual('Instructions for\nthe previous item', values=c(21,24)))

(f5 <- dat %>% 
    gather(lag, cue_prioritem, cue_prioritem1, cue_prioritem2, cue_prioritem3, cue_prioritem4) %>% 
    filter(!is.na(cue_prioritem)) %>% 
    group_by(subject, lag, cue_prioritem, cue) %>%
    summarise(acc = mean(cued_recall_acc, na.rm=T),
              acc_pred = mean(cued_recall_acc_pred, na.rm=T)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    gather(data,acc1,accNormed,acc_pred) %>% 
    mutate(data = recode(data, accNormed = 'Observed', acc_pred = 'SAC model fit')) %>% 
    Rmisc::normDataWithin('subject', 'acc') %>% 
    ggplot(aes(lag, acc1, shape=cue_prioritem, group=interaction(cue_prioritem, data), linetype=data, fill=data)) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    scale_x_discrete('Lag between current and\nprior study item', labels=1:4) +
    scale_y_continuous('c) Cued recall accuracy\nfor the current item') +
    scale_shape_manual('Instructions for\nthe previous item', values=c(21,24))  +
    scale_linetype_discrete('Data vs model') +
    scale_fill_manual('Data vs model', values=c('black','white')))

(f6 <- dat %>% 
    gather(lag, cue_prioritem, cue_prioritem1, cue_prioritem2, cue_prioritem3, cue_prioritem4) %>% 
    filter(!is.na(cue_prioritem)) %>% 
    group_by(subject, lag, cue_prioritem, cue) %>%
    summarise(acc = mean(free_recall_acc, na.rm=T),
              acc_pred = mean(free_recall_acc_pred, na.rm=T)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    gather(data,acc1,accNormed,acc_pred) %>% 
    mutate(data = recode(data, accNormed = 'Observed', acc_pred = 'SAC model fit')) %>% 
    Rmisc::normDataWithin('subject', 'acc') %>% 
    ggplot(aes(lag, acc1, shape=cue_prioritem, group=interaction(cue_prioritem, data), linetype=data, fill=data)) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    scale_x_discrete('Lag between current and\nprior study item', labels=1:4) +
    scale_y_continuous('f) Free recall accuracy\nfor the current item') +
    scale_shape_manual('Instructions for\nthe previous item', values=c(21,24))  +
    scale_linetype_discrete('Data vs model') +
    scale_fill_manual('Data vs model', values=c('black','white')))

legend <- g_legend(f5 + theme(legend.position='right'))
no_legend <- theme(legend.position = 'none')
(all_plots <- plot_grid(f1+no_legend, f2+no_legend, NULL, NULL,
                        f3+no_legend, f4+no_legend, NULL, legend,
                        f5+no_legend, f6+no_legend, NULL, NULL,
                        nrow = 3,
                        rel_widths = c(0.375,0.375, 0.05, 0.2)))

ggsave('figures/exp2_results_fit.tiff', all_plots, width=6.75, height=8, units='in', compression='lzw')


#############################################################################
# CUED RECALL ANALYSES
#
# these take a long time, so they can be loaded directly from the pre-saved 
# RData objects that are stored on the OSF repository (https://osf.io/5qd94/files/)
# under the OSF Storage > analysis_output folder
# Download the .RData files and store them in the output folder
#############################################################################

load('output/exp2_cued_recall_models_final.RData')
load('output/exp2_cued_recall_models_consec_value_final.RData')
load('output/exp2_cued_recall_models_lag_final.RData')

# # run bayesian multilevel logistic regression of cued recall as a function of current and preceding cue type
# ml0 <- brm(cued_recall_acc ~ 1 + (condition + cue_prioritem1||subject) + (1||stim1), 
#            data=dat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3)
# ml1 <- brm(cued_recall_acc ~ cue_prioritem1 + (condition + cue_prioritem1||subject) + (1||stim1), 
#            data=dat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#            prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# ml2 <- brm(cued_recall_acc ~ cue_prioritem1 + condition + (condition + cue_prioritem1||subject) + (1||stim1), 
#            data=dat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#            prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# ml3 <- brm(cued_recall_acc ~ cue_prioritem1 * condition + (condition + cue_prioritem1||subject) + (1||stim1), 
#            data=dat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#            prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# 
# 
# bf10 <- bayes_factor(ml1, ml0)
# bf21 <- bayes_factor(ml2, ml1)
# bf32 <- bayes_factor(ml3, ml2)
# save(ml0, ml1, ml2, ml3, bf10, bf21, bf32, file='output/exp2_cued_recall_models_final.RData')
# 
# 
# # run bayesian multilevel logistic regression of cued recall as a function of number of consecutive cue type
# mla_0 <- brm(cued_recall_acc ~ condition + (condition + cue_consec_value||subject) + (1||stim1), 
#              data=dat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#              prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# mla_1 <- brm(cued_recall_acc ~ condition + cue_consec_value + (condition + cue_consec_value||subject) + (1||stim1), 
#              data=dat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#              prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# 
# save(mla_0, mla_1, bfa_10, file='output/exp2_cued_recall_models_consec_value_final.RData')
# 
# # run bayesian multilevel logistic regression of cued recall as a function of preceding cue type and lag
# mlc_1 <- brm(cued_recall_acc ~ condition + cue_prioritem2 + cue_prioritem3 + cue_prioritem4 + (condition + cue_prioritem1||subject) + (1||stim1), 
#              data=filter(dat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#              cores = 3, chains = 3, prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# mlc_2 <- brm(cued_recall_acc ~ condition + cue_prioritem1 + cue_prioritem2 + cue_prioritem4 + (condition + cue_prioritem1||subject) + (1||stim1), 
#              data=filter(dat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#              cores = 3, chains = 3, prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# mlc_3 <- brm(cued_recall_acc ~ condition + cue_prioritem1 + cue_prioritem3 + cue_prioritem4 + (condition + cue_prioritem1||subject) + (1||stim1), 
#              data=filter(dat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#              cores = 3, chains = 3, prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# mlc_4 <- brm(cued_recall_acc ~ condition + cue_prioritem1 + cue_prioritem2 + cue_prioritem3 + (condition + cue_prioritem1||subject) + (1||stim1), 
#              data=filter(dat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#              cores = 3, chains = 3, prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# mlc_5 <- brm(cued_recall_acc ~ condition + cue_prioritem1 + cue_prioritem2 + cue_prioritem3 + cue_prioritem4 + (condition + cue_prioritem1||subject) + (1||stim1), 
#              data=filter(dat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#              cores = 3, chains = 3, prior  = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# 
# bfc_51 <- bayes_factor(mlc_5,mlc_1)
# bfc_52 <- bayes_factor(mlc_5,mlc_2)
# bfc_53 <- bayes_factor(mlc_5,mlc_3)
# bfc_54 <- bayes_factor(mlc_5,mlc_4)
# 
# hypothesis(mlc_5, 'cue_prioritem1TBR < cue_prioritem2TBR')
# hypothesis(mlc_5, 'cue_prioritem2TBR < cue_prioritem3TBR')
# hypothesis(mlc_5, 'cue_prioritem3TBR < cue_prioritem4TBR')
# 
# save(mlc_1, mlc_2, mlc_3, mlc_4, mlc_5, bfc_51, bfc_52, bfc_53, bfc_54, file='output/exp2_cued_recall_models_lag_final.RData')

#############################################################################
# FREE RECALL ANALYSES
#
# these take a long time, so they can be loaded directly from the pre-saved 
# RData objects that are stored on the OSF repository (https://osf.io/5qd94/files/)
# under the OSF Storage > analysis_output folder
# Download the .RData files and store them in the output folder
#############################################################################

load('output/exp2_free_recall_models_final.RData')
load('output/exp2_free_recall_models_consec_value_final.RData')
load('output/exp2_free_recall_models_lag_final.RData')

# # run bayesian multilevel logistic regression of free recall as a function of current and preceding cue type
# fml0 <- brm(free_recall_acc ~ 1 + (condition + cue_prioritem1||subject) + (1||stim1), 
#             data=fdat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3)
# fml1 <- brm(free_recall_acc ~ condition + (condition + cue_prioritem1||subject) + (1||stim1), 
#             data=fdat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#             prior = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# fml2 <- brm(free_recall_acc ~ cue_prioritem1 + condition + (condition + cue_prioritem1||subject) + (1||stim1), 
#             data=fdat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#             prior = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# fml3 <- brm(free_recall_acc ~ cue_prioritem1 * condition + (condition + cue_prioritem1||subject) + (1||stim1), 
#             data=fdat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#             prior = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# 
# 
# fbf10 <- bayes_factor(fml1, fml0)
# fbf21 <- bayes_factor(fml2, fml1)
# fbf32 <- bayes_factor(fml3, fml2)
# save(fml0, fml1, fml2, fml3, fbf10, fbf21, fbf32, file='output/exp2_free_recall_models_final.RData')
# 
# 
# # run bayesian multilevel logistic regression of free recall as a function of number of consecutive cue type
# fmla_0 <- brm(free_recall_acc ~ condition  + (condition + cue_consec_value||subject) + (1||stim1), 
#               data=fdat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#               prior = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# fmla_1 <- brm(free_recall_acc ~ condition + cue_consec_value + (condition + cue_consec_value||subject) + (1||stim1), 
#               data=fdat, family=bernoulli(), save_all_pars = TRUE, iter = 10000, cores = 3, chains = 3, 
#               prior = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# 
# fbfa_10 <- bayes_factor(fmla_1, fmla_0)
# save(fmla_0, fmla_1, fbfa_10, file='output/exp2_free_recall_models_consec_value_final.RData')
# 
# # run bayesian multilevel logistic regression of free recall as a function of preceding cue type and lag
# fmlc_1 <- brm(free_recall_acc ~ condition + cue_prioritem2 + cue_prioritem3 + cue_prioritem4 + (condition + cue_prioritem1||subject) + (1||stim1), 
#               data=filter(fdat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#               cores = 3, chains = 3, prior = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# fmlc_2 <- brm(free_recall_acc ~ condition + cue_prioritem1 + cue_prioritem2 + cue_prioritem4 + (condition + cue_prioritem1||subject) + (1||stim1), 
#               data=filter(fdat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#               cores = 3, chains = 3, prior = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# fmlc_3 <- brm(free_recall_acc ~ condition + cue_prioritem1 + cue_prioritem3 + cue_prioritem4 + (condition + cue_prioritem1||subject) + (1||stim1), 
#               data=filter(fdat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#               cores = 3, chains = 3, prior = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# fmlc_4 <- brm(free_recall_acc ~ condition + cue_prioritem1 + cue_prioritem2 + cue_prioritem3 + (condition + cue_prioritem1||subject) + (1||stim1), 
#               data=filter(fdat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#               cores = 3, chains = 3, prior = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# fmlc_5 <- brm(free_recall_acc ~ condition + cue_prioritem1 + cue_prioritem2 + cue_prioritem3 + cue_prioritem4 + (condition + cue_prioritem1||subject) + (1||stim1), 
#               data=filter(fdat, !is.na(cue_prioritem4)), family=bernoulli(), save_all_pars = TRUE, iter = 10000, control = list(adapt_delta = 0.98), 
#               cores = 3, chains = 3, prior = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# 
# fbfc_51 <- bayes_factor(fmlc_5,fmlc_1)
# fbfc_52 <- bayes_factor(fmlc_5,fmlc_2)
# fbfc_53 <- bayes_factor(fmlc_5,fmlc_3)
# fbfc_54 <- bayes_factor(fmlc_5,fmlc_4)
# 
# hypothesis(fmlc_5, 'conditionAtt < 0')
# hypothesis(fmlc_5, 'cue_prioritem1TBR < cue_prioritem2TBR')
# hypothesis(fmlc_5, 'cue_prioritem2TBR < cue_prioritem3TBR')
# hypothesis(fmlc_5, 'cue_prioritem3TBR < cue_prioritem4TBR')
# 
# save(fmlc_1, fmlc_2, fmlc_3, fmlc_4, fmlc_5, fbfc_51, fbfc_52, fbfc_53, fbfc_54, file='output/exp2_free_recall_models_lag_final.RData')


#############################################################################
# EFFECT OF SUBSEQUENT ITEM TYPE
#############################################################################

#############################################################################
# PLOTS
#############################################################################

# basic cued recall
(f1 <- dat %>% 
   filter(!is.na(postcue_prioritem1)) %>% 
   group_by(subject, condition, postcue_prioritem1) %>%
   summarise(acc = mean(cued_recall_acc),
             acc_pred = mean(cued_recall_acc_pred)) %>%
   Rmisc::normDataWithin('subject', 'acc') %>% 
   ggplot(aes(condition, accNormed, shape=postcue_prioritem1, group=postcue_prioritem1)) +
   stat_summary(geom='pointrange') +
   stat_summary(geom='line') +
   coord_cartesian(ylim=c(0.1,0.6)) +
   scale_x_discrete('Instructions for the current item\n') +
   scale_y_continuous('a) Cued recall accuracy\nfor the current item') +
   scale_shape_discrete("Instructions for\nthe subsequent item\n"))

# basic free recall
(f2 <- dat %>% 
    filter(!is.na(postcue_prioritem1)) %>% 
    group_by(subject, condition, postcue_prioritem1) %>%
    summarise(acc = mean(free_recall_acc),
              acc_pred = mean(free_recall_acc_pred)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    ggplot(aes(condition, accNormed, shape=postcue_prioritem1, group=postcue_prioritem1)) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    coord_cartesian(ylim=c(0.1,0.6)) +
    scale_x_discrete('Instructions for the current item\n') +
    scale_y_continuous('d) Free recall accuracy\nfor the current item') +
    scale_shape_discrete("Instructions for\nthe subsequent item\n"))

# cumulative effect, cued recall
(f3 <- dat %>% 
    filter(!is.na(postcue_prioritem1)) %>% 
    group_by(subject, postcue_consec_lab, postcue_consec_lab, postcue_prioritem1, condition) %>%
    summarise(acc = mean(cued_recall_acc),
              acc_pred = mean(cued_recall_acc_pred)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    ggplot(aes(postcue_consec_lab, accNormed, group=1)) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    coord_cartesian(ylim=c(0.24,0.45)) +
    scale_x_discrete('# of immediately subsequent\n TBF or TBR items') +
    scale_y_continuous('b) Cued recall accuracy\nfor the current item'))

# cumulative effect, free recall
(f4 <- dat %>% 
    filter(!is.na(postcue_prioritem1)) %>% 
    group_by(subject, postcue_consec_lab, postcue_consec_lab, postcue_prioritem1, condition) %>%
    summarise(acc = mean(free_recall_acc),
              acc_pred = mean(free_recall_acc_pred)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    ggplot(aes(postcue_consec_lab, accNormed, group=1)) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    coord_cartesian(ylim=c(0.15,0.28)) +
    scale_x_discrete('# of immediately subsequent\n TBF or TBR items') +
    scale_y_continuous('e) Free recall accuracy\nfor the current item'))

(f5 <- dat %>% 
    gather(lag, postcue_prioritem, postcue_prioritem1, postcue_prioritem2, postcue_prioritem3, postcue_prioritem4) %>% 
    filter(!is.na(postcue_prioritem)) %>% 
    group_by(subject, lag, postcue_prioritem, condition) %>%
    summarise(acc = mean(cued_recall_acc),
              acc_pred = mean(cued_recall_acc_pred)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    ggplot(aes(lag, accNormed, shape=postcue_prioritem, group=postcue_prioritem)) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    coord_cartesian(ylim=c(0.35,0.43)) +
    scale_x_discrete('Lag between current and\nsubsequent study item', labels=1:4) +
    scale_y_continuous('c) Cued recall accuracy\nfor the current item') +
    scale_shape_discrete("Instructions for\nthe subsequent item\n"))

(f6 <- dat %>% 
    gather(lag, postcue_prioritem, postcue_prioritem1, postcue_prioritem2, postcue_prioritem3, postcue_prioritem4) %>% 
    filter(!is.na(postcue_prioritem)) %>% 
    group_by(subject, lag, postcue_prioritem, condition) %>%
    summarise(acc = mean(free_recall_acc),
              acc_pred = mean(free_recall_acc_pred)) %>%
    Rmisc::normDataWithin('subject', 'acc') %>% 
    ggplot(aes(lag, accNormed, shape=postcue_prioritem, group=postcue_prioritem)) +
    stat_summary(geom='pointrange') +
    stat_summary(geom='line') +
    coord_cartesian(ylim=c(0.19,0.275)) +
    scale_x_discrete('Lag between current and\nsubsequent study item', labels=1:4) +
    scale_y_continuous('f) Free recall accuracy\nfor the current item') +
    scale_shape_discrete("Instructions for\nthe subsequent item\n"))

legend <- g_legend(f5 + theme(legend.position='right'))
no_legend <- theme(legend.position = 'none')
(all_plots <- plot_grid(f1+no_legend, f2+no_legend, NULL, NULL,
                        f3+no_legend, f4+no_legend, NULL, legend,
                        f5+no_legend, f6+no_legend, NULL, NULL,
                        nrow = 3,
                        rel_widths = c(0.375,0.375, 0.05, 0.2)))
ggsave('figures/marevic2018_exp2_subsequent.tiff', all_plots, width=6.75, height=8, units='in', compression='lzw')


#############################################################################
# SUBSEQUENT ITEM - ANALYSIS
#############################################################################
# 
# 
# dat %>% 
#   filter(!is.na(postcue_prioritem1)) %>% 
#   group_by(subject, postcue_prioritem1) %>% 
#   summarise(c_acc_m = mean(cued_recall_acc),
#             f_acc_m = mean(free_recall_acc)) %>% 
#   group_by(postcue_prioritem1) %>% 
#   summarise(c_acc_sd = sd(c_acc_m),
#             c_acc_m = mean(c_acc_m),
#             f_acc_sd = sd(f_acc_m),
#             f_acc_m = mean(f_acc_m))
# 
# dat %>% 
#   group_by(postcue_consec_value) %>% 
#   summarise(cued = mean(cued_recall_acc) %>% round(3), 
#             free = mean(free_recall_acc) %>% round(3))
# 
# dat %>% 
#   group_by(cue_prioritem1) %>% 
#   summarise(cued = mean(cued_recall_acc) %>% round(3), 
#             free = mean(free_recall_acc) %>% round(3))
# 
# dat %>% 
#   group_by(cue_consec_value) %>% 
#   summarise(cued = mean(cued_recall_acc) %>% round(3), 
#             free = mean(free_recall_acc) %>% round(3))
# 
# post_ml1 <- brm(cued_recall_acc ~ condition + (condition + postcue_prioritem1||subject) + (1||stim1), 
#                 data=dat, family=bernoulli(), save_all_pars = TRUE, iter = 8000, cores=3, chains=3, 
#                 prior = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# post_ml2 <- brm(cued_recall_acc ~ condition + postcue_prioritem1 + (condition + postcue_prioritem1||subject) + (1||stim1), 
#                 data=dat, family=bernoulli(), save_all_pars = TRUE, iter = 8000, cores=3, chains=3, 
#                 prior = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# post_ml3 <- brm(cued_recall_acc ~ condition*postcue_prioritem1 + (condition + postcue_prioritem1||subject) + (1||stim1), 
#                 data=dat, family=bernoulli(), save_all_pars = TRUE, iter = 8000, cores=3, chains=3, 
#                 prior = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# post_bf12 <- bayes_factor(post_ml1, post_ml2)
# post_bf23 <- bayes_factor(post_ml2, post_ml3)
# save(post_ml1, post_ml2, post_ml3, post_bf12, post_bf23, file='output/exp2_postcued_recall_models.RData')
# 
# 
# 
# post_fml1 <- brm(free_recall_acc ~ condition + (condition + postcue_prioritem1||subject) + (1||stim1), 
#                  data=fdat, family=bernoulli(), save_all_pars = TRUE, iter = 8000, cores=3, chains=3, 
#                  prior = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# post_fml2 <- brm(free_recall_acc ~ condition + postcue_prioritem1 + (condition + postcue_prioritem1||subject) + (1||stim1), 
#                  data=fdat, family=bernoulli(), save_all_pars = TRUE, iter = 8000, cores=3, chains=3, 
#                  prior = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# post_fml3 <- brm(free_recall_acc ~ condition*postcue_prioritem1 + (condition + postcue_prioritem1||subject) + (1||stim1), 
#                  data=fdat, family=bernoulli(), save_all_pars = TRUE, iter = 8000, cores=3, chains=3, 
#                  prior = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# post_fml4 <- brm(free_recall_acc ~ condition + postcue_prioritem1 + cue_prioritem1 + (condition + cue_prioritem1 + postcue_prioritem1||subject) + (1||stim1), 
#                  data=fdat, family=bernoulli(), save_all_pars = TRUE, iter = 2000, cores=3, chains=3, 
#                  prior = set_prior('student_t(3, 0, 2.5)', class = 'b'))
# 
# hypothesis(post_fml4, ' cue_prioritem1TBR < postcue_prioritem1TBR')
# 
# post_fbf12 <- bayes_factor(post_fml1, post_fml2)
# post_fbf23 <- bayes_factor(post_fml2, post_fml3)
# save(post_fml1, post_fml2, post_fml3, post_fbf12, post_fbf23, file='output/exp2_postfree_recall_models.RData')