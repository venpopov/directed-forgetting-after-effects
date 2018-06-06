# -------------------------------------------------------------------
# GOAL: FIT a Multinomial Process Tree (MPT) model to the data from 
#       Popov, Marevic, Rummel & Reder (2018), Exp 2.
# AUTHOR: Ivan Marevic
# DATE: 26-APRIL-2018
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# SETUP
# -------------------------------------------------------------------

#load library
library(TreeBUGS)
library(here)
setwd(here())

# -------------------------------------------------------------------
# FIT MODEL
# -------------------------------------------------------------------

fitMPTAgg <- traitMPT(
  eqnfile = "analyses/MPT_exp2.eqn", 
  data = "data/exp2_FreqData_MPT_aggConditions.csv",   
  restrictions = list("l_F=l_R"),
  n.iter = 1000000, n.burnin = 20000, n.thin = 500, n.chains = 3,
  parEstFile = "output/exp2_MPT_param_aggCond.txt")

summary(fitMPTAgg)

#MCMC Traceplot and density
plot(fitMPTAgg, parameter = "mean")
#Gelman-Rubin plots
plot(fitMPTAgg, parameter = "mean", type = "gelman")

#Plotting individual and group mean param estimates
getParam(fitMPTAgg, parameter = "theta", stat = "summary")
plotParam(fitMPTAgg, includeIndividual = TRUE, estimate = "mean")

#Assessing model fit
plotFit(fitMPTAgg)
PPP(fitMPTAgg, M=1000)


#DF pre-effects (a parameter)
a_DFeff <- transformedParameters(fitMPTAgg, list("a_diff=a_F-a_R"), level = "group", nCPU = 4); summary(a_DFeff)

#DF pre-effects for each group (r parameter)
r_DFeff <- transformedParameters(fitMPTAgg, list("r_diff=r_F-r_R"), level = "group", nCPU = 4); summary(r_DFeff)

