# -------------------------------------------------------------------
# GOAL: FIT a Multinomial Process Tree (MPT) model to the data from 
#       Popov, Marevic, Rummel & Reder (2018), Exp 1.
# AUTHOR: Ivan Marevic
# DATE: 26-APRIL-2018
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# SETUP
# -------------------------------------------------------------------
library(TreeBUGS)


# -------------------------------------------------------------------
# FIT MODEL
# -------------------------------------------------------------------

fitMPT <- traitMPT(
  eqnfile = "MPT_exp1.eqn", 
  data = "exp1_FreqData_MPT.csv",   
  restrictions = list("l_PEF=l_PER=l_POF=l_POR"),  
  covData = "exp1_WMDATA_MPT.csv", 
  corProbit = TRUE,
  predStructure = list("a_PEF a_PER a_POF a_POR r_PEF r_PER r_POF r_POR ; WMCcomp"), # WMcomp as predictor for a and r parms
  n.iter = 1000000, n.thin = 300, n.chains = 3,
  parEstFile = "exp1_MPT_param.txt")

summary(fitMPT)

#MCMC Traceplot and density
plot(fitMPT, parameter = "mean")
#Gelman-Rubin plots
plot(fitMPT, parameter = "mean", type = "gelman")

#Plotting individual and group mean param estimates
getParam(fitMPT, parameter = "theta", stat = "summary")
plotParam(fitMPT, includeIndividual = TRUE, estimate = "mean")

#Assessing model fit
plotFit(fitMPT)
PPP(fitMPT, M=1000)

#DF Post and Pre effects for a parameter
a_DFeff_Post <- transformedParameters(fitMPT, list("a_diffPost=a_POR-a_POF"), level = "group", nCPU = 4); summary(a_DFeff_Post)
a_DFeff_Pre <- transformedParameters(fitMPT, list("a_diffPre=a_PEF-a_PER"), level = "group", nCPU = 4); summary(a_DFeff_Pre)
#DF Post and Pre effects for r parameter
r_DFeff_Post <- transformedParameters(fitMPT, list("r_diffPost=r_POR-r_POF"), level = "group", nCPU = 4); summary(r_DFeff_Post)
r_DFeff_Pre <- transformedParameters(fitMPT, list("r_diffPre=r_PEF-r_PER"), level = "group", nCPU = 4); summary(r_DFeff_Pre)
