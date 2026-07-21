library(ggplot2)
library(fspe)
library(dplyr)

source("estimators/estimators.R")
source("models/factor_analysis.R")

# X <- holzinger.swineford |> 
#   select(!(t25_frmbord2:t26_flags)) |> 
#   select(!(school))
X <- holzinger.swineford |> 
  select(t01_visperc:t24_woody)
X

pa_nfact <- parallel_analysis(X, 100, model="fa_oblique")
pa_nfact
fit <- fit_fa_model(X, pa_nfact$best_dim, rotate="oblimin")

print(fit$Lambda)
