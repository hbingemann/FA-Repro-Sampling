source("models/factor_analysis.R")
source("models/ppca.R")

fit_model <- function(X, nfactors, model = c("ppca", "fa_orthogonal", "fa_oblique")) {
  model <- match.arg(model)
  
  switch(model,
         ppca = fit_ppca_model(X, nfactors = nfactors),
         fa_orthogonal = fit_fa_model(X, nfactors = nfactors, rotate = "varimax"),
         fa_oblique    = fit_fa_model(X, nfactors = nfactors, rotate = "oblimin")
  )
}

