library(psych)
library(fspe)
library(EFAfactors)
source("models/models.R")



# Helper: approximate log-likelihood / parameter count for PPCA, so that
# AIC/BIC can be computed the same way as for the ML factor models.
.ppca_fit_stats <- function(fit, X, nfactors) {
  n <- nrow(X)
  p <- ncol(X)
  Xhat  <- pcaMethods::fitted(fit)
  resid <- X - Xhat
  sigma2 <- sum(resid^2) / (n * p)
  # loadings (p*q - q(q-1)/2) + 1 noise-variance parameter
  k <- p * nfactors - nfactors * (nfactors - 1) / 2 + 1
  loglik <- -0.5 * n * p * (log(2 * pi * sigma2) + 1)
  list(loglik = loglik, k = k, n = n, sigma2 = sigma2)
}


get_estimate <- function(method, X, max_dim, model) {
  switch(method,
    aic = aic(X, max_dim, model),
    bic = bic(X, max_dim, model),
    rmsea = rmsea(X, max_dim, model),
    parallel_analysis = parallel_analysis(X, max_dim, model),
    hull_method = hull_method(X, max_dim, model),
    kaiser_guttman = kaiser_guttman(X, max_dim, model),
    fspe = fspe(X, max_dim, model),
    ega = ega(X, max_dim, model),
    vss = vss(X, max_dim, model),
    map = map(X, max_dim, model)
  )
}

# ------------------------------------------------------------------
# 1. BIC
# ------------------------------------------------------------------
bic <- function(X, max_dim, model = c("ppca", "fa_orthogonal", "fa_oblique")) {
  model <- match.arg(model)
  scores <- numeric(max_dim)
  
  for (nfactors in 1:max_dim) {
    fit <- fit_model(X, nfactors, model)
    bic_est <- fit$fit$BIC
    if (is.null(bic_est)) {
      scores[nfactors] <- 999999
    } else {
      if (model == "ppca") {
        s <- .ppca_fit_stats(fit, X, nfactors)
        scores[nfactors] <- -2 * s$loglik + s$k * log(s$n)
      } else {
        scores[nfactors] <- bic_est
      }
    }
  }
  
  list(scores = scores, best_dim = which.min(scores))
}

# ------------------------------------------------------------------
# 2. AIC
# ------------------------------------------------------------------
aic <- function(X, max_dim, model = c("ppca", "fa_orthogonal", "fa_oblique")) {
  model <- match.arg(model)
  scores <- numeric(max_dim)
  
  for (nfactors in 1:max_dim) {
    fit <- fit_model(X, nfactors, model)
    aic_est <- fit$fit$AIC
    if (model == "ppca") {
      s <- .ppca_fit_stats(fit, X, nfactors)
      scores[nfactors] <- -2 * s$loglik + 2 * s$k
    } else {
      if (is.null(aic_est)) {
        scores[nfactors] <- 9999999999
      } else {
        scores[nfactors] <- aic_est
      }
    }
  }
  
  list(scores = scores, best_dim = which.min(scores))
}

# ------------------------------------------------------------------
# 3. RMSEA (FA models only)
# ------------------------------------------------------------------
rmsea <- function(X, max_dim, model = c("ppca", "fa_orthogonal", "fa_oblique"),
                  threshold = 0.05) {
  model <- match.arg(model)
  if (model == "ppca") {
    stop("RMSEA is an ML factor-analytic fit index and has no PPCA analogue.")
  }
  
  scores <- numeric(max_dim)
  for (nfactors in 1:max_dim) {
    fit <- fit_model(X, nfactors, model)
    scores[nfactors] <- fit$fit$RMSEA[1]
  }
  
  candidates <- which(scores <= threshold)
  best_dim <- if (length(candidates) > 0) min(candidates) else which.min(scores)
  
  list(scores = scores, best_dim = best_dim)
}

# ------------------------------------------------------------------
# 4. FSPE (Factor/Fit-based Selection via Prediction Error)
# ------------------------------------------------------------------
fspe <- function(X, max_dim, model = c("ppca", "fa_orthogonal", "fa_oblique")) {
  model <- match.arg(model)
  if (!requireNamespace("EGAnet", quietly = TRUE)) {
    stop("Package 'EGAnet' is required for fspe().")
  }
  
  method_map <- c(ppca = "pca", fa_orthogonal = "fa", fa_oblique = "fa")
  rotate_map <- c(ppca = NA, fa_orthogonal = "varimax", fa_oblique = "oblimin")
  
  result <- fspe::fspe(
    data = X,
    maxK = max_dim,
    nfold = 10,
    method = "PE",
    rep = 1,
    pbar = F
  )
  
  list(scores = result$PE, best_dim = result$nfactor)
}

# ------------------------------------------------------------------
# 5. EGA (Exploratory Graph Analysis)
# ------------------------------------------------------------------
ega <- function(X, max_dim, model = c("ppca", "fa_orthogonal", "fa_oblique")) {
  model <- match.arg(model)
  # EGA estimates dimensionality directly from a regularized partial-
  # correlation network + community detection, rather than by scanning
  # candidate factor counts. `model` and `max_dim` are accepted for
  # interface consistency but do not alter the network estimation itself.
  if (!requireNamespace("EGAnet", quietly = TRUE)) {
    stop("Package 'EGAnet' is required for ega().")
  }
  
  fit <- EGAnet::EGA(X, plot.EGA = FALSE)
  n_dim <- max(fit$wc)
  
  list(fit = fit, best_dim = min(n_dim, max_dim))
}

# ------------------------------------------------------------------
# 6. Kaiser-Guttman rule (eigenvalues > 1)
# ------------------------------------------------------------------
kaiser_guttman <- function(X, max_dim, model = c("ppca", "fa_orthogonal", "fa_oblique")) {
  model <- match.arg(model)
  # Computed from the correlation matrix directly; independent of
  # rotation or ppca vs fa. `model` kept for interface consistency only.
  
  R <- cor(X, use = "pairwise.complete.obs")
  eigenvalues <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
  eigenvalues <- eigenvalues[1:max_dim]
  
  list(scores = eigenvalues, best_dim = sum(eigenvalues > 1))
}

# ------------------------------------------------------------------
# 7. Parallel analysis
# ------------------------------------------------------------------
parallel_analysis <- function(X, max_dim, model = c("ppca", "fa_orthogonal", "fa_oblique"),
                              n_iter = 100, quantile = 0.95) {
  model <- match.arg(model)
  fa_type <- if (model == "ppca") "pc" else "fa"
  fm <- if (model %in% c("fa_orthogonal", "fa_oblique")) "ml" else "minres"
  
  pa <- psych::fa.parallel(
    X, fa = fa_type, fm = fm,
    n.iter = n_iter, quant = quantile, plot = FALSE
  )
  
  best_dim <- if (model == "ppca") pa$ncomp else pa$nfact
  list(fit = pa, best_dim = min(best_dim, max_dim))
}

# ------------------------------------------------------------------
# 8. Hull method
# ------------------------------------------------------------------
hull_method <- function(X, max_dim, model = c("ppca", "fa_orthogonal", "fa_oblique")) {
  model <- match.arg(model)
  if (!requireNamespace("EGAnet", quietly = TRUE)) {
    stop("Package 'EGAnet' is required for hull_method().")
  }
  
  method_map <- c(ppca = "PCA", fa_orthogonal = "EFAtools", fa_oblique = "EFAtools")
  rotate_map <- c(ppca = NA, fa_orthogonal = "varimax", fa_oblique = "oblimin")
  
  fit <- EFAfactors::Hull(
    data = X,
    max_dim = max_dim,
    method = method_map[[model]],
    rotation = rotate_map[[model]]
  )
  
  list(fit = fit, best_dim = fit$n_factors)
}

# ------------------------------------------------------------------
# 9. VSS (Very Simple Structure, Revelle & Rocklin, 1979)
# psych::VSS() fits factor/component solutions internally across
# 1:max_dim in a single call, so there's no need for our .fit_model
# loop here - we just pull out the complexity-1 fit statistics.
# ------------------------------------------------------------------
vss <- function(X, max_dim, model = c("ppca", "fa_orthogonal", "fa_oblique")) {
  model <- match.arg(model)
  rotate <- switch(model, ppca = "none", fa_orthogonal = "varimax", fa_oblique = "oblimin")
  fm <- if (model == "ppca") "pc" else "ml"
  
  result <- psych::VSS(X, n = max_dim, rotate = rotate, fm = fm, plot = FALSE)
  scores <- result$cfit.1  # VSS complexity-1 fit at each candidate dimensionality
  
  list(fit = result, scores = scores, best_dim = which.max(scores))
}

# ------------------------------------------------------------------
# 10. MAP (Velicer's Minimum Average Partial, 1976)
# MAP is inherently a PCA-based, partial-correlation criterion (it has
# no rotation and no ML factor-model variant), so the `fa_orthogonal`/
# `fa_oblique` distinction doesn't change anything about it. It's kept
# as an argument for interface consistency; only "ppca" vs "fa_*"
# changes fm internally (pc vs ml) in the underlying VSS() call, and
# even that has no real effect on the MAP values themselves.
# ------------------------------------------------------------------
map <- function(X, max_dim, model = c("ppca", "fa_orthogonal", "fa_oblique")) {
  model <- match.arg(model)
  fm <- if (model == "ppca") "pc" else "ml"
  
  result <- psych::VSS(X, n = max_dim, rotate = "none", fm = fm, plot = FALSE)
  scores <- result$map
  
  list(fit = result, scores = scores, best_dim = which.min(scores))
}
