#
# This file is subject to the terms and conditions defined in
# the file 'LICENSE', which is part of this source code.
#

# Init Python before the R package stack — sklearn import fails on wasm otherwise.
library(reticulate)
invisible(import("sys", convert = FALSE))

library(shiny)
library(bslib)
library(thematic)
library(plotly)
library(ggplot2)
library(reshape2)
library(DiceDesign)
library(sensitivity)
library(fOptions)
library(mco)
library(RColorBrewer)
library(nloptr)
library(MaxPro)
library(parallelPlot)
library(scatterPlotMatrix)
library(shinyBS)
library(NlcOptim)
library(DT)
library(pairsD3)
library(viridis)
library(base64enc)
library(SLHD)
library(MASS)
library(quadprog)
library(abind)
library(glmnet)
library(shinyWidgets)
library(shinyjs)
library(stringi)
library(shinycssloaders)
library(promises)
library(future.callr)
library(energy)
library(arsenal)
library(flexclust)
library(truncnorm)
library(ks)
library(goftest)
library(rvinecopulib)
library(corrplot)
library(dendextend)
library(copula)
library(RANN)
library(VGAM)
library(vbmp)
library(RobustGaSP)
library(sirus)
library(crs)
library(digest)
library(shinyFeedback)
library(zip)

# Minimal scikit-learn smoke test (same path as polynomials.build)
.sk_probe <- function(step, expr) {
  tryCatch(
    {
      out <- force(expr)
      message("sklearn smoke test ok: ", step)
      invisible(out)
    },
    error = function(e) {
      warning("sklearn smoke test failed at ", step, ": ", conditionMessage(e), call. = FALSE)
      invisible(NULL)
    }
  )
}

.sk_cv_loo <- function(poly, np) {
  cv <- poly$cv_results_
  if (inherits(cv, "python.builtin.object")) {
    idx <- as.integer(py_to_r(np$flatnonzero(np$isclose(poly$alphas, poly$alpha_))[0L]))
    loo <- if (as.integer(cv$ndim) == 2L) cv[, idx] else cv[, 0L, idx]
    return(as.numeric(py_to_r(loo)))
  }
  alphas <- as.numeric(poly$alphas)
  alpha <- as.numeric(poly$alpha_)
  idx <- which.min(abs(alphas - alpha))
  d <- dim(cv)
  if (length(d) == 2L) as.numeric(cv[, idx]) else as.numeric(cv[, 1L, idx])
}

sk_lm <- .sk_probe("sklearn.linear_model", import("sklearn.linear_model"))
sk_preproc <- .sk_probe("sklearn.preprocessing", import("sklearn.preprocessing"))
np <- .sk_probe("numpy", import("numpy"))

if (!is.null(np) && !is.null(sk_lm) && !is.null(sk_preproc)) {
  X <- as.matrix(cbind(c(-1, 0, 1, -1, 0), c(0, 1, 0, 1, 0)))
  y <- as.matrix(c(1, 2, 1, 3, 2), ncol = 1)
  scalerY <- .sk_probe("StandardScaler", sk_preproc$StandardScaler())
  if (!is.null(scalerY)) {
    y_scaled <- .sk_probe("fit_transform y", scalerY$fit_transform(y))
    poly_feat <- .sk_probe("PolynomialFeatures", sk_preproc$PolynomialFeatures(degree = 2L))
    if (!is.null(poly_feat) && exists("y_scaled", inherits = FALSE)) {
      X_poly <- .sk_probe("fit_transform X", poly_feat$fit_transform(X))
      scalerX <- .sk_probe("StandardScaler X", sk_preproc$StandardScaler())
      if (!is.null(scalerX) && !is.null(X_poly)) {
        X_poly <- .sk_probe("scale X_poly", scalerX$fit_transform(X_poly))
        poly <- .sk_probe(
          "RidgeCV fit",
          sk_lm$RidgeCV(
            alphas = c(1e-3, 0.01, 0.1, 1),
            scoring = "r2",
            fit_intercept = TRUE,
            store_cv_results = TRUE
          )$fit(X_poly, y_scaled)
        )
        if (!is.null(poly)) {
          loo <- .sk_cv_loo(poly, np)
          .sk_probe("inverse_transform yloo", as.numeric(scalerY$inverse_transform(matrix(loo, ncol = 1))))
        }
      }
    }
  }
}
