# Compute a weighted Pearson correlation after dropping incomplete observations.
weighted_cor <- function(x, y, w) {
  complete <- complete.cases(x, y, w) & w > 0
  x <- x[complete]
  y <- y[complete]
  w <- w[complete]

  # Return missing when the data cannot identify a correlation.
  if (length(x) < 2 || sum(w) == 0 || stats::weighted.mean((x - stats::weighted.mean(x, w))^2, w) == 0 ||
      stats::weighted.mean((y - stats::weighted.mean(y, w))^2, w) == 0) {
    return(NA_real_)
  }

  # Use weighted covariance divided by weighted standard deviations.
  x_mean <- stats::weighted.mean(x, w)
  y_mean <- stats::weighted.mean(y, w)
  cov_xy <- stats::weighted.mean((x - x_mean) * (y - y_mean), w)
  sd_x <- sqrt(stats::weighted.mean((x - x_mean)^2, w))
  sd_y <- sqrt(stats::weighted.mean((y - y_mean)^2, w))
  cov_xy / (sd_x * sd_y)
}

scaleControl <- function(data, control = NULL, weights = NULL) {
  #if (!is.list(data)) {
  #  data = list(data)
  #}
  if (is.null(control)) {
    if (is.null(weights)) {
      return(scale(data))
    } else {
      return(corpcor::wt.scale(data, w = weights))
    }
  }

  if (is.null(weights)) {
    returnData = lapply(data, function(x) return((x-mean(x[control==TRUE], na.rm=TRUE))/sd(x[control==TRUE], na.rm=TRUE)))
  } else  {
    returnData = lapply(data, function(x) return((x-weighted.mean(x[control==TRUE], w = weights, na.rm=TRUE)) /
                                                   sqrt(Hmisc::wtd.var(x[control==TRUE], w = weights, na.rm=TRUE))))
  }
  return (returnData)
}


## Create ideology index
# Create an index with equal weihgts for covariates x
# directions - if directions are provided all variables are first reoriented to have the same interpretation. directions should be 1 or -1
# scale - should each covariate be standardized before creating the index
# scaleIndex - should the final index be standardized
# control - if a control group is provided and the index should be standardized, it is standardized with respect to the control group
createStandardIndex <- function(x, control=NULL, directions=NULL, scale=TRUE, scaleIndex=TRUE, weights = NULL) {
  if (!is.null(directions)) {
    x = as.matrix(x) * matrix(rep(directions, nrow(x)), byrow = TRUE, ncol=ncol(x))
  }

  if (scale==TRUE) {
    scaled = simplify2array(scaleControl(x, control, weights))
  } else {
    scaled = simplify2array(x)
  }
  index = rowMeans(scaled, na.rm=TRUE)

  if (scaleIndex==TRUE) {
    if (!is.null(control)) {
      index = scaleControl(index, control, weights)
    } else {
      if (is.null(weights)) {
        index = scale(index)
      } else {
        index = corpcor::wt.scale(index, w = weights)
      }

    }
  } else {
    index = list(index)
  }

  return(index)
}

