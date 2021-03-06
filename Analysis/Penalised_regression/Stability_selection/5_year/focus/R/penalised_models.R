#' Penalised regression
#'
#' Runs penalised regression using implementation from
#' \code{\link[glmnet]{glmnet}}. This function is not using stability.
#'
#' @inheritParams VariableSelection
#' @param ... additional parameters passed to \code{\link[glmnet]{glmnet}}.
#'
#' @return A list with: \item{selected}{matrix of binary selection status. Rows
#'   correspond to different model parameters. Columns correspond to
#'   predictors.} \item{beta_full}{array of model coefficients. Rows correspond
#'   to different model parameters. Columns correspond to predictors. Indices
#'   along the third dimension correspond to outcome variable(s).}
#'
#' @family underlying algorithm functions
#' @seealso \code{\link{SelectionAlgo}}, \code{\link{VariableSelection}}
#'
#' @examples
#'
#' # Data simulation
#' set.seed(1)
#' simul <- SimulateRegression(pk = 50)
#'
#' # Running the LASSO
#' mylasso <- PenalisedRegression(
#'   xdata = simul$X, ydata = simul$Y,
#'   Lambda = c(0.1, 0.2), family = "gaussian"
#' )
#' @export
PenalisedRegression <- function(xdata, ydata, Lambda = NULL, family, ...) {
  # Storing extra arguments
  extra_args <- list(...)

  # Running the regression
  if (family == "multinomial") {
    # Extracting relevant extra arguments
    ids <- which(names(extra_args) %in% names(formals(glmnet::glmnet)))
    ids <- ids[!ids %in% c("x", "y", "lambda", "family", "type.multinomial")]

    # Running model
    mymodel <- do.call(glmnet::glmnet, args = c(list(x = xdata, y = ydata, lambda = Lambda, family = family, type.multinomial = "grouped"), extra_args[ids]))
  } else {
    # Extracting relevant extra arguments
    ids <- which(names(extra_args) %in% names(formals(glmnet::glmnet)))
    ids <- ids[!ids %in% c("x", "y", "lambda", "family")]

    # Running model
    mymodel <- do.call(glmnet::glmnet, args = c(list(x = xdata, y = ydata, lambda = Lambda, family = family), extra_args[ids]))
  }

  if (!is.infinite(mymodel$lambda[1])) {
    # Extracting and formatting the beta coefficients
    if (!family %in% c("mgaussian", "multinomial")) {
      mybeta <- stats::coef(mymodel)
      mybeta <- t(as.matrix(mybeta))

      # Preparing the outputs
      beta_full <- mybeta[, colnames(xdata), drop = FALSE] # removing the intercept if included
      if ("penalty.factor" %in% names(extra_args)) {
        selected <- ifelse(mybeta[, colnames(xdata)[which(extra_args$penalty.factor == 1)], drop = FALSE] != 0, yes = 1, no = 0)
      } else {
        selected <- ifelse(mybeta[, colnames(xdata), drop = FALSE] != 0, yes = 1, no = 0)
      }
    } else {
      if (family == "mgaussian") {
        mybeta <- array(NA,
          dim = c(length(Lambda), ncol(xdata), ncol(ydata)),
          dimnames = list(paste0("s", 0:(length(Lambda) - 1)), colnames(xdata), colnames(ydata))
        )
        for (y_id in 1:ncol(ydata)) {
          tmpbeta <- stats::coef(mymodel)[[y_id]]
          tmpbeta <- t(as.matrix(tmpbeta))
          tmpbeta <- tmpbeta[, colnames(xdata), drop = FALSE] # removing the intercept if included
          mybeta[rownames(tmpbeta), colnames(tmpbeta), y_id] <- tmpbeta
        }
      }
      if (family == "multinomial") {
        y_levels <- sort(unique(ydata))
        mybeta <- array(NA,
          dim = c(length(Lambda), ncol(xdata), length(y_levels)),
          dimnames = list(
            paste0("s", 0:(length(Lambda) - 1)), colnames(xdata),
            paste0("Y", y_levels)
          )
        )
        for (y_id in 1:length(y_levels)) {
          tmpbeta <- stats::coef(mymodel)[[y_id]]
          tmpbeta <- t(as.matrix(tmpbeta))
          tmpbeta <- tmpbeta[, colnames(xdata), drop = FALSE] # removing the intercept if included
          mybeta[rownames(tmpbeta), colnames(tmpbeta), y_id] <- tmpbeta
        }
      }

      # Preparing the outputs
      if ("penalty.factor" %in% names(extra_args)) {
        selected <- ifelse(mybeta[, colnames(xdata)[which(extra_args$penalty.factor == 1)], 1, drop = FALSE] != 0, yes = 1, no = 0)
      } else {
        selected <- ifelse(mybeta[, , 1, drop = FALSE] != 0, yes = 1, no = 0)
      }
      beta_full <- mybeta
    }
  } else {
    # Returning infinite beta is the model failed
    selected <- beta_full <- Inf
  }

  return(list(selected = selected, beta_full = beta_full))
}


#' Graph estimation algorithm
#'
#' Runs the algorithm for estimation of an undirected graph with no self-loops
#' specified in the argument \code{implementation} and returns the estimated
#' adjacency matrix. This function is not using stability.
#'
#' @inheritParams GraphicalModel
#' @param xdata matrix with observations as rows and variables as columns.
#' @param Sequential_template logical matrix encoding the type of procedure to
#'   use for data with multiple blocks in stability selection graphical
#'   modelling. For multi-block estimation, the stability selection model is
#'   constructed as the union of block-specific stable edges estimated while the
#'   others are weakly penalised (\code{TRUE} only for the block currently being
#'   calibrated and \code{FALSE} for other blocks). Other approaches with joint
#'   calibration of the blocks are allowed (all entries are set to \code{TRUE}).
#' @param ... additional parameters passed to the function provided in
#'   \code{implementation}.
#'
#' @return An array with binary and symmetric adjacency matrices along the third
#'   dimension.
#'
#' @family underlying algorithm functions
#' @seealso \code{\link{GraphicalModel}}
#'
#' @details The use of the procedure from Equation (4) or (5) is controlled by
#'   the argument "Sequential_template".
#'
#' @examples
#' \dontrun{
#'
#' # Data simulation
#' set.seed(1)
#' simul <- SimulateGraphical()
#'
#' # Running graphical LASSO
#' myglasso <- GraphicalAlgo(xdata = simul$data, Lambda = matrix(c(0.1, 0.2), ncol = 1))
#' }
#' @export
PenalisedGraphical <- function(xdata, pk = NULL, Lambda, Sequential_template = NULL,
                               scale = TRUE, start = "cold", ...) {
  # Storing extra arguments
  extra_args <- list(...)

  # Create matrix with block indices
  bigblocks <- BlockMatrix(pk)
  bigblocks_vect <- bigblocks[upper.tri(bigblocks)]
  N_blocks <- unname(table(bigblocks_vect))
  blocks <- unique(as.vector(bigblocks_vect))
  names(N_blocks) <- blocks
  nblocks <- max(blocks)

  if (is.null(Sequential_template)) {
    Sequential_template <- BlockLambdaGrid(Lambda = Lambda)$Sequential_template
  }

  # Initialisation of array storing adjacency matrices
  adjacency <- array(NA, dim = c(ncol(xdata), ncol(xdata), nrow(Lambda)))

  for (k in 1:nrow(Lambda)) {
    # Creating penalisation matrix
    if (nblocks > 1) {
      lambdamat <- bigblocks
      for (b in 1:nblocks) {
        lambdamat[bigblocks == b] <- Lambda[k, b]
      }
    } else {
      lambdamat <- Lambda[k, 1]
    }

    # Estimation of the covariance
    if (scale) {
      cov_sub <- stats::cor(xdata)
    } else {
      cov_sub <- stats::cov(xdata)
    }

    # Extracting relevant extra arguments
    ids <- which(names(extra_args) %in% names(formals(glassoFast::glassoFast)))
    ids <- ids[!ids %in% c("S", "rho", "start", "w.init", "wi.init")] # avoid duplicates and args that cannot be manually set (related to warm start)

    # Estimation of the sparse inverse covariance
    if ((start == "warm") & (k != 1)) {
      if (all(which(Sequential_template[k, ]) == which(Sequential_template[k - 1, ]))) {
        # Warm start using
        g_sub <- do.call(glassoFast::glassoFast, args = c(
          list(
            S = cov_sub, rho = lambdamat,
            start = "warm", w.init = sigma, wi.init = omega
          ),
          extra_args[ids]
        ))
      } else {
        # Cold start if first iteration for the block
        g_sub <- do.call(glassoFast::glassoFast, args = c(
          list(S = cov_sub, rho = lambdamat),
          extra_args[ids]
        ))
      }
    } else {
      # Cold start
      g_sub <- do.call(glassoFast::glassoFast, args = c(
        list(S = cov_sub, rho = lambdamat),
        extra_args[ids]
      ))
    }
    omega <- g_sub$wi
    sigma <- g_sub$w

    # Creating adjacency matrix
    A <- ifelse(omega != 0, yes = 1, no = 0)
    A <- A + t(A)
    A <- ifelse(A != 0, yes = 1, no = 0)
    diag(A) <- 0

    adjacency[, , k] <- A
  }

  return(adjacency)
}
