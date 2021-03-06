#' Stability selection graphical model
#'
#' Runs stability selection graphical models with different combinations of
#' parameters controlling the sparsity of the underlying selection algorithm
#' (e.g. penalty parameter for regularised models) and thresholds in selection
#' proportions. These two parameters are jointly calibrated by maximising the
#' stability score of the model (possibly under a constraint on the expected
#' number of falsely stably selected features).
#'
#' @inheritParams VariableSelection
#' @param xdata data matrix with observations as rows and variables as columns.
#'   For multi-block stability selection, the variables in data have to be
#'   ordered by group.
#' @param pk optional vector encoding the grouping structure. Only used for
#'   multi-block stability selection where \code{pk} indicates the number of
#'   variables in each group. If \code{pk=NULL}, single-block stability
#'   selection is performed.
#' @param Lambda matrix of parameters controlling the level of sparsity in the
#'   underlying feature selection algorithm specified in \code{implementation}.
#'   If \code{implementation=PenalisedGraphical}, \code{Lambda} contains penalty
#'   parameters. If \code{Lambda=NULL}, \code{\link{LambdaGridGraphical}} is
#'   used to define a relevant grid. \code{Lambda} can be provided as a vector
#'   or a matrix with \code{length(pk)} columns. If \code{implementation} is not
#'   set to \code{PenalisedGraphical}, \code{Lambda} must be provided.
#' @param lambda_other_blocks optional vector of parameters controlling the
#'   level of sparsity in neighbour blocks for the multi-block procedure. To use
#'   jointly a specific set of parameters for each block,
#'   \code{lambda_other_blocks} must be set to \code{NULL} (not recommended).
#'   Only used for multi-block stability selection, i.e. if \code{length(pk)>1}.
#' @param implementation function to use for graphical modelling. If
#'   \code{implementation=PenalisedGraphical}, the algorithm implemented in
#'   \code{\link[glassoFast]{glassoFast}} is used for regularised estimation of
#'   a conditional independence graph. Alternatively, a function taking
#'   \code{xdata} and \code{Lambda} as arguments and returning a binary and
#'   symmetric matrix for which diagonal elements are equal to zero can be used.
#' @param start character string indicating if the algorithm should be
#'   initialised at the estimated (inverse) covariance with previous penalty
#'   parameters (\code{start="warm"}) or not (\code{start="cold"}). Using
#'   \code{start="warm"} can speed-up the computations, but could lead to
#'   convergence issues (in particular with small \code{Lambda_cardinal}). Only
#'   used for \code{implementation=PenalisedGraphical} (see argument
#'   \code{"start"} in \code{\link[glassoFast]{glassoFast}}).
#' @param scale logical indicating if the correlation (\code{scale=TRUE}) or
#'   covariance (\code{scale=FALSE}) matrix should be used as input of
#'   \code{\link[glassoFast]{glassoFast}} if
#'   \code{implementation=PenalisedGraphical}. Otherwise, this argument must be
#'   used in the function provided in \code{implementation}.
#' @param lambda_max optional maximum value for the grid in penalty parameters.
#'   If \code{lambda_max=NULL}, the maximum value is set to the maximum
#'   covariance in absolute value. Only used if
#'   \code{implementation=PenalisedGraphical} and \code{Lambda=NULL}.
#' @param lambda_path_factor multiplicative factor used to define the minimum
#'   value in the grid.
#' @param max_density threshold on the density. The grid is defined such that
#'   the density of the estimated graph does not exceed max_density.
#'
#' @details To ensure reproducibility of the results, the state of the random
#'   number generator is fixed to \code{seed}. For parallelisation of the code,
#'   stability selection results produced with different \code{seed}s and all
#'   other parameters equal can be combined (more details in
#'   \code{\link{Combine}}).
#'
#' @references \insertRef{ourstabilityselection}{focus}
#'
#'   \insertRef{stabilityselectionMB}{focus}
#'
#'   \insertRef{stabilityselectionSS}{focus}
#'
#' @return A list with: \item{S}{a matrix of the best (block-specific) stability
#'   scores for different (sets of) parameters controlling the level of sparsity
#'   in the underlying algorithm. } \item{Lambda}{a matrix of (block-specific)
#'   parameters controlling the level of sparsity. } \item{Q}{a matrix of
#'   average numbers of (block-specific) edges selected by the underlying
#'   algorihm for different (sets of) parameters controlling the level of
#'   sparsity.} \item{Q_s}{a matrix of calibrated numbers of (block-specific)
#'   stable edges for different (sets of) parameters controlling the level of
#'   sparsity in the underlying algorithm. } \item{P}{a matrix of calibrated
#'   (block-specific) thresholds in selection proportions for different (sets
#'   of) parameters controlling the level of sparsity in the underlying
#'   algorithm. } \item{PFER}{a matrix of the (block-specific) upper-bounds in
#'   PFER of calibrated stability selection models with different (sets of)
#'   parameters controlling the level of sparsity in the underlying algorithm.}
#'   \item{FDP}{a matrix of the (block-specific) upper-bounds in FDP of
#'   calibrated stability selection models with different (sets of) parameters
#'   controlling the level of sparsity in the underlying algorithm.}
#'   \item{S_2d}{an array of (block-specific) stability scores obtained with
#'   different combinations of parameters. Columns correspond to different
#'   thresholds in selection proportions. In multi-block stability selection,
#'   indices along the third dimension correspond to different blocks.}
#'   \item{PFER_2d}{an array of computed upper-bounds of PFER obtained with
#'   different combinations of parameters. Columns correspond to different
#'   thresholds in selection proportions. Only available for single-block
#'   stability selection.} \item{FDP_2d}{an array of computed upper-bounds of
#'   FDP obtained with different combinations of parameters. Columns correspond
#'   to different thresholds in selection proportions. Only available for
#'   single-block stability selection.} \item{selprop}{an array of selection
#'   proportions. Rows and columns correspond to nodes in the graph. Indices
#'   along the third dimension correspond to different (sets of) parameters
#'   controlling the level of sparsity in the underlying algorithm.}
#'   \item{sign}{a matrix of signs of Pearson's correlations estimated from
#'   \code{xdata}.} \item{method}{a list with \code{type="graphical_model"},
#'   \code{implementation}, \code{start}, \code{resampling} and
#'   \code{PFER_method} values used for the run.} \item{params}{a list with
#'   values of other objects used for the run.} For all objects except
#'   \code{selprop}, \code{sign} and those stored in \code{methods} or
#'   \code{params}, rows correspond to parameter values stored in the output
#'   \code{Lambda}. In multi-block stability selection, columns of these same
#'   objects except correspond to different blocks.
#'
#' @family stability selection functions
#' @seealso \code{\link{LambdaGridGraphical}}, \code{\link{Resample}},
#'   \code{\link{GraphicalAlgo}}, \code{\link{Combine}},
#'   \code{\link{StabilityScore}}
#'
#' @examples
#' \dontshow{
#' # Single-block stability selection
#' set.seed(1)
#' simul <- SimulateGraphical(n = 50, pk = 10, nu = 0.1)
#' stab <- GraphicalModel(xdata = simul$data, K = 5, verbose = FALSE)
#' CalibrationPlot(stab)
#' A <- Adjacency(stab)
#' mygraph <- Graph(A)
#' perf <- SelectionPerformance(theta = A, theta_star = simul$theta)
#' perfgraph <- SelectionPerformanceGraph(
#'   theta = Adjacency(stab),
#'   theta_star = simul$theta, plot = TRUE
#' )
#' SelectionProportions(stab)
#'
#' # Multi-block stability selection
#' set.seed(1)
#' pk <- c(10, 10)
#' simul <- SimulateGraphical(n = 50, pk = pk)
#' stab <- GraphicalModel(xdata = simul$data, pk = pk, Lambda_cardinal = 10, K = 5, verbose = FALSE)
#' CalibrationPlot(stab)
#' A <- Adjacency(stab)
#' mygraph <- Graph(A)
#' perf <- SelectionPerformance(theta = A, theta_star = simul$theta, pk = pk)
#' SelectionProportions(stab)
#' }
#' \dontrun{
#'
#' # Single-block stability selection
#' set.seed(1)
#' simul <- SimulateGraphical(n = 100, pk = 20, nu = 0.1)
#' stab <- GraphicalModel(xdata = simul$data)
#' plot(Graph(stab))
#'
#' # Multi-block stability selection
#' set.seed(1)
#' simul <- SimulateGraphical(pk = c(10, 10))
#' stab <- GraphicalModel(xdata = simul$data, pk = c(10, 10), Lambda_cardinal = 10)
#' stab$Lambda # sets of penalty parameters used jointly
#'
#' # Multi-parameter stability selection (not recommended)
#' Lambda <- matrix(c(0.8, 0.6, 0.3, 0.5, 0.4, 0.3, 0.7, 0.5, 0.1), ncol = 3)
#' stab <- GraphicalModel(
#'   xdata = simul$data, pk = c(10, 10),
#'   Lambda = Lambda, lambda_other_blocks = NULL
#' )
#' stab$Lambda
#'
#' # Example with user-defined function: shrinkage estimation and selection
#' set.seed(1)
#' simul <- SimulateGraphical(n = 100, pk = 20, nu = 0.1)
#' if (requireNamespace("corpcor", quietly = TRUE)) {
#'   ShrinkageSelection <- function(xdata, Lambda, ...) {
#'     mypcor <- corpcor::pcor.shrink(xdata, verbose = FALSE)
#'     adjacency <- NULL
#'     for (k in 1:nrow(Lambda)) {
#'       A <- ifelse(abs(mypcor) >= Lambda[k, 1], yes = 1, no = 0)
#'       diag(A) <- 0
#'       adjacency <- abind::abind(adjacency, A, along = 3)
#'     }
#'     return(adjacency)
#'   }
#'   myglasso <- GraphicalAlgo(
#'     xdata = simul$data,
#'     Lambda = matrix(c(0.05, 0.1), ncol = 1), implementation = ShrinkageSelection
#'   )
#'   stab <- GraphicalModel(
#'     xdata = simul$data, Lambda = matrix(c(0.01, 0.05, 0.1), ncol = 1),
#'     implementation = ShrinkageSelection
#'   )
#'   stable_adjacency <- Adjacency(stab)
#' }
#' }
#' @export
GraphicalModel <- function(xdata, pk = NULL, Lambda = NULL, lambda_other_blocks = 0.1,
                           pi_list = seq(0.6, 0.9, by = 0.01), K = 100, tau = 0.5, seed = 1, n_cat = 3,
                           implementation = PenalisedGraphical, start = "warm", scale = TRUE,
                           resampling = "subsampling", PFER_method = "MB", PFER_thr = Inf, FDP_thr = Inf,
                           Lambda_cardinal = 50, lambda_max = NULL, lambda_path_factor = 0.001, max_density = 0.5,
                           n_cores = 1, output_data = FALSE, verbose = TRUE, ...) {
  # Definition of the type of approach (single or multi-block)
  if (is.null(pk)) {
    pk <- ncol(xdata)
  }
  if (length(pk) > 1) {
    calibration <- "multi-block"
  } else {
    calibration <- "single-block"
  }

  # Error and warning messages
  bigblocks <- bigblocks_vect <- blocks <- N_blocks <- nblocks <- PFER_thr_blocks <- FDP_thr_blocks <- NULL
  CheckInputGraphical(
    xdata = xdata, pk = pk, Lambda = Lambda, lambda_other_blocks = lambda_other_blocks,
    pi_list = pi_list, K = K, tau = tau, seed = seed, n_cat = n_cat,
    implementation = implementation, start = start, scale = scale,
    resampling = resampling, PFER_method = PFER_method, PFER_thr = PFER_thr, FDP_thr = FDP_thr,
    Lambda_cardinal = Lambda_cardinal,
    lambda_max = lambda_max, lambda_path_factor = lambda_path_factor, max_density = max_density,
    verbose = verbose
  )

  # Launching stability selection and calibration
  if (is.null(Lambda)) {
    # Defining a broad grid of lambda values
    Lambda <- LambdaGridGraphical(
      xdata = xdata, pk = pk, lambda_other_blocks = lambda_other_blocks, tau = tau,
      implementation = implementation, start = "cold", scale = scale,
      resampling = resampling, PFER_method = PFER_method, PFER_thr = PFER_thr, FDP_thr = FDP_thr,
      lambda_max = lambda_max, lambda_path_factor = lambda_path_factor, max_density = max_density,
      Lambda_cardinal = Lambda_cardinal, ...
    )
  }

  # Check if parallelisation is possible (forking)
  if (.Platform$OS.type != "unix") {
    if (n_cores > 1) {
      warning("Invalid input for argument 'n_cores'. Parallelisation relies on forking, it is only available on Unix systems.")
    }
    n_cores <- 1
  }

  # Stability selection and score
  mypar <- parallel::mclapply(X = 1:n_cores, FUN = function(k) {
    return(SerialGraphical(
      xdata = xdata, pk = pk, Lambda = Lambda, lambda_other_blocks = lambda_other_blocks,
      pi_list = pi_list, K = ceiling(K / n_cores), tau = tau, seed = as.numeric(paste0(seed, k)), n_cat = n_cat,
      implementation = implementation, start = start, scale = scale,
      resampling = resampling, PFER_method = PFER_method, PFER_thr = PFER_thr, FDP_thr = FDP_thr,
      output_data = output_data, verbose = verbose, ...
    ))
  })

  # Combining the outputs from parallel iterations
  out <- mypar[[1]]
  if (n_cores > 1) {
    for (i in 2:length(mypar)) {
      out <- do.call(Combine, list(stability1 = out, stability2 = mypar[[2]], graph = TRUE))
    }
  }

  # Re-set the function names
  if ("methods" %in% names(out)) {
    myimplementation <- as.character(substitute(implementation))
    if (is.function(resampling)) {
      myresampling <- as.character(substitute(resampling))
    } else {
      myresampling <- resampling
    }
    out$methods$implementation <- myimplementation
    out$methods$resampling <- myresampling
  }

  return(out)
}


#' Stability selection graphical model (internal)
#'
#' Runs stability selection graphical models with different combinations of
#' parameters controlling the sparsity of the underlying selection algorithm
#' (e.g. penalty parameter for regularised models) and thresholds in selection
#' proportions. These two parameters are jointly calibrated by maximising the
#' stability score of the model (possibly under a constraint on the expected
#' number of falsely stably selected features). This function uses a serial
#' implementation and requires the grid of parameters controlling the underlying
#' algorithm as input (for internal use only).
#'
#' @inheritParams GraphicalModel
#' @param Lambda matrix of parameters controlling the level of sparsity in the
#'   underlying feature selection algorithm specified in \code{implementation}.
#'   If \code{implementation="glassoFast"}, \code{Lambda} contains penalty
#'   parameters.
#'
#' @return A list with: \item{S}{a matrix of the best (block-specific) stability
#'   scores for different (sets of) parameters controlling the level of sparsity
#'   in the underlying algorithm. } \item{Lambda}{a matrix of (block-specific)
#'   parameters controlling the level of sparsity. } \item{Q}{a matrix of
#'   average numbers of (block-specific) edges selected by the underlying
#'   algorihm for different (sets of) parameters controlling the level of
#'   sparsity.} \item{Q_s}{a matrix of calibrated numbers of (block-specific)
#'   stable edges for different (sets of) parameters controlling the level of
#'   sparsity in the underlying algorithm. } \item{P}{a matrix of calibrated
#'   (block-specific) thresholds in selection proportions for different (sets
#'   of) parameters controlling the level of sparsity in the underlying
#'   algorithm. } \item{PFER}{a matrix of the (block-specific) upper-bounds in
#'   PFER of calibrated stability selection models with different (sets of)
#'   parameters controlling the level of sparsity in the underlying algorithm.}
#'   \item{FDP}{a matrix of the (block-specific) upper-bounds in FDP of
#'   calibrated stability selection models with different (sets of) parameters
#'   controlling the level of sparsity in the underlying algorithm.}
#'   \item{S_2d}{an array of (block-specific) stability scores obtained with
#'   different combinations of parameters. Columns correspond to different
#'   thresholds in selection proportions. In multi-block stability selection,
#'   indices along the third dimension correspond to different blocks.}
#'   \item{PFER_2d}{an array of computed upper-bounds of PFER obtained with
#'   different combinations of parameters. Columns correspond to different
#'   thresholds in selection proportions. Only available for single-block
#'   stability selection.} \item{FDP_2d}{an array of computed upper-bounds of
#'   FDP obtained with different combinations of parameters. Columns correspond
#'   to different thresholds in selection proportions. Only available for
#'   single-block stability selection.} \item{selprop}{an array of selection
#'   proportions. Rows and columns correspond to nodes in the graph. Indices
#'   along the third dimension correspond to different (sets of) parameters
#'   controlling the level of sparsity in the underlying algorithm.}
#'   \item{sign}{a matrix of signs of Pearson's correlations estimated from
#'   \code{xdata}.} \item{method}{a list with \code{implementation},
#'   \code{start}, \code{resampling} and \code{PFER_method} values used for the
#'   run.} \item{param}{a list with values of other objects used for the run.}
#'   For all objects except \code{selprop}, \code{sign} and those stored in
#'   \code{methods} or \code{params}, rows correspond to parameter values stored
#'   in the output \code{Lambda}. In multi-block stability selection, columns of
#'   these same objects except correspond to different blocks.
#'
#' @keywords internal
SerialGraphical <- function(xdata, pk = NULL, Lambda, lambda_other_blocks = 0.1,
                            pi_list = seq(0.6, 0.9, by = 0.01), K = 100, tau = 0.5, seed = 1, n_cat = n_cat,
                            implementation = PenalisedGraphical, start = "cold", scale = TRUE,
                            resampling = "subsampling", PFER_method = "MB", PFER_thr = Inf, FDP_thr = Inf,
                            output_data = FALSE, verbose = TRUE, ...) {
  # Marginal correlation to get sign of the relationship
  mycor_for_sign <- stats::cor(xdata)

  # Defining K if using complementary pairs (SS)
  if (PFER_method == "SS") {
    K <- ceiling(K / 2) * 2
    tau <- 0.5
  }

  # Creating matrix with block indices
  bigblocks <- BlockMatrix(pk)
  bigblocks_vect <- bigblocks[upper.tri(bigblocks)]
  N_blocks <- unname(table(bigblocks_vect))
  blocks <- unique(as.vector(bigblocks_vect))
  names(N_blocks) <- blocks
  nblocks <- max(blocks)

  # Preparing the PFER and FDP thresholds
  if (length(PFER_thr) == 1) {
    PFER_thr_blocks <- ceiling(prop.table(N_blocks) * PFER_thr)
  } else {
    if (length(PFER_thr) == nblocks) {
      PFER_thr_blocks <- PFER_thr
    }
  }
  if (length(FDP_thr) == 1) {
    FDP_thr_blocks <- rep(FDP_thr, nblocks)
  } else {
    if (length(FDP_thr) == nblocks) {
      FDP_thr_blocks <- FDP_thr
    }
  }

  # Re-formatting Lambda
  if (is.vector(Lambda)) {
    grid <- BlockLambdaGrid(Lambda = Lambda, lambda_other_blocks = lambda_other_blocks)
    Lambda <- grid$Lambda
    Sequential_template <- grid$Sequential_template
  } else {
    grid <- BlockLambdaGrid(Lambda = Lambda, lambda_other_blocks = lambda_other_blocks)
    Lambda <- grid$Lambda
    Sequential_template <- grid$Sequential_template
  }

  # Initialising array of selection proportions
  bigstab <- array(0,
    dim = c(ncol(xdata), ncol(xdata), nrow(Lambda)),
    dimnames = list(colnames(xdata), colnames(xdata), NULL)
  )

  # Setting seed for reproducibility
  withr::local_seed(seed)

  # Initialisation of the run
  if (verbose) {
    pb <- utils::txtProgressBar(style = 3)
  }

  # Using MB formula of the PFER
  if (PFER_method == "MB") {
    for (i in 1:K) {
      # Subsampling of the data
      s <- Resample(data = xdata, family = NULL, tau = tau, resampling = resampling, ...)
      xdata_sub <- xdata[s, ]

      # Estimation of the networks for different penalties
      A <- GraphicalAlgo(
        xdata = xdata_sub, pk = pk, Lambda = Lambda, Sequential_template = Sequential_template,
        scale = scale, implementation = implementation, start = start, ...
      )

      # Computing the selection counts
      for (k in 1:dim(A)[3]) {
        bigstab[, , k] <- bigstab[, , k] + A[, , k]
      }

      if (verbose) {
        utils::setTxtProgressBar(pb, i / K)
      }
    }
    # Getting selection proportions from selection counts
    for (k in 1:dim(bigstab)[3]) {
      bigstab[, , k] <- bigstab[, , k] / K
      diag(bigstab[, , k]) <- 0
    }
  }

  # Using complementary pairs and SS formula of the PFER
  if (PFER_method == "SS") {
    for (i in 1:ceiling(K / 2)) {
      # Sample 1
      s <- Resample(data = xdata, family = NULL, tau = tau, resampling = resampling, ...)
      xdata_sub <- xdata[s, ]

      # Estimation of the networks for different penalties
      A1 <- GraphicalAlgo(
        xdata = xdata_sub, pk = pk, Lambda = Lambda, Sequential_template = Sequential_template,
        scale = scale, implementation = implementation, start = start, ...
      )

      # Sample 2: everything not in sample 1
      xdata_sub <- xdata[-s, ]

      # Estimation of the networks for different penalties
      A2 <- GraphicalAlgo(
        xdata = xdata_sub, pk = pk, Lambda = Lambda, Sequential_template = Sequential_template,
        scale = scale, implementation = implementation, start = start, ...
      )

      # Computing the simultaneous selection counts
      for (k in 1:dim(A1)[3]) {
        A <- ifelse((A1[, , k] + A2[, , k]) == 2, yes = 1, no = 0)
        bigstab[, , k] <- bigstab[, , k] + A
      }

      if (verbose) {
        utils::setTxtProgressBar(pb, i / ceiling(K / 2))
      }
    }
    # Getting selection proportions from selection counts
    for (k in 1:dim(bigstab)[3]) {
      bigstab[, , k] <- bigstab[, , k] / ceiling(K / 2)
      diag(bigstab[, , k]) <- 0
    }
  }

  # Computation of the stability score
  if (K > 2) {
    metrics <- StabilityMetrics(
      selprop = bigstab, pk = pk, pi_list = pi_list, K = K, n_cat = n_cat,
      Sequential_template = Sequential_template, graph = TRUE,
      PFER_method = PFER_method, PFER_thr_blocks = PFER_thr_blocks, FDP_thr_blocks = FDP_thr_blocks
    )
    if (verbose) {
      utils::setTxtProgressBar(pb, 1)
      cat("\n")
    }
  } else {
    # Initialising objects to be filled
    Q <- matrix(NA, nrow = nrow(Lambda), ncol = nblocks)
    for (k in 1:nrow(Lambda)) {
      # Extracting corresponding selection proportions
      stab_iter <- bigstab[, , k]

      # Getting number of selected variables per block
      for (block_id in 1:nblocks) {
        stab_iter_block <- stab_iter[(bigblocks == block_id) & (upper.tri(bigblocks))] # selection proportions in the block
        q_block <- round(sum(stab_iter_block)) # average number of edges selected by the original procedure in the block
        Q[k, block_id] <- q_block
      }
    }
  }

  # Preparing outputs
  if (K > 2) {
    myimplementation <- as.character(substitute(implementation, env = parent.frame(n = 2)))
    if (is.function(resampling)) {
      myresampling <- as.character(substitute(resampling))
    } else {
      myresampling <- resampling
    }
    if (nblocks == 1) {
      out <- list(
        S = metrics$S, Lambda = Lambda,
        Q = metrics$Q, Q_s = metrics$Q_s, P = metrics$P,
        PFER = metrics$PFER, FDP = metrics$FDP,
        S_2d = metrics$S_2d, PFER_2d = metrics$PFER_2d, FDP_2d = metrics$FDP_2d,
        selprop = bigstab, sign = sign(mycor_for_sign),
        methods = list(
          type = "graphical_model", implementation = myimplementation, start = start,
          resampling = myresampling, PFER_method = PFER_method
        ),
        params = list(
          K = K, pi_list = pi_list, tau = tau, n_cat = n_cat,
          pk = pk, n = nrow(xdata),
          PFER_thr = PFER_thr, FDP_thr = FDP_thr, seed = seed,
          lambda_other_blocks = lambda_other_blocks, Sequential_template = Sequential_template
        )
      )
      if (output_data) {
        out$params <- c(out$params, list(xdata = xdata))
      }
    } else {
      out <- list(
        S = metrics$S, Lambda = Lambda,
        Q = metrics$Q, Q_s = metrics$Q_s, P = metrics$P,
        PFER = metrics$PFER, FDP = metrics$FDP,
        S_2d = metrics$S_2d,
        selprop = bigstab, sign = sign(mycor_for_sign),
        methods = list(
          type = "graphical_model", implementation = myimplementation, start = start,
          resampling = myresampling, PFER_method = PFER_method
        ),
        params = list(
          K = K, pi_list = pi_list, tau = tau, n_cat = n_cat,
          pk = pk, n = nrow(xdata),
          PFER_thr = PFER_thr, FDP_thr = FDP_thr, seed = seed,
          lambda_other_blocks = lambda_other_blocks, Sequential_template = Sequential_template
        )
      )
      if (output_data) {
        out$params <- c(out$params, list(xdata = xdata))
      }
    }
    return(out)
  } else {
    return(list(Q = Q))
  }
}
