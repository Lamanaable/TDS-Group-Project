% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variable_selection.R
\name{SerialRegression}
\alias{SerialRegression}
\title{Stability selection in regression (internal)}
\usage{
SerialRegression(
  xdata,
  ydata = NULL,
  Lambda,
  pi_list = seq(0.6, 0.9, by = 0.01),
  K = 100,
  tau = 0.5,
  seed = 1,
  n_cat = 3,
  family = "gaussian",
  implementation = PenalisedRegression,
  resampling = "subsampling",
  PFER_method = "MB",
  PFER_thr = Inf,
  FDP_thr = Inf,
  output_data = FALSE,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{xdata}{matrix of predictors with observations as rows and variables as
columns.}

\item{ydata}{optional vector or matrix of outcome(s). If \code{family} is set
to \code{"binomial"} or \code{"multinomial"}, \code{ydata} can be a vector
with character/numeric values, or a factor.}

\item{Lambda}{matrix of parameters controlling the level of sparsity in the
underlying feature selection algorithm specified in \code{implementation}.
With \code{implementation="glmnet"}, \code{Lambda} contains penalty
parameters.}

\item{pi_list}{vector of thresholds in selection proportions. If
\code{n_cat=3}, these values must be \code{>0.5} and \code{<1}. If
\code{n_cat=2}, these values must be \code{>0} and \code{<1}.}

\item{K}{number of resampling iterations.}

\item{tau}{subsample size. Only used with \code{resampling="subsampling"}.}

\item{seed}{value of the seed.}

\item{n_cat}{number of categories used to compute the stability score.
Possible values are 2 or 3.}

\item{family}{type of regression model. If
\code{implementation=PenalisedRegression}, this argument is defined as in
\code{\link[glmnet]{glmnet}}. Possible values include \code{"gaussian"}
(linear regression), \code{"binomial"} (logistic regression),
\code{"multinomial"} (multinomial regression), and \code{"cox"} (survival
analysis).}

\item{implementation}{function to use for variable selection. By default,
\code{PenalisedRegression}, based on \code{\link[glmnet]{glmnet}}, is used
for regularised regression. Other possible functions are: \code{SparsePLS},
\code{GroupPLS} and \code{SparseGroupPLS}. Alternatively, a function with
arguments \code{xdata}, \code{ydata}, \code{Lambda}, \code{family} and
\code{...}, and returning a list of two matrices named \code{selected} and
\code{beta_full} of the correct dimensions can be used.}

\item{resampling}{resampling approach. Possible values are:
\code{"subsampling"} for sampling without replacement of a proportion
\code{tau} of the observations, or \code{"bootstrap"} for sampling with
replacement generating a resampled dataset with as many observations as in
the full sample. Alternatively, this argument can be a function to use for
resampling. This function must use arguments named \code{data} and
\code{tau} and return IDs of observations to be included in the resampled
dataset (see example in \code{\link{Resample}}).}

\item{PFER_method}{method used to compute the upper-bound of the expected
number of False Positives (or Per Family Error Rate, PFER). With
\code{PFER_method="MB"}, the method proposed by Meinshausen and Bühlmann
(2010) is used. With \code{PFER_method="SS"}, the method proposed by Shah
and Samworth (2013) under the assumption of unimodality is used.}

\item{PFER_thr}{threshold in PFER for constrained calibration by error
control. With \code{PFER_thr=Inf} and \code{FDP_thr=Inf}, unconstrained
calibration is used.}

\item{FDP_thr}{threshold in the expected proportion of falsely selected
features (or False Discovery Proportion, FDP) for constrained calibration
by error control. With \code{PFER_thr=Inf} and \code{FDP_thr=Inf},
unconstrained calibration is used.}

\item{output_data}{logical indicating if the input datasets \code{xdata} and
\code{ydata} should be included in the output.}

\item{verbose}{logical indicating if a loading bar and messages should be
printed.}

\item{...}{additional parameters passed to the functions provided in
\code{implementation} or \code{resampling}.}
}
\value{
A list with: \item{S}{a matrix of the best stability scores for
  different parameters controlling the level of sparsity in the underlying
  algorithm.} \item{Lambda}{a matrix of parameters controlling the level of
  sparsity in the underlying algorithm.} \item{Q}{a matrix of the average
  number of selected features by underlying algorithm with different
  parameters controlling the level of sparsity.} \item{Q_s}{a matrix of the
  calibrated number of stably selected features with different parameters
  controlling the level of sparsity.} \item{P}{a matrix of calibrated
  thresholds in selection proportions for different parameters controlling
  the level of sparsity in the underlying algorithm.} \item{PFER}{a matrix of
  the upper-bounds in PFER of calibrated stability selection models with
  different parameters controlling the level of sparsity.} \item{FDP}{a
  matrix of the upper-bounds in FDP of calibrated stability selection models
  with different parameters controlling the level of sparsity.} \item{S_2d}{a
  matrix of stability scores obtained with different combinations of
  parameters. Columns correspond to different tresholds in selection
  proportions.} \item{selprop}{a matrix of selection proportions. Columns
  correspond to predictors from \code{xdata}.} \item{Beta}{an array of model
  coefficients. Columns correspond to predictors from \code{xdata}. Indices
  along the third dimension correspond to different resampling iterations.
  With multivariate outcomes, indices along the fourth dimension correspond
  to outcome-specific coefficients.} \item{method}{a list of
  \code{implementation}, \code{family}, \code{resampling} and
  \code{PFER_method} values used for the run.} \item{param}{a list of
  \code{K}, \code{pi_list}, \code{tau}, \code{n_cat}, \code{pk}, \code{n}
  (number of observations), \code{PFER_thr}, \code{FDP_thr} and \code{seed}
  values used for the run. The datasets \code{xdata} and \code{ydata} are
  also included if \code{output_data=TRUE}.} For all objects except those
  stored in \code{methods} or \code{params}, rows correspond to parameter
  values stored in the output \code{Lambda}.
}
\description{
Runs stability selection regression models with different combinations of
parameters controlling the sparsity of the underlying selection algorithm
(e.g. penalty parameter for regularised models) and thresholds in selection
proportions. These two parameters are jointly calibrated by maximising the
stability score of the model (possibly under a constraint on the expected
number of falsely stably selected features). This function uses a serial
implementation and requires the grid of parameters controlling the underlying
algorithm as input (for internal use only).
}
\keyword{internal}
