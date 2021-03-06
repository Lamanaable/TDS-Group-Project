% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualisation.R
\name{Heatmap}
\alias{Heatmap}
\title{Heatmap visualisation}
\usage{
Heatmap(
  mat,
  colours = c("ivory", "navajowhite", "tomato", "darkred"),
  resolution = 10000,
  axes = TRUE,
  legend = TRUE,
  legend_length = 15,
  legend_range = NULL
)
}
\arguments{
\item{mat}{data matrix.}

\item{colours}{vector of colours used for the heatmap. By default a gradient
of colours ranging from ivory to dark red is used.}

\item{resolution}{number of different colours to use.}

\item{axes}{logical indicating if the row and column names of \code{mat}
should be displayed.}

\item{legend}{logical indicating if the colour bar should be included.}

\item{legend_length}{length of the colour bar.}

\item{legend_range}{range of the colour bar.}
}
\value{
A heatmap.
}
\description{
Produces a heatmap for visualisation of matrix entries.
}
\examples{
\dontrun{

# Data simulation
set.seed(1)
mat <- matrix(rnorm(200), ncol = 20)

# Generating heatmaps
Heatmap(mat = mat)
Heatmap(mat = mat, colours = c("lightgrey", "blue", "black"), legend = FALSE)
}

}
\seealso{
\code{\link{CalibrationPlot}}
}
