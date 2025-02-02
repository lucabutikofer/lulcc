% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Model.R
\name{OrderedModelPred}
\alias{OrderedModelPred}
\alias{OrderedModelPred,ObsLulcRasterStack,ExpVarRasterList-method}
\alias{OrderedModelPred,ObsLulcRasterStack,ExpVarRasterList,PredictiveModelList-method}
\title{Create an OrderedModelPred object}
\usage{
OrderedModelPred(obs, ef, prediction, ...)

\S4method{OrderedModelPred}{ObsLulcRasterStack,ExpVarRasterList}(
  obs,
  ef,
  time,
  prediction,
  demand,
  hist,
  mask,
  neighb = NULL,
  rules = NULL,
  nb.rules = NULL,
  order,
  params,
  output = NULL,
  ...
)
}
\arguments{
\item{obs}{an ObsLulcRasterStack object}

\item{ef}{an ExpVarRasterList object}

\item{prediction}{model predictions as matrix, rows in the same order as
non-NA cells in land-use raster; colnames must correspond to "obs" categories.
-->> Change this to new class
PredLulcRasterStack.}

\item{\dots}{additional arguments (none)}

\item{time}{numeric vector containing timesteps over which simulation will
occur}

\item{demand}{matrix with demand for each land use category in terms of number
of cells to be allocated. The first row should be the number of cells
allocated to the initial observed land use map (i.e. the land use map for
time 0)}

\item{hist}{RasterLayer containing land use history (values represent the
number of years the cell has contained the current land use category)}

\item{mask}{RasterLayer containing binary values where 0 indicates cells
that are not allowed to change}

\item{neighb}{an object of class NeighbRasterStack}

\item{rules}{matrix with land use change decision rules}

\item{nb.rules}{numeric with neighbourhood decision rules}

\item{order}{numeric vector of land use categories in the order that change
should be allocated. See Details}

\item{params}{list with model parameters}

\item{output}{either a RasterStack containing output maps or NULL}
}
\value{
An OrderedModelPred object.
}
\description{
Methods to create a \code{OrderedModelPred} object to supply to
\code{\link{allocate}}.
}
\details{
The \code{params} argument is a list of parameter values which should contain
the following components:

\describe{
  \item{\code{max.diff}}{The maximum allowed difference between allocated and
    demanded area of any land use type. Default is 5}
}
}
\examples{

## see lulcc-package examples
}
\references{
Fuchs, R., Herold, M., Verburg, P.H., and Clevers, J.G.P.W. (2013). A
high-resolution and harmonized model approach for reconstructing and analysing
historic land changes in Europe, Biogeosciences, 10:1543-1559.
}
\seealso{
\code{\link{OrderedModelPred-class}}, \code{\link{allocate}}
}
