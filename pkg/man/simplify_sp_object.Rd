\name{simplify_sp_object}
\alias{simplify_sp_object}
\title{simplify_sp_object}
\usage{
  simplify_sp_object(sp_object, tol = 2e-04)
}
\arguments{
  \item{sp_object}{A sp(SpatialPolygons) object to be
  simplified.}

  \item{tol}{Numerical tolerance value to be used by the
  Douglas-Peuker algorithm (as in gSimplify {rgeos}).}
}
\value{
  A simplified sp object
}
\description{
  A wrap function which simplifies an sp(SpatialPolygons)
  object.
}
\details{
  simplify sp object
}
\examples{
\donttest{
data(AU)
au_simpl <- simplify_sp_object(AU)
object.size(AU)
object.size(au_simpl)  # see the difference
}
}

