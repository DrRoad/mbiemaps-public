#' simplify sp object
#' @title simplify sp object
#' @importFrom rgeos gSimplify
#' @importFrom sp SpatialPolygonsDataFrame

#' @description A wrap function which simplify the sp(SpatialPolygons) object.

#' @rdname simplify_sp_object
#' @name simplify_sp_object
#' @param sp_object a sp(SpatialPolygons) object to be simplified.
#' @param tol Numerical tolerance value to be used by the Douglas-Peuker algorithm (as in gSimplify {rgeos}).


#' @return a simplified sp object

#' @examples \donttest{
#' data(AU)
#' au_simpl <- simplify_sp_object(AU)
#' object.size(AU) 
#' object.size(au_simpl)  # see the difference
#' }

#' @export simplify_sp_object
simplify_sp_object <- function(sp_object, tol = .0002){
  SPDF <- sp_object
  spdfdata <- SPDF@data
  SPDF1 <- rgeos::gSimplify(SPDF, tol=tol,topologyPreserve=T)
  sp_simpl <- SpatialPolygonsDataFrame(SPDF1,spdfdata)
  return(sp_simpl)
}
