#' add coordinates to sp object
#' @title Add coordinates to sp object
#' @importFrom sp coordinates

#' @description A wrap function which add coordinates to sp object.

#' @rdname AddCoord
#' @name AddCoord
#' @param sp_object a sp(SpatialPolygons) object to be added coordinates.

#' @return data of sp object with coordinates added.

#' @examples \donttest{
#' data(AU)
#' AU@@data$long <- AU@@data$lat <- NULL
#' head(AU@@data)
#' AU@@data <- AddCoord(AU)
#' head(AU@@data)
#' }

#' @export AddCoord
AddCoord <- function(sp_object){
  tmp <- as.data.frame(coordinates(sp_object))
  names(tmp) <- c("long", "lat")
  
  sp_object@data$long <- sp_object@data$lat <- NULL
  
  tmp <- cbind(sp_object@data, tmp)
  return(tmp)
}