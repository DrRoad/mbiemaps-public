#' fortify sp object
#' @title fortify sp object
#' @importFrom ggplot2 fortify

#' @description A wrap function which fortify the sp(SpatialPolygons) object for ggplot.

#' @rdname fortify_sp_object
#' @name fortify_sp_object
#' @param sp_object a sp(SpatialPolygons) object to be simplified.
#' @param reg_col_name the column name of the data of sp_object@@h which uniquely identify the region.

#' @return a dataframe which is ready for ggplot.

#' @examples \donttest{
#' data(AU)
#' data(au_simpl)
#' AU_gg <- fortify_sp_object(AU)
#' au_simpl_gg <- fortify_sp_object(au_simpl)
#' object.size(AU)
#' object.size(au_simpl)
#' object.size(AU_gg)
#' object.size(au_simpl_gg)
#' ggplot(au_simpl_gg, aes(x=long, y=lat, group=group, fill=gsub(" Region", "", NAME))) + 
#'   geom_polygon(color="black") +
#'   theme_nothing() +
#'   labs(fill="")
#' }

#' @export fortify_sp_object
fortify_sp_object <- function(sp_object, reg_col_name="NAME"){
  if(length(unique(sp_object@data[,reg_col_name])) != nrow(sp_object@data)) {
    stop("The column corresponding to specified reg_col_name needs to be unique!")
  }
  
  tmp <- ggplot2::fortify(sp_object, region=reg_col_name)
  names(tmp) <- gsub("id", reg_col_name , names(tmp))
  tmp2 <- merge(tmp, sp_object@data, suffixes=c("", ".centre"), by=reg_col_name, sort=FALSE)
  return(tmp2)
}
