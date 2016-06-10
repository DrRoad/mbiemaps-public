\name{TA}
\alias{TA}
\alias{ta_simpl}
\alias{ta_simpl_gg}
\docType{data}
\title{
Territorial Authority boundaries
}
\description{
A spatial polygons data frame file with the boundaries and names of New Zealand's Territorial Authorities as in 2014.
}
\usage{data(TA)
data(ta_simpl)
data(ta_simpl_gg)}
\format{
 \code{TA} and \code{ta_simpl} are spatial polygons dataframes, in latitude and longitude.  \code{ta_simpl_gg} is a fortified version suitable for use with ggplot2.
 }
\details{
This object was created by importing a shapefile, downloaded from the Statistics New Zealand website, with \code{readOGR()} and using \code{spTransform} to project it in latitude and longitude (rather than NZTM coordinates).  The boundaries of TA have not been adjusted in any way.  

\code{ta_simpl} is a simplified spatial polygons dataframe (much smaller than TA), and \code{ta_simpl_gg} is a version that has been "fortified"" and is suitable for use with ggplot (see example).

The name, length, area and centre of each polygon is stored in the data slot of TA and ta_simpl and has been carried through to ta_simpl_gg.
}
\source{
Statistics New Zealand
}
\references{
\href{http://www.stats.govt.nz/browse_for_stats/people_and_communities/Geographic-areas/digital-boundary-files.aspx}{http://www.stats.govt.nz/browse_for_stats/people_and_communities/Geographic-areas/digital-boundary-files.aspx}
}



\note{
Mapping seems to work better in an R console (eg in conjunction with Notepad++) than in RStudio.
}

\seealso{
\code{\link{TA_TRED}}, \code{\link{Region}}
}

\examples{
\donttest{
#================Example 1: ta_simpl=====================
library(mbiemaps)
library(leaflet)
library(dplyr)

data(ta_simpl) 
pal <- colorQuantile("YlGn", NULL, n = 5)
ta_popup <- paste0("Territorial Authority: <strong>", 
                      ta_simpl@data$NAME, "</strong>")


# Colour of regions is mapped to their size
ta_simpl \%>\%
   leaflet() \%>\%
   addPolygons(fillColor = ~pal(SHAPE_Area), 
               fillOpacity = 0.8, 
               color = "#BDBDC3", 
               weight = 1, 
               popup = ta_popup)

#================Example 2: ta_simpl_gg=====================                          
data(ta_simpl_gg)
ggplot(ta_simpl_gg, aes(x=long, y=lat, group=group, fill=gsub(" Region", "", NAME))) + 
   geom_polygon(color="black") +
   ggmap::theme_nothing() +
   labs(fill="") +
   coord_map()

}
}
\keyword{datasets}
