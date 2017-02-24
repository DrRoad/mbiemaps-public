\name{AU}
\alias{AU}
\alias{au_simpl}
\alias{au_simpl_gg}
\docType{data}
\title{
Area Unit boundaries
}
\description{
A spatial polygons data frame file with the boundaries and names of New Zealand's Area Units as in 2014.
}
\usage{data(AU)
data(au_simpl)
data(au_simpl_gg)}
\format{
 Spatial polygons dataframe, in latitude and longitude
 }
\details{
This object was created by importing a shapefile, downloaded from the Statistics New Zealand website, with \code{readOGR()} and using \code{spTransform} to project it in latitude and longitude (rather than NZTM coordinates).  The boundaries of AU have not been adjusted in any way.  

\code{au_simpl} is a spatial polygons data frame which is simplified from \code{AU}; \code{au_simpl_gg} is a data frame created from au_simpl by means of the fortify function.  Unfortunately nine area units seem to have been lost on the way.  These are Ravenswood, Woodend West, Rangiora Central, Kaiapoi North West, Kaiapoi North East, Silverstream, Highfield Park, Prestons and Rolleston South East.

The name, length, area and centre of each polygon is stored in the data slot of AU.  au_simpl does not have the area information at this point.
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
\code{\link{Region}}, \code{\link{TA}}
}

\examples{
\donttest{
#================Example 1: au_simpl=====================
library(mbiemaps)
library(leaflet)
library(dplyr)

data(au_simpl) 
pal <- colorQuantile("YlGn", NULL, n = 5)
au_popup <- paste0("Area Unit Boundaries: <strong>", 
                      au_simpl@data$NAME, "</strong>")


# Colour of regions is mapped to their size
au_simpl \%>\%
   leaflet() \%>\%
   addPolygons(fillColor = ~pal(SHAPE_Area), 
               fillOpacity = 0.8, 
               color = "#BDBDC3", 
               weight = 1, 
               popup = au_popup)

#================Example 2: au_simpl_gg=====================                          
data(au_simpl_gg)
ggplot(au_simpl_gg, aes(x=long, y=lat, group=group, fill=gsub(" Region", "", NAME))) + 
   geom_polygon(color="black") +
   ggmap::theme_nothing() +
   labs(fill="") +
   coord_map()


}
}
\keyword{datasets}
