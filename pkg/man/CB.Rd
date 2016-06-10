\name{CB}
\alias{CB}
\alias{cb_simpl}
\alias{cb_simpl_gg}
\docType{data}
\title{
Community Board boundaries
}
\description{
A spatial polygons data frame file with the boundaries and names of New Zealand's Community Boards as in 2014.
}
\usage{data(Region)}
\format{
 Spatial polygons dataframe, in latitude and longitude
 }
\details{
This object was created by importing a shapefile, downloaded from the Statistics New Zealand website, with \code{readOGR()} and using \code{spTransform} to project it in latitude and longitude (rather than NZTM coordinates).  The boundaries have not been adjusted in any way.  

The name, length, area and centre of each polygon is stored in the data slot.
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

#==========Example 1 - Map of all regions using the "leaflet" package=======
library(mbiemaps)
library(leaflet)
library(dplyr)

data(cb_simpl) 
pal <- colorQuantile("YlGn", NULL, n = 5)
cb_simpl_popup <- paste0("<strong>", 
                         cb_simpl@data$NAME, "</strong>")

   # Colour of regions is mapped to their size
cb_simpl \%>\%
   leaflet() \%>\%
   addPolygons(fillColor = ~pal(SHAPE_Area), 
               fillOpacity = 0.8, 
               color = "#BDBDC3", 
               weight = 1, 
               popup = cb_simpl_popup)


#===========Example 2 - Print a map of Auckland Local Boards===============

data(cb_simpl_gg)
map_data <- subset(cb_simpl_gg, TA2014_NAM=="Auckland")
AuckMap <- 
  ggplot(map_data, aes(long, lat, group = group, fill = NAME)) + 
  geom_polygon(color="black", show.legend=T) + 
  coord_map() + 
  labs(x = "long", y = "lat", fill = "legend") + 
  ggtitle("Local Boards - Auckland") 

print(AuckMap)
}
}
\keyword{datasets}
