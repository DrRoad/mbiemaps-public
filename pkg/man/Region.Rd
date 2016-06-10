\name{Region}
\alias{Region}
\alias{region_simpl}
\alias{region_simpl_gg}
\docType{data}
\title{
Regional Council boundaries
}
\description{
A spatial polygons data frame file with the boundaries and names of New Zealand's Regional Councils as in 2014.
}
\usage{data(Region)
data(region_simpl)
data(region_simpl_gg)}
\format{
 \code{Region} and \code{region_simpl} are spatial polygons dataframes, in latitude and longitude.  \code{region_simpl_gg} is a fortified version suitable for use with ggplot2.
 }
\details{
The \code{data(Region)} object was created by importing a shapefile, downloaded from the Statistics New Zealand website, with \code{readOGR()} and using \code{spTransform} to project it in latitude and longitude (rather than NZTM coordinates).  The boundaries of Region have not been adjusted in any way.  

\code{region_simpl} brings in two objects.  \code{region_simpl} is a simplified spatial polygons dataframe (much smaller than Region), and \code{region_simpl_gg} is a version that has been "fortified"" and is suitable for use with ggplot (see example).

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
\code{\link{TA}}, \code{\link{AU}}
}

\examples{
\donttest{
#================Example 1: region_simpl=====================
library(mbiemaps)
library(leaflet)
library(dplyr)

data(region_simpl) 
pal <- colorQuantile("YlGn", NULL, n = 5)
region_popup <- paste0("Regional Council: <strong>", 
                     region_simpl@data$NAME, "</strong>")


# Colour of regions is mapped to their size
region_simpl \%>\%
   leaflet() \%>\%
   addPolygons(fillColor = ~pal(SHAPE_Area), 
               fillOpacity = 0.8, 
               color = "#BDBDC3", 
               weight = 1, 
               popup = region_popup)
               
#================Example 2: region_simpl_gg=====================                          
data(region_simpl_gg)
ggplot(region_simpl_gg, aes(x=long, y=lat, group=group, fill=gsub(" Region", "", NAME))) + 
   geom_polygon(color="black") +
   ggmap::theme_nothing() +
   labs(fill="") +
   coord_map()
}
}
\keyword{datasets}
