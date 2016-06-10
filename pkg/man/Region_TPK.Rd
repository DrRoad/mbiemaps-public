\name{Region_TPK}
\alias{Region_TPK}
\alias{region_TPK_simpl}
\alias{region_TPK_simpl_gg}
\docType{data}
\title{
TPK region boundaries.
}
\description{
The spatial polygons data frame files for the boundaries of TPK(Te Puni Kokiri, Ministry of Maori Development) regions .
}
\usage{data(Region_TPK)
data(region_TPK_simpl)
data(region_TPK_simpl_gg)}
\format{
 Spatial polygons dataframe, in latitude and longitude
 }
\details{
This object was created by grouping area units boundaries to TPK region boundaries.  

\code{region_TPK_simpl} is a spatial polygons data frame; \code{region_TPK_simpl_gg} is a data frame created from region_TPK_simpl by means of the fortify function. 

The name, length, area and centre of each polygon is stored in the data slot of Region_TPK.  region_TPK_simpl does not have the area information at this point.
}
\source{
Statistics New Zealand, Ministry of Maori Development
}
\references{
}

\note{
Mapping seems to work better in an R console (eg in conjunction with Notepad++) than in RStudio.
}

\seealso{
\code{\link{Region}}, \code{\link{TA}}, \code{\link{AU}}
}

\examples{
\donttest{

#=================Example 1: Region_TPK_simpl===================
library(mbiemaps)
library(leaflet)
library(dplyr)

data(region_TPK_simpl) 
pal <- colorQuantile("YlGn", NULL, n = 5)
region_popup <- paste0("Region_TPK: <strong>", 
                     region_TPK_simpl@data$NAME, "</strong>")

# Colour of regions is mapped to their Region Code
region_TPK_simpl \%>\%
  leaflet() \%>\%
  addPolygons(fillColor = ~pal(as.numeric(TPK_Region_Code)), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = region_popup)

#=================Example 2: Region_TPK_simpl_gg===================
data(region_TPK_simpl_gg)
head(region_TPK_simpl_gg)

#A simple plot
ggplot(region_TPK_simpl_gg, aes(x=long, y=lat, group=group)) + 
  geom_polygon(color="black", fill="white") +
  coord_map()

#A coloured and labelled version
ggplot() +
  geom_polygon(data=region_TPK_simpl_gg, 
               aes(x=long, y=lat, group=group, fill=TPK_Region),
               colour="grey95") +
  geom_text(data = unique(region_TPK_simpl_gg[ , c("TPK_Region", "long.centre", "lat.centre")]), 
               aes(x=long.centre, y=lat.centre, label=TPK_Region)) +
  scale_fill_brewer(palette="Set3") +
  coord_map()
}
}
\keyword{datasets}
