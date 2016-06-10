\name{Region_HLFS}
\alias{Region_HLFS}
\alias{region_HLFS_simpl}
\alias{region_HLFS_simpl_gg}
\docType{data}
\title{
HLFS region boundaries.
}
\description{
The spatial polygons data frame files for the boundaries of HLFS (Household Labour Force Survey) regions .
}
\usage{data(Region_HLFS)
data(region_HLFS_simpl)
data(region_HLFS_simpl_gg)}
\format{
 Spatial polygons dataframe, in latitude and longitude
 }
\details{
This object was created by grouping area units boundaries to HLFS region boundaries.  

\code{region_HLFS_simpl} is a spatial polygons data frame; \code{region_HLFS_simpl_gg} is a data frame created from region_HLFS_simpl by means of the fortify function. 

The name, area and centre of each polygon is stored in the data slot of Region_HLFS.
}
\source{
Grouped from Region data according to the specifications of Stuart King (stuart.king@mbie.govt.nz).  The Region data comes from Statistics New Zealand, Ministry of Maori Development. 
}
\references{
}

\note{
Mapping seems to work better in an R console (eg in conjunction with Notepad++) than in RStudio.
}

\seealso{
\code{\link{Region_Jobs_Online}}, \code{\link{Region_TPK}}, \code{\link{Region}}, \code{\link{AU}}
}

\examples{
\donttest{

#=================Example 1: Region_HLFS_simpl - leaflet example===================
library(mbiemaps)
library(leaflet)
library(dplyr)

data(region_HLFS_simpl) 
pal <- colorQuantile("YlGn", NULL, n = 5)
region_popup <- paste0("HLFS Region: <strong>", 
                     region_HLFS_simpl@data$NAME, "</strong>")

# Colour of regions is mapped to their size
region_HLFS_simpl \%>\%
  leaflet() \%>\%
  addPolygons(fillColor = ~pal(as.numeric(area)), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = region_popup)

#=================Example 2: Region_HLFS_simpl_gg - a simple plot===================
library(mbiemaps)

#display the first ten rows of data
data(region_HLFS_simpl_gg)
head(region_HLFS_simpl_gg)

#A simple plot
ggplot(region_HLFS_simpl_gg, aes(x=long, y=lat, group=group)) + 
  geom_polygon(color="black", fill="white") +
  coord_map()

 =======

#A coloured and labelled version
ggplot() +
   geom_polygon(data=region_HLFS_simpl_gg, 
                aes(x=long, y=lat, group=group, fill=NAME),
                colour="grey95") +
   geom_text(data = unique(region_HLFS_simpl_gg[ , c("NAME", "long.centre", "lat.centre")]), 
             aes(x=long.centre, y=lat.centre, label=NAME)) +
   scale_fill_brewer(palette="Set3") +
   coord_map()
}
}
\keyword{datasets}
