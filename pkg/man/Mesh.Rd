\name{Mesh}
\alias{Mesh}
\alias{mesh_simpl}
\alias{mesh_simpl_gg}
\docType{data}
\title{
Mesh-block Unit boundaries
}
\description{
A spatial polygons data frame file with the boundaries and names of New Zealand's Mesh-blocks as in 2014.
}
\usage{data(Mesh)
data(mesh_simpl)
data(mesh_simpl_gg)}
\format{
 Spatial polygons dataframe, in latitude and longitude
 }
\details{
This object was created by importing a shapefile, downloaded from the Statistics New Zealand website, with \code{readOGR()} and using \code{spTransform} to project it in latitude and longitude (rather than NZTM coordinates).  The boundaries of Mesh have not been adjusted in any way.  

\code{mesh_simpl} is a spatial polygons data frame; \code{mesh_simpl_gg} is a data frame created from mesh_simpl by means of the fortify function. 

The name, length, area and centre of each polygon is stored in the data slot of Mesh.  mesh_simpl does not have the area information at this point.
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
\code{\link{Region}}, \code{\link{TA}}, \code{\link{AU}}
}

\examples{
\donttest{
data(Mesh, mesh_simpl, mesh_simpl_gg)

map_data <- subset(mesh_simpl_gg, REGC2014_N=="Wellington Region")
Map1 <- 
  ggplot(map_data, aes(long, lat, group = group, fill = MB2014)) + 
  geom_polygon(color="black", show.legend=FALSE) + 
  coord_map() + 
  labs(x = "", y = "", fill = "legend") + 
  ggtitle("Wellington Region") 

map_data <- subset(mesh_simpl_gg, REGC2014_N=="Wellington Region" & TA2014_NAM == "Wellington City")
Map2 <- 
  ggplot(map_data, aes(long, lat, group = group, fill = MB2014)) + 
  geom_polygon(color="black", show.legend=FALSE) + 
  coord_map() + 
  labs(x = "", y = "", fill = "legend") + 
  ggtitle("Wellington City") 

map_data <- subset(mesh_simpl_gg, REGC2014_N=="Wellington Region" & TA2014_NAM == "Wellington City" & AU2014_NAM == "Lambton")
Map3 <- 
  ggplot(map_data, aes(long, lat, group = group, fill = MB2014)) + 
  geom_polygon(color="black", show.legend=FALSE) + 
  coord_map() + 
  labs(x = "", y = "", fill = "legend") + 
  ggtitle("Lambton") 

print(Map1)
print(Map2)
print(Map3)
}
}
\keyword{datasets}
