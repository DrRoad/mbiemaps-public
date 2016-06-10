\name{Region_CINZ}
\alias{Region_CINZ}
\alias{region_CINZ_simpl}
\alias{region_CINZ_simpl_gg}
\alias{region_CAS_simpl_gg}
\docType{data}
\title{
CINZ/CAS region boundaries.
}
\description{
The spatial polygons data frame files for the boundaries of CINZ(Conventions and Incentives New Zealand) regions and CAS(Conventions Activity Survey) regions.
}
\usage{data(Region_CINZ)
data(region_CINZ_simpl)
data(region_CINZ_simpl_gg)
data(region_CAS_simpl_gg)}
\format{
 Spatial polygons dataframe, in latitude and longitude
 }
\details{
This object was created by converting RTO/area units boundaries to CINZ region boundaries.  

\code{region_CINZ_simpl} is a spatial polygons data frame; \code{region_CINZ_simpl_gg} is a data frame created from region_CINZ_simpl by means of the fortify function. 

The name, length, area and centre of each polygon is stored in the data slot of Region_CINZ.  region_CINZ_simpl does not have the area information at this point.
}
\source{
Statistics New Zealand
}
\references{
\href{http://www.stats.govt.nz/browse_for_stats/people_and_communities/Geographic-areas/digital-boundary-files.aspx}{http://www.stats.govt.nz/browse_for_stats/people_and_communities/Geographic-areas/digital-boundary-files.aspx},
\href{http://www.conventionsnz.co.nz/PicsHotel/CINZ2009/CustomPages/DiscoverPres.aspx?IDL=2&IDType=309&ID=1597}{http://www.conventionsnz.co.nz/PicsHotel/CINZ2009/CustomPages/DiscoverPres.aspx?IDL=2&IDType=309&ID=1597}
}

\note{
Mapping seems to work better in an R console (eg in conjunction with Notepad++) than in RStudio.
}

\seealso{
\code{\link{Region}}, \code{\link{TA}}, \code{\link{AU}}
}

\examples{
\donttest{
require(sp)
data(Region_CINZ)
plot(Region_CINZ)
axis(1)
axis(2)

data(region_CINZ_simpl)
plot(region_CINZ_simpl)

data(region_CINZ_simpl_gg)
head(region_CINZ_simpl_gg)
ggplot(region_CINZ_simpl_gg, aes(x=long, y=lat, group=group)) + 
  geom_polygon(color="black", fill="white") +
  theme_nothing() +
  coord_map()
}
}
\keyword{datasets}
