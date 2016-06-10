\name{AU_TA_lookup}
\alias{AU_TA_lookup}

\docType{data}
\title{
Lookup table: AU(Area Units) to TA(Territorial Authorities)
}
\description{
A lookup table convert AU(Area Units) to corresponding RTO (regions used for Regional Tourism Orgnaization) or RTI_TA (Territorial Authorities used for regional tourism indicators).
}
\usage{data(AU_TA_lookup)}


\examples{
\donttest{
require(sp)
data(AU, AU_TA_lookup)
id <- merge(AU@data, AU_TA_lookup, all.x=TRUE, by.x="NAME", by.y="area_unit", sort=FALSE)

# Create TA object based on merging area units 
TA_TRED <- unionSpatialPolygons(AU, IDs=id$RTI_TA) 
N <- data.frame(RTI_TA=unique(id$RTI_TA))
row.names(N) <- N[,1]
TA_TRED <- SpatialPolygonsDataFrame(TA_TRED, data = N) 

# remove area outside TA
TA_TRED <- subset(TA_TRED,RTI_TA != "Area Outside Territorial Authority") # new code, seems a proper way to exclude polygons.

spplot(TA_TRED,"RTI_TA")
}
}
\keyword{datasets}
