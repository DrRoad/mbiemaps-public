\name{RTO}
\alias{RTO}
\alias{rto_gg}
\docType{data}
\title{
Regional Tourism Organisation boundaries
}
\description{
A spatial polygons data frame file with the boundaries and names of the Regional Tourism Organisations (RTOs) as in 2012, with some simplifications eg for those in Canterbury.
}
\usage{data(RTO)
data(rto_gg)}
\format{
 RTO is a Spatial polygons dataframe, in latitude and longitude. rto_gg is a fortified version suitable for use with ggplot2.
 }
\details{
In no way should these boundaries be interpreted as of any meaning other than for statistical convenience in analysing and presenting tourism data.  They do not, for example, exactly match the actual boundaries between RTOs.

This object was created by dissolving area units to match (at least approximately) the RTOs.  The original area units information came from Statistics New Zealand.
}
\source{
New Zealand Ministry of Business, Innovation and Employment
}
\references{
}

\note{
Mapping seems to work better in an R console (eg in conjunction with Notepad++) than in RStudio.  RStudio is very slow to render maps.
}

\seealso{
}

\examples{
\donttest{
#==============Example 1, basic plot of RTO and TA using base graphics===================
require(sp)
data(RTO)
data(TA)
par(fg=NA)
plot(RTO, col=tourism.cols("Alternating"))
par(fg="black")
plot(TA, add=TRUE)
axis(1)
axis(2)

#==================Example 2, choropleth map of RTOs using leaflet package==================

library(mbiemaps)
library(leaflet)
library(dplyr)

data(RTO) 
pal <- colorQuantile("YlGn", NULL, n = 5)
npoints <- length(RTO@data$RTO)
RTO@data<-mutate(RTO@data,fillCol=runif(npoints, min=1, max=5))
RTO_popup <- paste0("RTO: <strong>", 
                      RTO@data$RTO, "</strong>")


# Colour of regions is mapped to their size
RTO \%>\%
   leaflet() \%>\%
   addPolygons(fillColor = ~pal(fillCol), 
               fillOpacity = 0.8, 
               color = "#BDBDC3", 
               weight = 1, 
               popup = RTO_popup)



#==================Example 3, choropleth map of RTOs using lattice package==================

# Load in RTO map
library(sp)
library(lattice)
library(latticeExtra)
library(mbieDBmisc)
data(RTO)

# Load in some Regional Tourism Estimate (RTE) data
RTEs <- ImportTourism("RTEs")

head(RTEs)
dom <- RTEs \%>\% 
   filter(Type == "Domestic", YEMar \%in\% c(2009,2013)) \%>\% 
   group_by(RTO,YEMar) \%>\% 
   summarise(Spend = sum(Spend)) \%>\% 
   tidyr::spread(YEMar, Spend) \%>\% 
   mutate(Growth=CAGR(.$"2013"/.$"2009",4)) \%>\% 
   ungroup \%>\%
   select(RTO, Growth, Spend2013=one_of("2013"))
	

# Merge the RTE with the data from the data slot of the RTO map
data2 <- left_join(RTO@data, dom, by="RTO")

# Check that the order was preserved (i.e. is same as the original RTO@data)
cbind(RTO@data, data2)

# Replace the data slot of our RTO map with the new data frame
RTO@data <- data2



#-------------draw map simple version with all the defaults--------------
# We only need to say which variable in the RTO@data slot to use for the colours
spplot(RTO, zcol="Growth")


#----------------draw map Fancier version - title, palette, border colour, fonts, added circle layer---------------
library(extrafont)

# Define colour palette
   # First, how many colours are needed that are in the negative band
   lessthanzero <- round((-min(RTO@data$Growth)) / sum(abs(range(RTO@data$Growth))) * 100)

   # then create a palette of colours from red to grey and out to MBIE blue
   cols <- c(colorRampPalette(c("red", "grey95"))(lessthanzero), colorRampPalette(c("grey95", mbie.cols(1)))(100-lessthanzero))

# Define coordinates at which to add circles
coords <- SpatialPoints(coordinates(RTO))

# Define circle size
sizes <- sqrt(RTO@data$Spend2013)/8

# Set parameters so that there will be no axis line ie no box around the plot
trellis.par.set("axis.line", list(col=NA,lty=1,lwd=1))

spplot(RTO, zcol="Growth", col.regions=cols, main="Domestic tourist spend", col="white", 
	sp.layout=list("sp.points", coords, pch=1, col="black", lwd=2, cex=sizes), # add circles layer
	par.settings=list(grid.pars=list(fontfamily="Georgia")))
grid.text("Circle size is proportional to\ndomestic tourism spend in 2013", .2,.7, gp=gpar(fontfamily="Calibri"))
grid.text("Average growth per year in domestic tourism 2009 - 2013", .93,.5, , rot=-90, gp=gpar(fontfamily="Calibri"))

#==================Example 4, ggplot2 example==================

data(rto_gg)

rto_gg_s <- unique(rto_gg[, c("long.centre", "lat.centre", "RTO")])
ggplot() +
  geom_polygon(data=rto_gg, aes(x=long, y=lat, group=group, fill=RTO)) +
  geom_text(aes(x=long.centre, y=lat.centre, label=RTO), data=rto_gg_s) +
  theme_nothing() +
  coord_map() +
  theme(legend.position="none")
}
}
\keyword{datasets}
