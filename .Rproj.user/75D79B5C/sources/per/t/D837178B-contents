###Point Pattern Analysis (PPA)###
library(spatstat)
library(sf)
library(sp)
#Analysis of point observation over an area
#To discuss this we'll refer to lecture notes by Manuel Gimond on : https://mgimond.github.io/Spatial/chp11_0.html
#This method was very popular before modern personal computer was common, mainly because of the practice is easy to do
#Theoritical summary is available at the written website address above.
#Now to illustrate how it works we'll prepare data of starbucks store location in Massachussets
load(url("https://github.com/mgimond/Spatial/raw/main/Data/ppa.RData"))
#We get 3 objects from the above load function: starbucks, ma, and pop
#Starbucks is the planar point pattern type of object with just rectangular "window"
starbucks
plot(starbucks)
#This is the type of data that usually being used for, just points of observation being coordinate
#For this particular data, however, is also enclosed in whats called "window"
#This window in general can be a polygon, which in practice represent the geographical area of observation 
starbucks$x
starbucks$y #2 dimensions (x and y) coordinate for each point
starbucks$window #Enclosing "window" or usually called polygon for general term
#This leads to the 2nd object : "ma"
plot(ma)#ma represent the geographical area or map of the location of the observation that is Massachusetts
#This ma window can be used as the enclosing area for point pattern analysis.
#For the last object : pop, it is an image object shows the population density for each sub area
plot(pop) #Such map on the course also known as raster or in a more popular term "heatmap"
#This object is used as predictor for the number of store within an area. Since prediction study is optional
#for this discussion, we will only focus on the rather simplistic PPA. Thus, we will only use starbucks and ma
#For those interested in dependence PPA can look up in the main references written in line 3
plot(starbucks, main=NULL, cols=rgb(0,0,0,.2), pch=20)


#Our discussion for this part will focus on a particular questions :
#(1) Based on the data, are the locations of the starbuck stores distributed randomly?
#(2) If not, what type of pattern seems to exist in this data?
#To answer those question, we'll proceed on the PPA with by first applying the quadrat mapping.
#Quadrat mapping or in general, quadrat method is the idea of dividing the area of observation into sub-areas called 
#quadrat which then for each of these individual quadrat, the density is calculated. This density is then used to test
#the null hypothesis of the store locations being randomly distributed. In a more general idea, null hypothesis
#for this method is used to test the complete spatial randomness (CSR) of the data.
Q = quadratcount(starbucks, nx= 6, ny=3)
plot(starbucks, pch=20, cols="grey70", main=NULL) 
plot(Q, add=TRUE) #Plot number of counts for each sub area
Q_d = intensity(Q)
plot(intensity(Q, image=TRUE), main=NULL, las=1)
plot(starbucks, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)#Density or known in spatial statistics as intensity plot
#The density above is the number of observed points per the area of the quadrat
#Now we'll test for CSR of the data
quadrat.test(starbucks, nx= 6, ny=3)
#For a side note, this test is similar to that of chi-square test. In other words we are testing whether the number of
#observed store in a quadrat is the same as the expected number of store in the quadrat.
#With such idea, it is clear that the design of the test is prone to difference of result when the definition
#of the quadrat is different.
#For this particular data, we dont really have to worry about such flaw. However in case of meeting the aforementioned
#situation, it is recommended to test quadrat CSR for several different quadrat setting then justify your decision
#based on the majority of the results.
quadrat.test(starbucks, nx= 5, ny=4)
quadrat.test(starbucks, nx= 3, ny=7)
quadrat.test(starbucks, nx= 10, ny=3)
#After finding that the data is not spatially random based on the above result, the following will discuss the follow-
#through.We will analyze using distance based method : Ripley's K function, L function, and G nearest neighbour function.

#First is K function, for more information : 
#https://cran.r-project.org/web/packages/spNetwork/vignettes/KNetworkFunctions.html#:~:text=The%20K%2Dfunction%20is%20a,expect%20from%20a%20given%20distribution.
K_star = Kest(starbucks)
plot(K_star, main="K Function", legendargs=list(cex=0.8, xpd=TRUE, inset=c(0, 0) ))
#From the plot above, we can simply observe the line of expected K function IF the data is spatially random (K pois)
#if it is below the K function lines for our observed data with different correction (iso, trans, bord) the data is 
#clustered. On the other hand, if it is above the observed K function lines the data is dispersed.
#On this particular example, starbucks store location seems to follow a clustered pattern.

#L function : https://en.wikipedia.org/wiki/Spatial_descriptive_statistics
#Similar to K function
L_star = Lest(starbucks)
plot(L_star, main="L Function", legendargs=list(cex=0.65, xpd=TRUE, inset=c(0, 0) ))
#From the plot above, it is concluded that the starbucks' stores locations seems to form cluster

#G nearest neighbour function
#A more in-depth summary : https://gistbok.ucgis.org/bok-topics/point-pattern-analysis
G_star = Gest(starbucks)
plot(G_star, main="G Function", legendargs=list(cex=0.65, xpd=TRUE, inset=c(0, 0) ))
#Same as the previous 2,  starbucks' stores locations form clustered pattern

#The following code reiterate the previous procedure but the window for the data is changed to be the actual map
#of massachusetts using object ma from before
Window(starbucks) = ma #change window into massachusetts polygon
plot(starbucks, main=NULL, cols=rgb(0,0,0,.2), pch=20)
#Mapping into quadrats
Q1 = quadratcount(starbucks, nx= 6, ny=3)
plot(starbucks, pch=20, cols="grey70", main=NULL) 
plot(Q1, add=TRUE) 
Q_d1 = intensity(Q1)
plot(intensity(Q1, image=TRUE), main=NULL, las=1)
plot(starbucks, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)
#CSR test
quadrat.test(starbucks, nx= 6, ny=3)
quadrat.test(starbucks, nx= 5, ny=4)
quadrat.test(starbucks, nx= 3, ny=7)
quadrat.test(starbucks, nx= 10, ny=3)
#K, L, and G function
K_star = Kest(starbucks)
L_star = Lest(starbucks)
G_star = Gest(starbucks)
plot(K_star, main="K Function", legendargs=list(cex=0.8, xpd=TRUE, inset=c(0, 0) ))
plot(L_star, main="L Function", legendargs=list(cex=0.65, xpd=TRUE, inset=c(0, 0) ))
plot(G_star, main="G Function", legendargs=list(cex=0.6, xpd=TRUE, inset=c(0, 0) ))


#Making point planar from data frame
#We create data from google maps pinpoint
SMG_TouristSpot = read.csv("D:/My Drive/Kuliah 2/Monograf Spatial/SpatialMonograph/SMG_TouristSpot.csv")
SMG_TouristSpot = SMG_TouristSpot[,-3]
SMGtour = st_as_sf(SMG_TouristSpot, wkt = "WKT", crs = 4326)#Change into sf object
plot(SMGtour, cols=rgb(0,0,0,.2), pch=20, main = "Semarang Tourism Spot")
SMGtour
#Create simplest window that is a rectangle to enclose the tourism spots
xco = c(110.3060469, 110.3060469, 110.4862531, 110.4862531, 110.3060469)
yco = c(-6.945105199999999, -7.0758766, -7.0758766, -6.945105199999999, -6.945105199999999)
rect1 = Polygon(cbind(xco, yco))
rect1 = Polygons(list(rect1), ID = "A")
rectsmg = SpatialPolygons(list(rect1))
#Apply this window into our data is like projecting data into a surface area
SMGtourSP = as(SMGtour, "Spatial")
plot(SMGtourSP)

#There are constraints when we want to do projection since it has been such a complicated problem in the field of
#geographical mapping
rectsmg@proj4string
SMGtourSP@proj4string #first we make sure the projection "rule" for both point data and the polygon is the same
library(raster)
wgs84 = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs(rectsmg) = wgs84
crs(SMGtourSP) = wgs84
plot(rectsmg)
points(SMGtourSP)
#Lets project into semarang map polygon
library(ggplot2)
library(rgdal)
SMG_map = readOGR(dsn = "Kota_Semarang-polygon.shp", stringsAsFactors = F)
SMG_map@proj4string
crs(SMG_map) = wgs84
SMG_map_proj = ggplot() + geom_polygon(data = SMG_map, aes(x = long, y = lat, group = group), 
                                       colour = "black", fill = NA) + geom_sf(data = SMGtour)
SMG_map_proj + theme_void()
y = as(SMG_map, "SpatialPolygons")
p = slot(y, "polygons")
v = lapply(p, function(z) { SpatialPolygons(list(z)) })
SMG_map_win = lapply(v, as.owin)
library(maptools)
#Convert to ppp obejct
ppp_SMG_tour = ppp(SMGtourSP@coords[,1], SMGtourSP@coords[,2], SMG_map_win[[1]])
plot(ppp_SMG_tour)