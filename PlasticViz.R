library(dplyr)
library(tidyverse)
library(lubridate)
library(forcats)
library(scales)
library(leaflet)
library(ggmap)
library(data.table)
library(sp)
library(rgdal)
library(KernSmooth)
library(mapview)
library(raster)
library(magrittr)
library(leafsync)
library(htmlwidgets)
library(htmltools)

## Marine Debris - Julie's Dataset
md <- MarineDebris_data_fourdecimallat_long_day_total_04_19_21_N_attempts_MarineDebris_data_fourdecimallat_long_day_total_04_19_21_N_attempts
#md <- md %>% filter(material == "PLASTIC") 
#md2014 <- md %>% filter(year == "2014")
md <- md %>% mutate(stdplastic = total_plastic/N.cleanupAttempts)
head(md)
#Creating Date & Index column 
#md$date <- as.Date(with(md, paste(year, month, day, sep='/')))
md$index <- 1:nrow(md)

mdFixed <- Summary_MarineDebris_data_four_dec_lat_long_year_total_04_24_21_Summary_MarineDebris_data_four_dec_lat_long_year_total_04_24_21
mdFixed$index <- 1:nrow(mdFixed)

mdFixed_info <- mdFixed %>%
  group_by(index) %>%
  summarise(lat=as.numeric(latitude[1]),
            long=as.numeric(longitude[1]),
            size=as.numeric(total_plastic/N.cleanupAttempts),
            name=paste0(index[1],'<br/>',
                        'Sample Date: ', year, '<br/>',
                        'Plastic Total Count: ', total_plastic, '<br/>',
                        'Plastic Standardized Count: ', round(total_plastic/N.cleanupAttempts,2)))
#Dropping NAs
#md <- md %>% drop_na(NewPlasticTotal)

# Visualization
colnames(md)

#Older Data - 4/20
md_info <- md %>%
  group_by(index) %>%
  summarise(lat=as.numeric(latitude[1]),
            long=as.numeric(longitude[1]),
            size=as.numeric(stdplastic),
            name=paste0(list_name[1],'<br/>',
                        'Sample Date: ', DATE, '<br/>',
                        'Plastic Total Count: ', total_plastic, '<br/>',
                        'Plastic Standardized Count: ', round(stdplastic,2)))
                        #'Percent Plastic: ', plasticpercent, '<br/>',
                        #'Stratum: ', stratum))

# 4/22 Data
md2 <- Summary_MarineDebris_data_four_dec_lat_long_year_total_04_24_21_Summary_MarineDebris_data_four_dec_lat_long_year_total_04_24_21

md2$index <- 1:nrow(md2)

md_info2 <- md2 %>%
  group_by(index) %>%
  summarise(lat=as.numeric(latitude[1]),
            long=as.numeric(longitude[1]),
            size=as.numeric(total_plastic/N.cleanupAttempts),
            name=paste0(index[1],'<br/>',
                        'Sample Date: ', year, '<br/>',
                        'Plastic Total Count: ', total_plastic, '<br/>',
                        'Plastic Standardized Count: ', round(total_plastic/N.cleanupAttempts,2)))


leaflet(md_info) %>%
  addTiles() %>%
  addPolygons(data = spgons,
              color = heat.colors(NLEV, NULL)[LEVS]) %>%
  addCircles(data = md_info,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name) %>%
  addProviderTiles(providers$Esri.)

leaflet(md_info2) %>%
  addTiles() %>%
  addPolygons(data = spgons,
              color = heat.colors(NLEV, NULL)[LEVS]) %>%
  addCircles(data = md_info2,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name) %>%
  addProviderTiles(providers$OpenMapSurfer)

## Total Airbnb Density
abnb <-  AB_US_2020
abnb <-  abnb %>% 
  filter(city == 'San Diego')

x <- cbind(abnb$longitude, abnb$latitude)

kde <- bkde2D(x,
              bandwidth=c(.0045, .0068), gridsize = c(100,100))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

leaflet(spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])

## Year listings
abnb2 <-  airbnb_listings

w <- cbind(abnb2$Longitude, abnb2$Latitude)

kde <- bkde2D(w,
              bandwidth=c(.0045, .0068), gridsize = c(100,100))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

leaflet(spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])  


airbnb2020 <- listings_1_

r <- cbind(airbnb2020$longitude, airbnb2020$latitude)

kde <- bkde2D(r,
              bandwidth=c(.0045, .0068), gridsize = c(1500,1500))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
airbnb2020spgons = SpatialPolygons(pgons)

leaflet(airbnb2020spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])  

length(pgons)
airbnb2020 <- listings_1_

KernelDensityRaster <- raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))

palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values)

KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 30)] <- NA

palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values, na.color = "transparent")

leaflet() %>% addTiles() %>% 
  addRasterImage(KernelDensityRaster, 
                 colors = palRaster, 
                 opacity = .6) %>%
  addLegend(pal = palRaster, 
            values = KernelDensityRaster@data@values, 
            title = "Kernel Density of Points")

## Coastal Cleanup Data - Best So Far - Original 4/20 data
ccd <-  SanDiegoRawData_date_stdEffortcolumns2015to2020_041921_SanDiegoRawData_date_stdEffortcolumns2015to2020_041921
colnames(ccd)
ccd <- ccd %>% filter(Zone == "San Diego County, CA, USA") %>% 
  mutate(plasticpercent = NewPlasticTotal/Total.Items.Collected,
         plasticweight = plasticpercent*Kilograms,
         plastic_area_weight = Kilometers)

ccd2 <- Summary_of_trash_from_2015_to_2020_coastal_cleanup_Summary_of_trash_from_2015_to_2020_coastal_cleanup

ccdFixed <- Fixed_SanDiegoRawData_date_stdEffortcolumns2015to2020_04_22_21_Fixed_SanDiegoRawData_date_stdEffortcolumns2015to2020_04_22_21

ccd2$index <- 1:nrow(ccd2)

ccd_info2 <- ccd2 %>%
  group_by(index) %>%
  summarise(lat=as.numeric(Latitude[1]),
            long=as.numeric(Longitude[1]),
            size=as.numeric(stdzdTotalPlastic),
            name=paste0(Group.Name[1],'<br/>',
                        'Sample Date: ', year, '<br/>',
                        'Plastic Collected: ', NewPlasticTotal, '<br/>',
                        'Percent Plastic: ', round(plasticpercent,2), '<br/>',
                        'Number of People: ', People))

fixed_info <- ccdFixed %>%
  group_by(Cleanup.ID) %>%
  summarise(lat=as.numeric(Latitude[1]),
            long=as.numeric(Longitude[1]),
            size=as.numeric(stdzdTotalPlastic),
            name=paste0(Group.Name[1],'<br/>',
                        'Sample Date: ', DATE, '<br/>',
                        'Standardized Plastic Collected: ', stdzdTotalPlastic, '<br/>',
                        'Total Plastic Collected: ', NewPlasticTotal, '<br/>',
                        'Number of People: ', People))
## Visualization
ccd_info <- ccd %>%
  group_by(Cleanup.ID) %>%
  summarise(lat=as.numeric(Latitude[1]),
            long=as.numeric(Longitude[1]),
            size=as.numeric(plastic_area_weight),
            name=paste0(Group.Name[1],'<br/>',
                        'Sample Date: ', DATE, '<br/>',
                        'Plastic Collected: ', NewPlasticTotal, '<br/>',
                        'Percent Plastic: ', round(plasticpercent,2), '<br/>',
                        'Number of People: ', People))


## Total Airbnb density CCD
leaflet(ccd_info) %>%
  addTiles() %>%
  addPolygons(data = spgons,
              color = heat.colors(NLEV, NULL)[LEVS]) %>%
  addCircles(data = ccd_info,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~size,
             popup = ~name)

leaflet(fixed_info) %>%
  addTiles() %>%
  addPolygons(data = spgons,
              color = heat.colors(NLEV, NULL)[LEVS]) %>%
  addCircles(data = fixed_info,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~size,
             popup = ~name)
## Total Event Density
event <- special_events_list_datasd_v1

event <- event[!is.na(event$lng), ]
event <- event[!is.na(event$lat), ]
y <- cbind(event$lng, event$lat)

kde <- bkde2D(y,
              bandwidth=c(.0045, .0068), gridsize = c(500, 500))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons_event = SpatialPolygons(pgons)

leaflet(spgons_event) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])

# Total MD Event Density

TMDED <- leaflet(md_info) %>%
  addTiles() %>%
  addPolygons(data = spgons_event,
              color = heat.colors(NLEV, NULL)[LEVS]) %>%
  addCircles(data = md_info,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name) %>%
  addProviderTiles(providers$Esri.OpenStreetMap.Mapnik)

TMDED <- mapview(TMDED)
mapshot(TMDED, file = "~/TotalMDEventD.png")

# Total CCD Event Density

leaflet(ccd_info) %>%
  addTiles() %>%
  addPolygons(data = spgons_event,
              color = heat.colors(NLEV, NULL)[LEVS]) %>%
  addCircles(data = ccd_info,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name) %>%
  addProviderTiles(providers$Stadia.Outdoors)

#Filtering By Date - Year

md14 <- mdFixed %>% filter(year == '2014')
md15 <- mdFixed %>% filter(year == '2015')
md16 <- mdFixed %>% filter(year == '2016')
md17 <- mdFixed %>% filter(year == '2017')
md18 <- mdFixed %>% filter(year == '2018')
md19 <- mdFixed %>% filter(year == '2019')
md20 <- mdFixed %>% filter(year == '2020')
mdsep <- md %>% filter(month == '9')

ccd15 <- ccdFixed %>% filter(year == '2015')
ccd16 <- ccdFixed %>% filter(year == '2016')
ccd17 <- ccdFixed %>% filter(year == '2017')
ccd18 <- ccdFixed %>% filter(year == '2018')
ccd19 <- ccdFixed %>% filter(year == '2019')
ccd20 <- ccdFixed %>% filter(year == '2020')

md_info15 <- md15 %>%
  group_by(index) %>%
  summarise(lat=as.numeric(latitude[1]),
            long=as.numeric(longitude[1]),
            size=as.numeric(total_plastic/N.cleanupAttempts),
            name=paste0(index[1],'<br/>',
                        'Sample Date: ', year, '<br/>',
                        'Plastic Total Count: ', total_plastic, '<br/>',
                        'Plastic Standardized Count: ', round(total_plastic/N.cleanupAttempts,2)))

md_info16 <- md16 %>%
  group_by(index) %>%
  summarise(lat=as.numeric(latitude[1]),
            long=as.numeric(longitude[1]),
            size=as.numeric(total_plastic/N.cleanupAttempts),
            name=paste0(index[1],'<br/>',
                        'Sample Date: ', year, '<br/>',
                        'Plastic Total Count: ', total_plastic, '<br/>',
                        'Plastic Standardized Count: ', round(total_plastic/N.cleanupAttempts,2)))
md_info17 <- md17 %>%
  group_by(index) %>%
  summarise(lat=as.numeric(latitude[1]),
            long=as.numeric(longitude[1]),
            size=as.numeric(total_plastic/N.cleanupAttempts),
            name=paste0(index[1],'<br/>',
                        'Sample Date: ', year, '<br/>',
                        'Plastic Total Count: ', total_plastic, '<br/>',
                        'Plastic Standardized Count: ', round(total_plastic/N.cleanupAttempts,2)))

md_info18 <- md18 %>%
  group_by(index) %>%
  summarise(lat=as.numeric(latitude[1]),
            long=as.numeric(longitude[1]),
            size=as.numeric(total_plastic/N.cleanupAttempts),
            name=paste0(index[1],'<br/>',
                        'Sample Date: ', year, '<br/>',
                        'Plastic Total Count: ', total_plastic, '<br/>',
                        'Plastic Standardized Count: ', round(total_plastic/N.cleanupAttempts,2)))

md_info19 <- md19 %>%
  group_by(index) %>%
  summarise(lat=as.numeric(latitude[1]),
            long=as.numeric(longitude[1]),
            size=as.numeric(total_plastic/N.cleanupAttempts),
            name=paste0(index[1],'<br/>',
                        'Sample Date: ', year, '<br/>',
                        'Plastic Total Count: ', total_plastic, '<br/>',
                        'Plastic Standardized Count: ', round(total_plastic/N.cleanupAttempts,2)))

md_info20 <- md20 %>%
  group_by(index) %>%
  summarise(lat=as.numeric(latitude[1]),
            long=as.numeric(longitude[1]),
            size=as.numeric(total_plastic/N.cleanupAttempts),
            name=paste0(index[1],'<br/>',
                        'Sample Date: ', year, '<br/>',
                        'Plastic Total Count: ', total_plastic, '<br/>',
                        'Plastic Standardized Count: ', round(total_plastic/N.cleanupAttempts,2)))


ccd_info15 <- ccd15 %>%
  group_by(Cleanup.ID) %>%
  summarise(lat=as.numeric(Latitude[1]),
            long=as.numeric(Longitude[1]),
            size=as.numeric(stdzdTotalPlastic),
            name=paste0(Group.Name[1],'<br/>',
                        'Sample Date: ', DATE, '<br/>',
                        'Standardized Plastic Collected: ', stdzdTotalPlastic, '<br/>',
                        'Plastic Collected: ', NewPlasticTotal, '<br/>',
                        'Number of People: ', People))

ccd_info16 <- ccd16 %>%
  group_by(Cleanup.ID) %>%
  summarise(lat=as.numeric(Latitude[1]),
            long=as.numeric(Longitude[1]),
            size=as.numeric(stdzdTotalPlastic),
            name=paste0(Group.Name[1],'<br/>',
                        'Sample Date: ', DATE, '<br/>',
                        'Standardized Plastic Collected: ', stdzdTotalPlastic, '<br/>',
                        'Plastic Collected: ', NewPlasticTotal, '<br/>',
                        'Number of People: ', People))

ccd_info17 <- ccd17 %>%
  group_by(Cleanup.ID) %>%
  summarise(lat=as.numeric(Latitude[1]),
            long=as.numeric(Longitude[1]),
            size=as.numeric(stdzdTotalPlastic),
            name=paste0(Group.Name[1],'<br/>',
                        'Sample Date: ', DATE, '<br/>',
                        'Standardized Plastic Collected: ', stdzdTotalPlastic, '<br/>',
                        'Plastic Collected: ', NewPlasticTotal, '<br/>',
                        'Number of People: ', People))

ccd_info18 <- ccd18 %>%
  group_by(Cleanup.ID) %>%
  summarise(lat=as.numeric(Latitude[1]),
            long=as.numeric(Longitude[1]),
            size=as.numeric(stdzdTotalPlastic),
            name=paste0(Group.Name[1],'<br/>',
                        'Sample Date: ', DATE, '<br/>',
                        'Standardized Plastic Collected: ', stdzdTotalPlastic, '<br/>',
                        'Plastic Collected: ', NewPlasticTotal, '<br/>',
                        'Number of People: ', People))

ccd_info19 <- ccd19 %>%
  group_by(Cleanup.ID) %>%
  summarise(lat=as.numeric(Latitude[1]),
            long=as.numeric(Longitude[1]),
            size=as.numeric(stdzdTotalPlastic),
            name=paste0(Group.Name[1],'<br/>',
                        'Sample Date: ', DATE, '<br/>',
                        'Standardized Plastic Collected: ', stdzdTotalPlastic, '<br/>',
                        'Plastic Collected: ', NewPlasticTotal, '<br/>',
                        'Number of People: ', People))

ccd_info20 <- ccd20 %>%
  group_by(Cleanup.ID) %>%
  summarise(lat=as.numeric(Latitude[1]),
            long=as.numeric(Longitude[1]),
            size=as.numeric(stdzdTotalPlastic),
            name=paste0(Group.Name[1],'<br/>',
                        'Sample Date: ', DATE, '<br/>',
                        'Standardized Plastic Collected: ', stdzdTotalPlastic, '<br/>',
                        'Plastic Collected: ', NewPlasticTotal, '<br/>',
                        'Number of People: ', People))

leaflet(ccd_info15) %>%
  addTiles() %>%
  addCircles(data = ccd_info15,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name) %>%
  addCircles(data = ccd_info16,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name) %>%
  addCircles(data = ccd_info17,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name)

leaflet(md_info15) %>%
  addTiles() %>%
  addCircles(data = md_info15,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name) %>%
  addCircles(data = md_info16,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name) %>%
  addCircles(data = md_info17,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name)

leaflet(ccd_info16) %>%
  addTiles() %>%
  addCircles(data = ccd_info16,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name)

leaflet(ccd_info17) %>%
  addTiles() %>%
  addCircles(data = ccd_info17,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name)

leaflet(ccd_info18) %>%
  addTiles() %>%
  addCircles(data = ccd_info18,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name) %>%
  addCircles(data = ccd_info19,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name) %>%
  addCircles(data = ccd_info20,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name) %>%

leaflet(ccd_info19) %>%
  addTiles() %>%
  addCircles(data = ccd_info19,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name)

leaflet(ccd_info20) %>%
  addTiles() %>%
  addCircles(data = ccd_info20,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name)

leaflet(md_info18) %>%
  addTiles() %>%
  addCircles(data = md_info18,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name) %>%
  addCircles(data = md_info19,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name) %>%
  addCircles(data = md_info20,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name)

#Filter Airbnb Date

abnb2$date <- as.POSIXct(abnb2$`Host Since`, format = "%m/%d/%y")
abnb2$date
abnb2$year <- format(abnb2$date, format = "%Y")
abnb2$year
#15
airbnb15 <- abnb2 %>% 
  filter(year <= '2015')
  
a15 <- cbind(airbnb15$Longitude, airbnb15$Latitude)

kde <- bkde2D(a15,
              bandwidth=c(.0045, .0068), gridsize = c(100,100))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
a15spgons = SpatialPolygons(pgons)

leaflet(a15spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS]) 

airbnb_info <- airbnb2020 %>%
  group_by(id) %>%
  summarise(lat=as.numeric(latitude[1]),
            long=as.numeric(longitude[1]),
            size=as.numeric(availability_365),
            name=paste0(name[1]))

leaflet(airbnb_info) %>%
  addTiles() %>%
  addCircles(color = 'red',
             lng = ~long,
             lat = ~lat,
             weight = 1,
             radius = ~sqrt(size)*2,
             popup = ~name)

#16
airbnb16 <- abnb2 %>% 
  filter(year <= '2016')

a16 <- cbind(airbnb16$Longitude, airbnb16$Latitude)

kde <- bkde2D(a16,
              bandwidth=c(.0045, .0068), gridsize = c(1500,1500))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
a16spgons = SpatialPolygons(pgons)

leaflet(a16spgons) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS]) 

KernelDensityRaster <- raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))

palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values)

KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 30)] <- NA

palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values, na.color = "transparent")

leaflet() %>% addTiles() %>% 
  addRasterImage(KernelDensityRaster, 
                 colors = palRaster, 
                 opacity = .6) %>%
  addLegend(pal = palRaster, 
            values = KernelDensityRaster@data@values, 
            title = "Kernel Density of Points")


## Airbnb Maps CCD 
airbnb_map15 <- leaflet(ccd_info15) %>%
  addTiles() %>%
  addPolygons(data = a15spgons,
              color = heat.colors(NLEV, NULL)[LEVS]) %>%
  addCircles(data = ccd_info15,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name)

airbnb_map16 <- leaflet(ccd_info16) %>%
  addTiles() %>%
  addPolygons(data = a16spgons,
              color = heat.colors(NLEV, NULL)[LEVS]) %>%
  addCircles(data = ccd_info16,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name)


airbnb_map20 <- leaflet(ccd_info20) %>%
  addTiles() %>%
  addCircles(data = ccd_info20,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name,
             color = 'black') %>% 
  addCircles(data = md_info20,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name,
             color = 'blue') %>%
  addRasterImage(KernelDensityRaster, 
                 colors = palRaster, 
                 opacity = .5) %>%
  addLegend(values = 1, position = "bottomleft", labels = "Marine Debris", colors = "blue") %>%
  addLegend(values = 2, position = "bottomleft", labels = "Coastal CleanUp", colors = "black") %>%
  addLegend(position = "bottomleft",
            pal = palRaster, 
            values = KernelDensityRaster@data@values, 
            title = " Airbnb Kernel Density of Points")

## Airbnb Maps MD 
airbnb_map15MD <- leaflet(md_info15) %>%
  addTiles() %>%
  addPolygons(data = a15spgons,
              color = heat.colors(NLEV, NULL)[LEVS]) %>%
  addCircles(data = md_info15,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name)

bins <- c(0,1,2,3,4,5)

pal <- colorNumeric(palette = "RdOrYl", domain = kde)

airbnb_map16MD <- leaflet(md_info16) %>%
  addTiles() %>%
  addCircles(data = md_info16,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name,
             color = 'blue') %>%
  addCircles(data = ccd_info16,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name,
             color = 'black') %>%
  addRasterImage(KernelDensityRaster, 
                 colors = palRaster, 
                 opacity = .5) %>%
  addLegend(values = 1, position = "bottomleft", labels = "Marine Debris", colors = "blue") %>%
  addLegend(values = 2, position = "bottomleft", labels = "Coastal CleanUp", colors = "black") %>%
  addLegend(position = "bottomleft",
            pal = palRaster, 
            values = KernelDensityRaster@data@values, 
            title = " Airbnb Kernel Density of Points")


map <- map()

palFun <- colorNumeric("heat.colors", domain = airbnb2020spgons)

dataType(airbnb2020$count)
pale
airbnb2020$count <- rep(1, nrow(airbnb2020))


airbnb_map20MD <- leaflet(md_info20) %>%
  addTiles() %>%
  addPolygons(data = airbnb2020spgons,
              color = heat.colors(NLEV, NULL)[LEVS]) %>%
  addCircles(data = md_info20,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name) %>%
  addLegend(position = "bottomleft",
            pal = palFun,
            labels = "Kernel Density",
            values = count(airbnb2020spgons),
            title = "Marine Debris Standardized Plastic Collected over Airbnb Listing Points 2020")








# Filter Event Date
event19 <- event %>% 
  filter(date_event_start >= '2019-04-21 09:00:00' & date_event_start <= '2019-12-31 24:00:00')

event20 <- event %>% 
  filter(date_event_start >= '2020-01-1 07:00:00' & date_event_start <= '2020-12-31 24:00:00')

event21 <- event %>% 
  filter(date_event_start >= '2021-01-1 07:00:00' & date_event_start <= '2021-12-31 24:00:00')

a <- cbind(event19$lng, event19$lat)

kde <- bkde2D(a,
              bandwidth=c(.0045, .0068), gridsize = c(500, 500))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons_event19 = SpatialPolygons(pgons)

leaflet(spgons_event19) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])

event_info <- event %>%
  group_by(event_id) %>%
  summarise(lat=as.numeric(lat[1]),
            long=as.numeric(lng[1]),
            size=as.numeric(event_id),
            name=paste0(event_title[1]))

leaflet(event_info) %>%
  addTiles() %>%
  addCircles(color = 'purple',
             lng = ~long,
             lat = ~lat,
             weight = 3,
             radius = ~(size/size),
             popup = ~name)

# Total CCD Event Density 2019

eventd_map19 <- leaflet(ccd_info19) %>%
  addTiles() %>%
  addPolygons(data = spgons_event19,
              color = heat.colors(NLEV, NULL)[LEVS]) %>%
  addCircles(data = ccd_info19,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name) %>%
  addControl(title, position = 'bottomleft', className = 'Map title')

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Event Density with Coastal Cleanup Data for 2019")
) 

MDeventd_map19 <- leaflet(md_info19) %>%
  addTiles() %>%
  addPolygons(data = spgons_event19,
              color = heat.colors(NLEV, NULL)[LEVS]) %>%
  addCircles(data = md_info19,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name)
# 2020

b <- cbind(event20$lng, event20$lat)

kde <- bkde2D(b,
              bandwidth=c(.0045, .0068), gridsize = c(500, 500))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)

## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons_event20 = SpatialPolygons(pgons)

leaflet(spgons_event20) %>% addTiles() %>% 
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS])

leaflet(ccd_info20) %>%
  addTiles() %>%
  addPolygons(data = spgons_event20,
              color = heat.colors(NLEV, NULL)[LEVS]) %>%
  addCircles(data = ccd_info20,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name)

leaflet(md_info20) %>%
  addTiles() %>%
  addPolygons(data = spgons_event20,
              color = heat.colors(NLEV, NULL)[LEVS]) %>%
  addCircles(data = md_info20,
             lng = ~long,
             lat = ~lat,
             weight = 5,
             radius = ~sqrt(size)*2,
             popup = ~name)

#2021

c <- cbind(event19$lng, event19$lat)

kde <- bkde2D(c,
              bandwidth=c(.0045, .0068), gridsize = c(2000, 2000))

KernelDensityRaster <- raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))

palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values)

KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 30)] <- NA

palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values, na.color = "transparent")

leaflet() %>% addTiles() %>% 
  addRasterImage(KernelDensityRaster, 
                 colors = palRaster, 
                 opacity = .6) %>%
  addLegend(pal = palRaster, 
            values = KernelDensityRaster@data@values, 
            title = "Kernel Density of Points")


c <- cbind(event20$lng, event20$lat)

kde <- bkde2D(c,
              bandwidth=c(.0045, .0068), gridsize = c(500, 500))

KernelDensityRaster <- raster(list(x=kde$x1 ,y=kde$x2 ,z = kde$fhat))

palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values)

KernelDensityRaster@data@values[which(KernelDensityRaster@data@values < 30)] <- NA

palRaster <- colorNumeric("Spectral", domain = KernelDensityRaster@data@values, na.color = "transparent")

leaflet() %>% addTiles() %>% 
  addRasterImage(KernelDensityRaster, 
                 colors = palRaster, 
                 opacity = .6) %>%
  addLegend(pal = palRaster, 
            values = KernelDensityRaster@data@values, 
            title = "Kernel Density of Points")

