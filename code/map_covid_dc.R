# ------------------------------------------------------- # 
# Map of SARS-CoV-2 Rate in the District of Columbia
# 
# Created by Ian Buller (Github: @idblr)
# Created on 05/15/2020
#
# Recently modified by: 
# Recently modified on: 
#
# Notes:
# A) 05/15/2020 (@idblr) - Inspired by Prince of Petworth (@PopVille) post https://www.popville.com/2020/05/dc-neighborhood-covid-coronavirus-map-population/#more-234053
# B) 05/15/2020 (@idblr) - Major credit to Molly (@zmotoly) for data
# ------------------------------------------------------- #

# -------- #
# PACKAGES #
# -------- #

loadedPackages <- c("ggmap", "googlesheets4", "grid", "htmlwidgets", "leaflet", "raster", "RColorBrewer", "rgdal", "sp")
invisible(lapply(loadedPackages, require, character.only = T))

# ---------------- #
# DATA IMPORTATION #
# ---------------- #

# District of Columbia Health Planning Neighborhoods
## Data are available from https://opendata.dc.gov/
gis_path <- "https://opendata.arcgis.com/datasets/de63a68eb7674548ae0ac01867123f7e_13.geojson"
dc <- geojsonio::geojson_read(gis_path,  what = "sp")
#plot(dc) # check importation, uncomment for plot

# https://docs.google.com/spreadsheets/d/1u-FlJe2B1rYV0obEosHBks9utkU30-C2TSkHka6AVS8/edit#gid=1923705378

# ---------------------- #
# GEOGRAPHIC PREPARATION #
# ---------------------- #

# Only Ward 5
ward5 <- dc_ward[dc_ward$WARD_ID == 5,]

# Restrict zipcodes to Ward 5
ward5_zip <- raster::crop(dc_zip, ward5) 

ward5_block <- raster::crop(dc_block, ward5)

# Create Random Addresses
dc_address <- sp::spsample(ward5, n = 100, "random")

plot(ward5)
plot(ward5_zip, add = T)
plot(dc_address, add = T)
plot(ward5_block, add = T)


dc_address@coords

# Extract zipcode at each point
dc_address$zipcode <- sp::over(dc_address,dc_zip)$ZIP_CODE 

# Count addresses within each zipcode
zip_count <- as.data.frame(table(dc_address$zipcode)) 

# Merge counts per zipcode to zipcode data frame
ward5_zip@data <- data.frame(ward5_zip@data, zip_count[match(ward5_zip@data[,"ZIPCODE"], zip_count[,"Var1"]),])

# --------------------- #
#### STATIC PLOTTING ####
# --------------------- #

# UTM Coordinate Reference System for meter distances (to include scale)
ward5_zip_proj <- spTransform(ward5_zip, CRS("+init=EPSG:32618")) # UTM zone 18N (Washington, DC)

# Colorkey scaled by count per zipcode
at <- seq(from = 0, to = max(ward5_zip$Freq), by = 3) + 1 # set breaks in colorkey (must be one more than default to fit min and max values)
at_names <- seq(from = 0, to = max(ward5_zip$Freq), by = 3) # set name of breaks in colorkey

# Custom scale
scale <- list("SpatialPolygonsRescale",
              layout.scale.bar(),
              scale = 1000, # 1000 meters
              fill = c("transparent", "black"), # colors of scale
              offset = c(325500, 4307500) # location in plot
)

# The scale argument sets length of bar in map units
text1 <- list("sp.text", c(325500, 4307650), "0 km", cex = 1*f) # lower limit set just above scale
text2 <- list("sp.text", c(326500, 4307650), "1 km", cex = 1*f) # upper limit set just above scale

# Custom compass rose
arrow <- list("SpatialPolygonsRescale", 
              sp::layout.north.arrow(),
              scale = 500, # size of plot 
              offset = c(325900, 4307700) # location in plot (just above and middle of scale)
)

# GREYSCALE
f <- 2 # expansion factor (with increasing size of the plot)
png(file = "figures/ward5_zip_greyscale.png", width = 2000, height = 1600) # turn on .png device
sp::spplot(ward5_zip_proj, # data
           "Freq", # column name
           col.regions = rev(gray.colors(length(at))), # color palette
           at = at + 1, # set breaks one more than default
           par.settings = list(axis.line = list(col =  'transparent')), # remove default box around plot
           colorkey = list(labels = list(at = at + 1, # set breaks one more than default (to include min and max values on colorkey)
                                         labels = at_names, # set name of breaks to match data
                                         cex = 2*f, # size of labels of colorkey
                                         fontface = 1, # set font style (normal)
                                         #fontfamily = 'LM Roman 10', # set font name
                                         legend = "frequency" # label of colorkey
           )
           ),
           sub = list(label = "Copywrite Ian Buller", cex = 1*f), # subtitle for credit
           sp.layout = list(scale, text1, text2, arrow) # additions: scale and north arrow
)
grid::grid.text("Frequency", # for overall label of colorkey
                x = grid::unit(0.95, "npc"), # x position
                y = grid::unit(0.50, "npc"), # y position
                rot = 90, # for vertical colorkey
                gp = grid::gpar(fontsize = 20*f # for custom size of overall label
                                #,fontfamily= 'LM Roman 10' # for custom font of overall label
                )
)
dev.off() # turn off device

# WITH COLOR
png(file = "figures/ward5_zip_wcolor.png", width = 2000, height = 1600)
sp::spplot(ward5_zip_proj,
           "Freq",
           col.regions = rev(terrain.colors(length(at))), # also: heat.colors (red) or create custom palette with RColorBrewer
           at = at + 1,
           par.settings = list(axis.line = list(col =  'transparent')),
           colorkey = list(labels = list(at = at + 1,
                                         labels = at_names,
                                         cex = 2*f, 
                                         fontface = 1, # set font style (normal)
                                         #fontfamily = 'LM Roman 10', # set font name
                                         legend = "frequency")
           ),
           sub = list(label = "Copywrite Ian Buller", cex = 1*f),
           sp.layout = list(scale, text1, text2, arrow)
)
grid::grid.text("Frequency", # for overall label of colorkey
                x = grid::unit(0.95, "npc"), # x position
                y = grid::unit(0.50, "npc"), # y position
                rot = 90, # for vertical colorkey
                gp = grid::gpar(fontsize = 20*f # for custom size of overall label
                                #,fontfamily= 'LM Roman 10' # for custom font of overall label
                )
)
dev.off()

## LEAFLET

ward5_zip$popup <- paste(ward5_zip$ZIPCODE, ": ", ward5_zip$Freq, " patient(s)", sep = "")


# Palette
pal_zcta <- colorNumeric(palette = "Blues", domain = ward5_zip$Freq)

# Icon
leafIcons <- icons(
  iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Featured_articles_star_modern.svg/800px-Featured_articles_star_modern.svg.png",
  iconWidth = 15, iconHeight = 15,
  iconAnchorX = 0, iconAnchorY = 0)

# Icon legend
html_legend <- "<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Featured_articles_star_modern.svg/800px-Featured_articles_star_modern.svg.png'style='width:20px;height:20px;'> Patient"

# Create leaflet plot
dc_m1 <- leaflet::leaflet(ward5_zip) %>% # initial data = location of ZCTAs
  leaflet::setView(lng = -77, lat = 38.92, zoom = 12) %>% # starting location (Washington DC)
  leaflet::addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = "OSM (Default)") %>% # default basemap
  leaflet::addProviderTiles(providers$Esri.WorldTopoMap, group = "Terrain") %>% # additional basemap
  leaflet::addProviderTiles(providers$OpenTopoMap, group = "Topographic") %>% # additional basemap
  leaflet::addPolygons(data = ward5_zip, color = "black", weight = 1, smoothFactor = 0.5, opacity = 1,
                       fillOpacity = 0.5, fillColor = ~pal_zcta(Freq), popup = ~popup, # stroke = FALSE
                       highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                       group = "By Zipcode"
  ) %>%
  leaflet::addMarkers(lng = dc_address@coords[,1], lat = dc_address@coords[,2], icon = leafIcons,
                      group = "By Address"
  ) %>%
  leaflet::addLayersControl(baseGroups = c("OSM (Default)", "Terrain", "Topographic"),
                            overlayGroups = c("By Zipcode", "By Address"),
                            options = layersControlOptions(collapsed = FALSE)
  ) %>% # layer selection
  addLegend("topright", pal = pal_zcta, values = ~Freq,
            title = "Frequency",
            opacity = 1,
            group = "By Zipcode") %>%
  addControl(html = html_legend, position = "topright") %>%
  leaflet::hideGroup(c("By Zipcode", "By Address")) %>% # no data shown (default)
  leaflet::addMiniMap() # add mini map
dc_m1 # display leaflet plot

# Export
htmlwidgets::saveWidget(dc_m1, file = "dc_ward5.html", selfcontained = TRUE)

# -------------------------- END OF CODE -------------------------- #  
