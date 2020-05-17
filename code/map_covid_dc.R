# ------------------------------------------------------- # 
# Map of SARS-CoV-2 Rate in the District of Columbia
# 
# Created by Ian Buller (Github: @idblr)
# Created on 05/15/2020
#
# Recently modified by: Ian Buller (Github: @idblr)
# Recently modified on: 05/16/2020
#
# Notes:
# A) 05/15/2020 (@idblr) - Inspired by Prince of Petworth (@PopVille) post https://www.popville.com/2020/05/dc-neighborhood-covid-coronavirus-map-population/#more-234053
# B) 05/15/2020 (@idblr) - Major credit to Molly (@zmotoly) for data
# C) 05/16/2020 (@idblr) - Created static ggplot2 of cumulative cases per 1,000 and a leaflet
# ------------------------------------------------------- #

# -------- #
# PACKAGES #
# -------- #

loadedPackages <- c("broom", "geojsonio", "ggplot2", "googlesheets4", "htmlwidgets", "leaflet", "sp", "stringr")
invisible(lapply(loadedPackages, require, character.only = T))

# -------- #
# SETTINGS #
# -------- #

gs4_deauth() # no Google authorization necessary because we are not reading a public repo

# ---------------- #
# DATA IMPORTATION #
# ---------------- #

# District of Columbia Health Planning Neighborhoods
## Data are available from https://opendata.dc.gov/
gis_path <- "https://opendata.arcgis.com/datasets/de63a68eb7674548ae0ac01867123f7e_13.geojson"
dc <- geojsonio::geojson_read(gis_path,  what = "sp")
#plot(dc) # check importation, uncomment for plot

# District of Columbia SAR-CoV-2 Data prepared by @zmotoly
covid_path <- "https://docs.google.com/spreadsheets/d/1u-FlJe2B1rYV0obEosHBks9utkU30-C2TSkHka6AVS8/edit#gid=1923705378"
covid <- googlesheets4::read_sheet(ss = covid_path,
                                   sheet = 2, # second sheet
                                   skip = 1) # skip 1st row of annotation
names(covid) <- sub("\n", "", names(covid)) # remove extra line in column names
names(covid) <- gsub(" ", "_", names(covid)) # replace spaces with underscore

# ---------------------- #
# GEOGRAPHIC PREPARATION #
# ---------------------- #

# Merge
dc_covid <- sp::merge(dc, covid, by.x = "CODE", by.y = "NB_Code")

# UTM Coordinate Reference System for meter distances (to include scale)
dc_covid_proj <- sp::spTransform(dc_covid, CRS("+init=EPSG:32618")) # UTM zone 18N (Washington, DC)

# --------------- #
# STATIC PLOTTING #
# --------------- #

# Uses ggplot2 package
## helpful material: https://cengel.github.io/rspatial/4_Mapping.nb.html
## Data preparation, ggplot2 requires a data.frame
dc_covid_df <- broom::tidy(dc_covid_proj) # convert to tidy data frame
dc_covid_proj$polyID <- sapply(slot(dc_covid_proj, "polygons"), function(x) slot(x, "ID")) # preserve polygon id
CoV_DC_df <- merge(dc_covid_df, dc_covid_proj, by.x = "id", by.y="polyID") # merge data

## Plot of cumulative cases per 1,000
ggplot2::ggplot() +                                             # initialize ggplot object
  ggplot2::geom_polygon(                                        # make a polygon
    data = CoV_DC_df,                                           # data frame
    ggplot2::aes(x = long, y = lat, group = group,              # coordinates, and group them by polygons
                 fill = ggplot2::cut_interval(Cases_per_1000_May_15, 10)),       # variable to use for filling
    colour = "white") +                                         # color of polygon borders
  ggplot2::scale_fill_brewer("Cumulative cases per 1,000",      # title of colorkey 
                             palette = "Purples",               # fill with brewer colors 
                             na.value = "grey67",               # color for NA (The National Mall)
                             direction = 1,                     # reverse colors in colorkey
                             guide = ggplot2::guide_legend(reverse = T)) +  # reverse order of colokey
  ggplot2::ggtitle("Cumulative SARS-CoV-2 cases per 1,000 (March 7, 2020 - May 15, 2020)") + # add title
  ggplot2::theme(line = ggplot2::element_blank(),               # remove axis lines
                 axis.text = ggplot2::element_blank(),          # remove tickmarks
                 axis.title = ggplot2::element_blank(),         # remove axis labels
                 panel.background = ggplot2::element_blank(),   # remove background gridlines
                 text = ggplot2::element_text(size = 10)) +     # set font size
  ggplot2::coord_equal()                                        # both axes the same scale

# -------------------- #
# INTERACTIVE PLOTTING #
# -------------------- #

# Uses leaflet package
# Work with unprojected spatialpolygonsdataframe
## Project to WGS84 EPSG:4326
CoV_DC_WGS84 <- sp::spTransform(dc_covid_proj, CRS("+init=epsg:4326"))

## Create Popups
dc_health <- stringr::str_to_title(CoV_DC_WGS84$Neighborhood_Name)
dc_health[c(11,25,41,49)] <- c("DC Medical Center", "GWU", "National Mall", "SW/Waterfront" )
CoV_DC_WGS84$popup1 <- paste(dc_health, ": ",
                             format(round(CoV_DC_WGS84$Total_cases_May_15, digits = 0), big.mark = ",", trim = T),
                             " cumulative cases", sep = "")
CoV_DC_WGS84$popup2 <- paste(dc_health, ": ",
                             format(round(CoV_DC_WGS84$Cases_per_1000_May_15, digits = 0), big.mark = ",", trim = T),
                             " cumulative cases per 1,000", sep = "")

## Set Palettes
pal_cum <- leaflet::colorNumeric(palette = "Purples",
                                 domain = CoV_DC_WGS84$Total_cases_May_15,
                                 na.color = "#555555")
pal_rate <- leaflet::colorNumeric(palette = "Purples",
                                  domain = CoV_DC_WGS84$Cases_per_1000_May_15,
                                  na.color = "#555555")

### Create leaflet plot
dc_m1 <-leaflet::leaflet(CoV_DC_WGS84, width = "100%") %>%                        # initial data
  leaflet::setView(lng = -77, lat = 38.9, zoom = 11) %>%                  # starting coordinates
  leaflet::addTiles() %>% # basemap
  leaflet::addPolygons(data = CoV_DC_WGS84, color = "black", weight = 1, smoothFactor = 0.5, opacity = 1,
                       fillOpacity = 0.67, fillColor = ~pal_cum(Total_cases_May_15), popup = ~popup1,
                       highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                       group = "Cumulative Cases") %>%
  leaflet::addPolygons(data = CoV_DC_WGS84, color = "black", weight = 1, smoothFactor = 0.5, opacity = 1,
                       fillOpacity = 0.67, fillColor = ~pal_rate(Cases_per_1000_May_15), popup = ~popup2,
                       highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                       group = "Cumulative Rate") %>%
  leaflet::addLayersControl(overlayGroups = c("Cumulative Cases", "Cumulative Rate"), # layer selection
                            options = layersControlOptions(collapsed = FALSE)) %>%     
  addLegend("topright", pal = pal_cum, values = ~Total_cases_May_15,                  # legend for cases
            title = "Cumulative Cases", opacity = 1, group = "Cumulative Cases") %>%
  addLegend("topright", pal = pal_rate, values = ~Cases_per_1000_May_15,              # legend for rate
            title = "Cumulative Rate per 1,000", opacity = 1, group = "Cumulative Rate") %>%
  leaflet::hideGroup(c("Cumulative Cases", "Cumulative Rate")) %>% # no data shown (default)
  leaflet::addMiniMap(position = "bottomleft") # add mini map
dc_m1 # display leaflet plot

###############
# EXPORTATION #
###############

# export leaflet plot
htmlwidgets::saveWidget(dc_m1, file = "COVID_DC_Leaflet.html")
# -------------------------- END OF CODE -------------------------- #  