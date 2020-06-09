# ------------------------------------------------------- # 
# Animated Map of SARS-CoV-2 Rate in the District of Columbia
# 
# Created by Ian Buller (Github: @idblr)
# Created on 06/07/2020
#
# Recently modified by: @idblr
# Recently modified on: 06/08/2020
#
# Notes:
# A) 06/07/2020 (@idblr) - Animations for 1) cumulative cases, 2) cumulative case rate, 3) cumulative testing, 4) cumulative testing rate, 5) average cases, 6) average case rate 7) test positivity, 8) average test positivity
# B) 06/08/2020 (@idblr) - Data for May 7 - June 7, 2020
# C) 06/08/2020 (@idblr) - Major credit to Molly (@zmotoly) for data
# ------------------------------------------------------- # 

# -------- #
# PACKAGES #
# -------- #

loadedPackages <- c("broom", "dplyr", "geojsonio", "gganimate", "ggplot2", "googlesheets4", "htmlwidgets", "leaflet", "sp", "stringr", "tidyr", "transformr")
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

# Fix column names
names(covid) <- sub("\n", "", names(covid)) # remove extra line in column names
names(covid) <- gsub(" ", "_", names(covid)) # replace spaces with underscore
names(covid) <- gsub("Total_cases", "Cumulative", names(covid)) # Change to one word
names(covid) <- gsub("Total_tests", "Tested_", names(covid)) # Change to one word
names(covid) <- gsub("Cases_per_1000", "Case.Rate", names(covid)) # Change to one word
names(covid) <- gsub("Tests_per_1000", "Test.Rate_", names(covid)) # Change to one word
names(covid) <- gsub("__", "_", names(covid)) # replace double underscores

# ---------------------- #
# GEOGRAPHIC PREPARATION #
# ---------------------- #

# Merge
dc_covid <- sp::merge(dc, covid, by.x = "CODE", by.y = "NB_Code")

# UTM Coordinate Reference System for meter distances (to include scale)
dc_covid_proj <- sp::spTransform(dc_covid, CRS("+init=EPSG:32618")) # UTM zone 18N (Washington, DC)

# --------------- #
# DATA MANAGEMENT #
# --------------- #

# Uses ggplot2 and gganimate packages
## helpful material: https://cengel.github.io/rspatial/4_Mapping.nb.html
## more helpful material: https://stackoverflow.com/questions/60840971/creating-a-choropleth-map-with-us-county-level-data
## Data preparation, ggplot2 requires a data.frame
dc_covid_df <- broom::tidy(dc_covid_proj) # convert to tidy data frame
dc_covid_proj$polyID <- sapply(slot(dc_covid_proj, "polygons"), function(x) slot(x, "ID")) # preserve polygon id
CoV_DC_df <- sp::merge(dc_covid_df, dc_covid_proj, by.x = "id", by.y="polyID") # merge data
names(CoV_DC_df)

# ------------------------------- #
# CALCULATE NECESSARY INFORMATION #
# ------------------------------- #

# 5-day rolling average incident cases and case rate
CoV_DC_loop <- CoV_DC_df %>% dplyr::select(starts_with("cumulative"))
CoV_DC_loop <- CoV_DC_loop[,rev(1:length(CoV_DC_loop))] 
CoV_DC_loop$Cumulative_May_22 <- NA
CoV_DC_loop$Cumulative_May_27 <- NA
CoV_DC_loop <- CoV_DC_loop[,c(1:15, 31, 16:19, 32, 20:30)] 
names(CoV_DC_loop)
i <- NULL

mat_inc <- matrix(ncol = length(CoV_DC_loop)-1, nrow = nrow(CoV_DC_loop))
col_lab <- vector(mode = "character", length = length(CoV_DC_loop)-1)

for (i in 1:length(CoV_DC_loop)-1) {
  mat_inc[ , i] <- ifelse(CoV_DC_loop[ , i+1] - CoV_DC_loop[ , i] < 0, NA, CoV_DC_loop[ , i+1] - CoV_DC_loop[ , i])
  col_lab[i] <- paste(sub("Cumulative", names(CoV_DC_loop[i+1]), replacement = "incident"))
  if(i == length(CoV_DC_loop))
    mat_inc <- data.frame(mat_inc)
    colnames(mat_inc) <- col_lab
}

CoV_DC_df <- cbind(CoV_DC_df, mat_inc)
names(CoV_DC_df)

# 5-day rolling average incident cases and case rate
CoV_DC_loop <-  CoV_DC_df %>% dplyr::select(starts_with("incident"))
names(CoV_DC_loop)
i <- NULL

mat_5d <- matrix(ncol = length(CoV_DC_loop)-4, nrow = nrow(CoV_DC_loop))
mat_5dr <- matrix(ncol = length(CoV_DC_loop)-4, nrow = nrow(CoV_DC_loop))
col_lab <- vector(mode = "character", length = length(CoV_DC_loop)-4)
col_labr <- vector(mode = "character", length = length(CoV_DC_loop)-4)

for (i in 1:(length(CoV_DC_loop)-4)) {
  mat_5d[ , i] <- rowMeans(CoV_DC_loop[ , i:(i+4)], na.rm = T)
  mat_5dr[ , i] <-  mat_5d[ , i] / CoV_DC_df$`Population_(2018_ACS)` * 1000
  col_lab[i] <- paste(sub("incident", names(CoV_DC_loop[(i+4)]), replacement = "average"))
  col_labr[i] <- paste(sub("incident", names(CoV_DC_loop[(i+4)]), replacement = "average.rate"))
  if(i == (length(CoV_DC_loop)-4))
    mat_5d <- data.frame(mat_5d)
    mat_5dr <- data.frame(mat_5dr)
    colnames(mat_5d) <- col_lab
    colnames(mat_5dr) <- col_labr
    out <- cbind(mat_5d, mat_5dr)
}
names(out)
CoV_DC_df <- cbind(CoV_DC_df, out)
names(CoV_DC_df)

# Daily testing
CoV_DC_loop <- CoV_DC_df %>% dplyr::select(starts_with("Tested"), starts_with("incident"))
CoV_DC_loop <- CoV_DC_loop[,c(rev(1:15),28:46)] 
CoV_DC_loop$Tested_May_22 <- NA
CoV_DC_loop$Tested_May_23 <- NA
CoV_DC_loop$Tested_May_24 <- NA
CoV_DC_loop$Tested_May_27 <- NA
CoV_DC_loop <- CoV_DC_loop[,c(1:2,35:37,3:4,38,5:34)] 
i <- NULL
names(CoV_DC_loop)

mat_inc <- matrix(ncol = length(CoV_DC_loop)/2-1, nrow = nrow(CoV_DC_loop))
mat_inc2 <- matrix(ncol = length(CoV_DC_loop)/2-1, nrow = nrow(CoV_DC_loop))
col_lab <- vector(mode = "character", length = length(CoV_DC_loop)/2-1)
col_lab2 <- vector(mode = "character", length = length(CoV_DC_loop)/2-1)

for (i in 1:(length(CoV_DC_loop)/2-1)) {
  mat_inc[ , i] <- ifelse(CoV_DC_loop[ , (i+1)] - CoV_DC_loop[ , i] < 0, NA, CoV_DC_loop[ , (i+1)] - CoV_DC_loop[ , i])
  mat_inc2[ , i] <-  CoV_DC_loop[ , (length(CoV_DC_loop)/2+i)] / mat_inc[ , i]
  col_lab[i] <- paste(sub("Tested", names(CoV_DC_loop[i+1]), replacement = "testing"))
  col_lab2[i] <- paste(sub("Tested", names(CoV_DC_loop[i+1]), replacement = "positivity"))
  if(i == (length(CoV_DC_loop)/2-1))
    mat_inc <- data.frame(mat_inc)
    mat_inc2 <- data.frame(mat_inc2)
    colnames(mat_inc) <- col_lab
    colnames(mat_inc2) <- col_lab2
    out <- cbind(mat_inc, mat_inc2)
}
names(out)
CoV_DC_df <- cbind(CoV_DC_df, out)
names(CoV_DC_df)

# 5-day rolling average testing and test positivity
CoV_DC_loop <-  CoV_DC_df %>% dplyr::select(starts_with("testing_"),starts_with("average_"))
names(CoV_DC_loop)
i <- NULL

mat_5d <- matrix(ncol = 18-4, nrow = nrow(CoV_DC_loop))
mat_5dr <- matrix(ncol = 18-4, nrow = nrow(CoV_DC_loop))
col_lab <- vector(mode = "character", length = 18-4)
col_labr <- vector(mode = "character", length = 18-4)

for (i in 1:(18-4)) {
  mat_5d[ , i] <- rowMeans(CoV_DC_loop[ , i:(i+4)], na.rm = T)
  mat_5dr[ , i] <-  CoV_DC_loop[ , 28+i] / mat_5d[ , i]
  col_lab[i] <- paste(sub("testing", names(CoV_DC_loop[(i+4)]), replacement = "test.avg"))
  col_labr[i] <- paste(sub("testing", names(CoV_DC_loop[(i+4)]), replacement = "posit.avg"))
  if(i == (18-4))
    mat_5d <- data.frame(mat_5d)
    mat_5dr <- data.frame(mat_5dr)
    colnames(mat_5d) <- col_lab
    colnames(mat_5dr) <- col_labr
    out <- cbind(mat_5d, mat_5dr)
}
names(out)
CoV_DC_df <- cbind(CoV_DC_df, out)
names(CoV_DC_df)

# ---------------------- #
# CONVERT TO LONG FORMAT #
# ---------------------- #

# Cumulative cases
CoV_DC_cumulative <-  CoV_DC_df %>% 
  dplyr::select(1:25,starts_with("Cumulative")) %>%
  pivot_longer(cols = starts_with("Cumulative"),
               values_to="cumulative",
               names_to=c("Cumulative","month", "day"),
               names_sep = "_") %>%  
  tidyr::unite("date_reported", month:day) %>%
  dplyr::select(-Cumulative) %>%
  dplyr::mutate(date_reported = as.Date(date_reported, format="%B_%d"))

summary(CoV_DC_cumulative$cumulative)

# Cumulative case rate
CoV_DC_caserate <-  CoV_DC_df %>%
  dplyr::select(1:25,starts_with("Case.Rate")) %>% 
  pivot_longer(cols = starts_with("Case.Rate"),
               values_to="case.rate",
               names_to=c("Case.Rate","month", "day"),
               names_sep = "_") %>%  
  tidyr::unite("date_reported", month:day) %>%
  dplyr::select(-Case.Rate) %>%
  dplyr::mutate(date_reported = as.Date(date_reported, format="%B_%d"))

summary(CoV_DC_caserate$case.rate)

# Cumulative testing
CoV_DC_tested <-  CoV_DC_df %>% 
  dplyr::select(1:25,starts_with("Tested")) %>% 
  pivot_longer(cols = starts_with("Tested"),
               values_to="tested",
               names_to=c("Tested","month", "day"),
               names_sep = "_") %>%  
  tidyr::unite("date_reported", month:day) %>%
  dplyr::select(-Tested) %>%
  dplyr::mutate(date_reported = as.Date(date_reported, format="%B_%d"))

summary(CoV_DC_tested$tested)

# Cumulative testing rate
CoV_DC_testrate <-  CoV_DC_df %>% 
  dplyr::select(1:25,starts_with("Test.Rate")) %>% 
  pivot_longer(cols = starts_with("Test.Rate"),
               values_to="test.rate",
               names_to=c("Test.Rate","month", "day"),
               names_sep = "_") %>%  
  tidyr::unite("date_reported", month:day) %>%
  dplyr::select(-Test.Rate) %>%
  dplyr::mutate(date_reported = as.Date(date_reported, format="%B_%d"))

summary(CoV_DC_testrate$test.rate)

# 5-day rolling average cases
CoV_DC_5d <-  CoV_DC_df %>% 
  dplyr::select(1:25,starts_with("average")) %>%
  pivot_longer(cols = starts_with("average"),
               values_to="average",
               names_to=c("Average","month", "day"),
               names_sep = "_") %>%  
  tidyr::unite("date_reported", month:day) %>%
  dplyr::select(-Average) %>%
  dplyr::mutate(date_reported = as.Date(date_reported, format="%B_%d"))

summary(CoV_DC_5d$average)

# 5-day rolling average case rate
CoV_DC_5drate <-  CoV_DC_df %>% 
  dplyr::select(1:25,starts_with("average.rate")) %>%
  pivot_longer(cols = starts_with("average.rate"),
               values_to="average.rate",
               names_to=c("Average.Rate","month", "day"),
               names_sep = "_") %>%  
  tidyr::unite("date_reported", month:day) %>%
  dplyr::select(-Average.Rate) %>%
  dplyr::mutate(date_reported = as.Date(date_reported, format="%B_%d"))

summary(CoV_DC_5drate$average.rate)

# Daily test positivity rate
CoV_DC_positivity <-  CoV_DC_df %>% 
  dplyr::select(1:25,starts_with("positivity")) %>%
  pivot_longer(cols = starts_with("positivity"),
               values_to="positivity",
               names_to=c("Positivity","month", "day"),
               names_sep = "_") %>%  
  tidyr::unite("date_reported", month:day) %>%
  dplyr::select(-Positivity) %>%
  dplyr::mutate(date_reported = as.Date(date_reported, format="%B_%d"))

## Data cleanup
CoV_DC_positivity$positivity <- ifelse(CoV_DC_positivity$positivity > 1, NA, CoV_DC_positivity$positivity)
summary(CoV_DC_positivity$positivity)

# 5-day rolling average test positivity
CoV_DC_5dtest <-  CoV_DC_df %>% 
  dplyr::select(1:25,starts_with("posit.avg")) %>%
  pivot_longer(cols = starts_with("posit.avg"),
               values_to="posit.avg",
               names_to=c("Posit.Avg","month", "day"),
               names_sep = "_") %>%  
  tidyr::unite("date_reported", month:day) %>%
  dplyr::select(-Posit.Avg) %>%
  dplyr::mutate(date_reported = as.Date(date_reported, format="%B_%d"))

## Data cleanup
CoV_DC_5dtest$posit.avg <- ifelse(is.infinite(CoV_DC_5dtest$posit.avg), NA, CoV_DC_5dtest$posit.avg)
CoV_DC_5dtest$posit.avg <- ifelse(CoV_DC_5dtest$posit.avg > 1, NA, CoV_DC_5dtest$posit.avg)
summary(CoV_DC_5dtest$posit.avg)

# ---------------------------- #
# ANIMATED GIFs FOR EACH VALUE #
# ---------------------------- #

# Cumulative cases
g1 <- CoV_DC_cumulative %>%                                       # data
  ggplot2::ggplot() +                                             # initial plot
  ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group, fill = cumulative), color = NA) + # add polygons
  ggplot2::scale_fill_gradient(name = "Cumulative Cases",
                               low = "lavenderblush",
                               high = "navyblue", 
                               na.value = "grey80",
                               breaks = range(CoV_DC_cumulative$cumulative, na.rm = T)) + # color fill 
  ggplot2::theme(line = ggplot2::element_blank(),                 # remove axis lines
                 axis.text = ggplot2::element_blank(),            # remove tickmarks
                 axis.title = ggplot2::element_blank(),           # remove axis labels
                 panel.background = ggplot2::element_blank(),     # remove gridlines
                 panel.border = ggplot2::element_blank(),         # remove border
                 legend.position = "bottom",                      # legend position
                 text = ggplot2::element_text(size = 15)) +       # set font size
  ggplot2::coord_equal() +                                        # force equal dimensions
  gganimate::transition_time(date_reported) +                     # animate by date
  ggplot2::labs(title = "Cumulative SARS-CoV-2 Cases\nDate: {frame_time}") # add title

gganimate::animate(g1, end_pause = 30) # animate

# Cumulative case rate
g2 <- CoV_DC_caserate %>%                                                 # data
  ggplot2::ggplot() +                                                     # initial plot
  ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group, fill = case.rate), color = NA) + # add polygons
  ggplot2::scale_fill_gradient(name = "Cumulative Case Rate",
                               low = "lavenderblush",
                               high = "navyblue", 
                               na.value = "grey80",
                               breaks = c(range(CoV_DC_caserate$case.rate, na.rm = T))) + # color fill 
  ggplot2::theme(line = ggplot2::element_blank(),                          # remove axis lines
                 axis.text = ggplot2::element_blank(),                     # remove tickmarks
                 axis.title = ggplot2::element_blank(),                    # remove axis labels
                 panel.background = ggplot2::element_blank(),              # remove gridlines
                 panel.border = ggplot2::element_blank(),                  # remove border
                 legend.position = "bottom",                               # legend position
                 text = ggplot2::element_text(size = 15)) +                # set font size
  ggplot2::coord_equal() +                                                 # force equal dimensions
  gganimate::transition_time(date_reported) +                              # animate by date
  ggplot2::labs(title = "Cumulative SARS-CoV-2 Case per 1,000\nDate: {frame_time}") # add title

gganimate::animate(g2, end_pause = 30) # animate

# Cumulative testing
g3 <- CoV_DC_tested %>%                                         # data
  ggplot2::ggplot() +                                           # initial plot
  ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group, fill = tested), color = NA) + # add polygons
  ggplot2::scale_fill_gradient(name = "Cumulative Tests",
                               low = "lavenderblush",
                               high = "navyblue", 
                               na.value = "grey80",
                               breaks = range(CoV_DC_tested$tested, na.rm = T)) + # color fill 
  ggplot2::theme(line = ggplot2::element_blank(),               # remove axis lines
                 axis.text = ggplot2::element_blank(),          # remove tickmarks
                 axis.title = ggplot2::element_blank(),         # remove axis labels
                 panel.background = ggplot2::element_blank(),   # remove gridlines
                 panel.border = ggplot2::element_blank(),       # remove border
                 legend.position = "bottom",                    # legend position
                 text = ggplot2::element_text(size = 15)) +     # set font size
  ggplot2::coord_equal() +                                      # force equal dimensions
  gganimate::transition_time(date_reported) +                   # animate by date
  ggplot2::labs(title = "Cumulative SARS-CoV-2 Testing\nDate: {frame_time}") # add title

gganimate::animate(g3, end_pause = 30) # animate

# Cumulative testing rate
g4 <- CoV_DC_testrate %>%                                       # data
  ggplot2::ggplot() +                                           # initial plot
  ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group, fill = test.rate), color = NA) + # add polygons
  ggplot2::scale_fill_gradient(name = "Cumulative Test Rate",
                               low = "lavenderblush",
                               high = "navyblue", 
                               na.value = "grey80",
                               breaks = range(CoV_DC_testrate$test.rate, na.rm = T)) + # color fill 
  ggplot2::theme(line = ggplot2::element_blank(),               # remove axis lines
                 axis.text = ggplot2::element_blank(),          # remove tickmarks
                 axis.title = ggplot2::element_blank(),         # remove axis labels
                 panel.background = ggplot2::element_blank(),   # remove gridlines
                 panel.border = ggplot2::element_blank(),       # remove border
                 legend.position = "bottom",                    # legend position
                 text = ggplot2::element_text(size = 15)) +     # set font size
  ggplot2::coord_equal() +                                      # force equal dimensions
  gganimate::transition_time(date_reported) +                   # animate by date
  ggplot2::labs(title = "Cumulative SARS-CoV-2 Testing per 1,000\nDate: {frame_time}") # add title

gganimate::animate(g4, end_pause = 30) # animate

# 5-day rolling average cases
g5 <- CoV_DC_5d %>%                                             # data
  ggplot2::ggplot() +                                           # initial plot
  ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group, fill = average), color = NA) + # add polygons
  ggplot2::scale_fill_gradient(name = "5-day average cases",
                               low = "lavenderblush",
                               high = "navyblue", 
                               na.value = "grey80",
                               breaks = range(CoV_DC_5d$average, na.rm = T)) + # color fill 
  ggplot2::theme(line = ggplot2::element_blank(),               # remove axis lines
                 axis.text = ggplot2::element_blank(),          # remove tickmarks
                 axis.title = ggplot2::element_blank(),         # remove axis labels
                 panel.background = ggplot2::element_blank(),   # remove gridlines
                 panel.border = ggplot2::element_blank(),       # remove border
                 legend.position = "bottom",                    # legend position
                 text = ggplot2::element_text(size = 15)) +     # set font size
  ggplot2::coord_equal() +                                      # force equal dimensions
  gganimate::transition_time(date_reported) +                   # animate by date
  ggplot2::labs(title = "5-Day Moving Average SARS-CoV-2 Cases\nDate: {frame_time}") # add title

gganimate::animate(g5, end_pause = 30) # animate

# 5-day rolling average case rate
g6 <- CoV_DC_5drate %>%                                         # data
  ggplot2::ggplot() +                                           # initial plot
  ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group, fill = average.rate), color = NA) + # add polygons
  ggplot2::scale_fill_gradient(name = "5-day average cases per 1,000",
                               low = "lavenderblush",
                               high = "navyblue", 
                               na.value = "grey80",
                               breaks = range(CoV_DC_5drate$average.rate, na.rm = T)) + # color fill 
  ggplot2::theme(line = ggplot2::element_blank(),               # remove axis lines
                 axis.text = ggplot2::element_blank(),          # remove tickmarks
                 axis.title = ggplot2::element_blank(),         # remove axis labels
                 panel.background = ggplot2::element_blank(),   # remove gridlines
                 panel.border = ggplot2::element_blank(),       # remove border
                 legend.position = "bottom",                    # legend position
                 text = ggplot2::element_text(size = 15)) +     # set font size
  ggplot2::coord_equal() +                                      # force equal dimensions
  gganimate::transition_time(date_reported) +                   # animate by date
  ggplot2::labs(title = "5-Day Moving Average SARS-CoV-2 Case Rate\nDate: {frame_time}") # add title

gganimate::animate(g6, end_pause = 30) # animate

# Daily test positivity
g7 <- CoV_DC_positivity %>%                                     # data
  ggplot2::ggplot() +                                           # initial plot
  ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group, fill = positivity), color = NA) + # add polygons
  ggplot2::scale_fill_gradient(name = "Test positivity rate",
                               low = "lavenderblush",
                               high = "navyblue", 
                               na.value = "grey80",
                               breaks = range(CoV_DC_positivity$positivity, na.rm = T)) + # color fill 
  ggplot2::theme(line = ggplot2::element_blank(),               # remove axis lines
                 axis.text = ggplot2::element_blank(),          # remove tickmarks
                 axis.title = ggplot2::element_blank(),         # remove axis labels
                 panel.background = ggplot2::element_blank(),   # remove gridlines
                 panel.border = ggplot2::element_blank(),       # remove border
                 legend.position = "bottom",                    # legend position
                 text = ggplot2::element_text(size = 15)) +     # set font size
  ggplot2::coord_equal() +                                      # force equal dimensions
  gganimate::transition_time(date_reported) +                   # animate by date
  ggplot2::labs(title = "SARS-CoV-2 Test Positivity Rate\nDate: {frame_time}") # add title

gganimate::animate(g7, end_pause = 30) # animate

# 5-day rolling average test positivity
g8 <- CoV_DC_5dtest %>%                                         # data
  ggplot2::ggplot() +                                           # initial plot
  ggplot2::geom_polygon(ggplot2::aes(x = long, y = lat, group = group, fill = posit.avg), color = NA) + # add polygons
  ggplot2::scale_fill_gradient(name = "5-day average rate",
                               low = "lavenderblush",
                               high = "navyblue", 
                               na.value = "grey80",
                               breaks = range(CoV_DC_5dtest$posit.avg, na.rm = T)) + # color fill 
  ggplot2::theme(line = ggplot2::element_blank(),               # remove axis lines
                 axis.text = ggplot2::element_blank(),          # remove tickmarks
                 axis.title = ggplot2::element_blank(),         # remove axis labels
                 panel.background = ggplot2::element_blank(),   # remove gridlines
                 panel.border = ggplot2::element_blank(),       # remove border
                 legend.position = "bottom",                    # legend position
                 text = ggplot2::element_text(size = 15)) +     # set font size
  ggplot2::coord_equal() +                                      # force equal dimensions
  gganimate::transition_time(date_reported) +                   # animate by date
  ggplot2::labs(title = "5-Day Average SARS-CoV-2 Test Positivity\nDate: {frame_time}") # add title

gganimate::animate(g8, end_pause = 30) # animate

# -------------------------------------- #
# ANOTHER METHOD: DOUBLE-CHECK FOR LOOPS #
# -------------------------------------- #

# Daily incident cases
CoV_DC_df$incident_May_8 <- ifelse(CoV_DC_df$Cumulative_May_8 - CoV_DC_df$Cumulative_May_7 < 0, NA, CoV_DC_df$Cumulative_May_8 - CoV_DC_df$Cumulative_May_7)
CoV_DC_df$incident_May_9 <- ifelse(CoV_DC_df$Cumulative_May_9 - CoV_DC_df$Cumulative_May_8 < 0, NA, CoV_DC_df$Cumulative_May_9 - CoV_DC_df$Cumulative_May_8)
CoV_DC_df$incident_May_10 <- ifelse(CoV_DC_df$Cumulative_May_10 - CoV_DC_df$Cumulative_May_9 < 0, NA, CoV_DC_df$Cumulative_May_10 - CoV_DC_df$Cumulative_May_9)
CoV_DC_df$incident_May_11 <- ifelse(CoV_DC_df$Cumulative_May_11 - CoV_DC_df$Cumulative_May_10 < 0, NA, CoV_DC_df$Cumulative_May_11 - CoV_DC_df$Cumulative_May_10)
CoV_DC_df$incident_May_12 <- ifelse(CoV_DC_df$Cumulative_May_12 - CoV_DC_df$Cumulative_May_11 < 0, NA, CoV_DC_df$Cumulative_May_12 - CoV_DC_df$Cumulative_May_11)
CoV_DC_df$incident_May_13 <- ifelse(CoV_DC_df$Cumulative_May_13 - CoV_DC_df$Cumulative_May_12 < 0, NA, CoV_DC_df$Cumulative_May_13 - CoV_DC_df$Cumulative_May_12)
CoV_DC_df$incident_May_14 <- ifelse(CoV_DC_df$Cumulative_May_14 - CoV_DC_df$Cumulative_May_13 < 0, NA, CoV_DC_df$Cumulative_May_14 - CoV_DC_df$Cumulative_May_13)
CoV_DC_df$incident_May_15 <- ifelse(CoV_DC_df$Cumulative_May_15 - CoV_DC_df$Cumulative_May_14 < 0, NA, CoV_DC_df$Cumulative_May_15 - CoV_DC_df$Cumulative_May_14)
CoV_DC_df$incident_May_16 <- ifelse(CoV_DC_df$Cumulative_May_16 - CoV_DC_df$Cumulative_May_15 < 0, NA, CoV_DC_df$Cumulative_May_16 - CoV_DC_df$Cumulative_May_15)
CoV_DC_df$incident_May_17 <- ifelse(CoV_DC_df$Cumulative_May_17 - CoV_DC_df$Cumulative_May_16 < 0, NA, CoV_DC_df$Cumulative_May_17 - CoV_DC_df$Cumulative_May_16)
CoV_DC_df$incident_May_18 <- ifelse(CoV_DC_df$Cumulative_May_18 - CoV_DC_df$Cumulative_May_17 < 0, NA, CoV_DC_df$Cumulative_May_18 - CoV_DC_df$Cumulative_May_17)
CoV_DC_df$incident_May_19 <- ifelse(CoV_DC_df$Cumulative_May_19 - CoV_DC_df$Cumulative_May_18 < 0, NA, CoV_DC_df$Cumulative_May_19 - CoV_DC_df$Cumulative_May_18)
CoV_DC_df$incident_May_20 <- ifelse(CoV_DC_df$Cumulative_May_20 - CoV_DC_df$Cumulative_May_19 < 0, NA, CoV_DC_df$Cumulative_May_20 - CoV_DC_df$Cumulative_May_19)
CoV_DC_df$incident_May_21 <- ifelse(CoV_DC_df$Cumulative_May_21 - CoV_DC_df$Cumulative_May_20 < 0, NA, CoV_DC_df$Cumulative_May_21 - CoV_DC_df$Cumulative_May_20)
CoV_DC_df$incident_May_22 <- NA # Missing data
CoV_DC_df$incident_May_23 <- NA # Missing data
CoV_DC_df$incident_May_24 <- ifelse(CoV_DC_df$Cumulative_May_24 - CoV_DC_df$Cumulative_May_23 < 0, NA, CoV_DC_df$Cumulative_May_24 - CoV_DC_df$Cumulative_May_23)
CoV_DC_df$incident_May_25 <- ifelse(CoV_DC_df$Cumulative_May_25 - CoV_DC_df$Cumulative_May_24 < 0, NA, CoV_DC_df$Cumulative_May_25 - CoV_DC_df$Cumulative_May_24)
CoV_DC_df$incident_May_26 <- ifelse(CoV_DC_df$Cumulative_May_26 - CoV_DC_df$Cumulative_May_25 < 0, NA, CoV_DC_df$Cumulative_May_26 - CoV_DC_df$Cumulative_May_25)
CoV_DC_df$incident_May_27 <- NA # Missing data
CoV_DC_df$incident_May_28 <- NA # Missing data
CoV_DC_df$incident_May_29 <- ifelse(CoV_DC_df$Cumulative_May_29 - CoV_DC_df$Cumulative_May_28 < 0, NA, CoV_DC_df$Cumulative_May_29 - CoV_DC_df$Cumulative_May_28)
CoV_DC_df$incident_May_30 <- ifelse(CoV_DC_df$Cumulative_May_30 - CoV_DC_df$Cumulative_May_29 < 0, NA, CoV_DC_df$Cumulative_May_30 - CoV_DC_df$Cumulative_May_29)
CoV_DC_df$incident_May_31 <- ifelse(CoV_DC_df$Cumulative_May_31 - CoV_DC_df$Cumulative_May_30 < 0, NA, CoV_DC_df$Cumulative_Jun_1 - CoV_DC_df$Cumulative_May_31)
CoV_DC_df$incident_Jun_1 <- ifelse(CoV_DC_df$Cumulative_Jun_1 - CoV_DC_df$Cumulative_May_31 < 0, NA, CoV_DC_df$Cumulative_Jun_1 - CoV_DC_df$Cumulative_May_31)
CoV_DC_df$incident_Jun_2 <- ifelse(CoV_DC_df$Cumulative_Jun_2 - CoV_DC_df$Cumulative_Jun_1 < 0, NA, CoV_DC_df$Cumulative_Jun_3 - CoV_DC_df$Cumulative_Jun_2)
CoV_DC_df$incident_Jun_3 <- ifelse(CoV_DC_df$Cumulative_Jun_3 - CoV_DC_df$Cumulative_Jun_2 < 0, NA, CoV_DC_df$Cumulative_Jun_3 - CoV_DC_df$Cumulative_Jun_2)
CoV_DC_df$incident_Jun_4 <- ifelse(CoV_DC_df$Cumulative_Jun_4 - CoV_DC_df$Cumulative_Jun_3 < 0, NA, CoV_DC_df$Cumulative_Jun_4 - CoV_DC_df$Cumulative_Jun_3)
CoV_DC_df$incident_Jun_5 <- ifelse(CoV_DC_df$Cumulative_Jun_5 - CoV_DC_df$Cumulative_Jun_4 < 0, NA, CoV_DC_df$Cumulative_Jun_5 - CoV_DC_df$Cumulative_Jun_4)
CoV_DC_df$incident_Jun_6 <- ifelse(CoV_DC_df$Cumulative_Jun_6 - CoV_DC_df$Cumulative_Jun_5 < 0, NA, CoV_DC_df$Cumulative_Jun_6 - CoV_DC_df$Cumulative_Jun_5)
CoV_DC_df$incident_Jun_7 <- ifelse(CoV_DC_df$Cumulative_Jun_7 - CoV_DC_df$Cumulative_Jun_6 < 0, NA, CoV_DC_df$Cumulative_Jun_7 - CoV_DC_df$Cumulative_Jun_6)

# 5-day rolling average incident cases
CoV_DC_df$average_May_12 <- rowMeans(CoV_DC_df[,125:129], na.rm = T)
CoV_DC_df$average_May_13 <- rowMeans(CoV_DC_df[,126:130], na.rm = T)
CoV_DC_df$average_May_14 <- rowMeans(CoV_DC_df[,127:131], na.rm = T)
CoV_DC_df$average_May_15 <- rowMeans(CoV_DC_df[,128:132], na.rm = T)
CoV_DC_df$average_May_16 <- rowMeans(CoV_DC_df[,129:133], na.rm = T)
CoV_DC_df$average_May_17 <- rowMeans(CoV_DC_df[,130:134], na.rm = T)
CoV_DC_df$average_May_18 <- rowMeans(CoV_DC_df[,131:135], na.rm = T)
CoV_DC_df$average_May_19 <- rowMeans(CoV_DC_df[,132:136], na.rm = T)
CoV_DC_df$average_May_20 <- rowMeans(CoV_DC_df[,133:137], na.rm = T)
CoV_DC_df$average_May_21 <- rowMeans(CoV_DC_df[,134:138], na.rm = T)
CoV_DC_df$average_May_22 <- rowMeans(CoV_DC_df[,135:139], na.rm = T)
CoV_DC_df$average_May_23 <- rowMeans(CoV_DC_df[,136:140], na.rm = T)
CoV_DC_df$average_May_24 <- rowMeans(CoV_DC_df[,137:141], na.rm = T)
CoV_DC_df$average_May_25 <- rowMeans(CoV_DC_df[,138:142], na.rm = T)
CoV_DC_df$average_May_26 <- rowMeans(CoV_DC_df[,139:143], na.rm = T)
CoV_DC_df$average_May_27 <- rowMeans(CoV_DC_df[,140:144], na.rm = T)
CoV_DC_df$average_May_28 <- rowMeans(CoV_DC_df[,141:145], na.rm = T)
CoV_DC_df$average_May_29 <- rowMeans(CoV_DC_df[,142:146], na.rm = T)
CoV_DC_df$average_May_30 <- rowMeans(CoV_DC_df[,143:147], na.rm = T)
CoV_DC_df$average_May_31 <- rowMeans(CoV_DC_df[,144:148], na.rm = T)
CoV_DC_df$average_Jun_1 <- rowMeans(CoV_DC_df[,145:149], na.rm = T)
CoV_DC_df$average_Jun_2 <- rowMeans(CoV_DC_df[,146:150], na.rm = T)
CoV_DC_df$average_Jun_3 <- rowMeans(CoV_DC_df[,147:151], na.rm = T)
CoV_DC_df$average_Jun_4 <- rowMeans(CoV_DC_df[,148:152], na.rm = T)
CoV_DC_df$average_Jun_5 <- rowMeans(CoV_DC_df[,149:153], na.rm = T)
CoV_DC_df$average_Jun_6 <- rowMeans(CoV_DC_df[,150:154], na.rm = T)
CoV_DC_df$average_Jun_7 <- rowMeans(CoV_DC_df[,151:155], na.rm = T)

# 5-day rolling average incident case rate
CoV_DC_df$average.rate_May_12 <- CoV_DC_df$average_May_12 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_13 <- CoV_DC_df$average_May_13 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_14 <- CoV_DC_df$average_May_14 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_15 <- CoV_DC_df$average_May_15 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_16 <- CoV_DC_df$average_May_16 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_17 <- CoV_DC_df$average_May_17 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_18 <- CoV_DC_df$average_May_18 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_19 <- CoV_DC_df$average_May_19 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_20 <- CoV_DC_df$average_May_20 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_21 <- CoV_DC_df$average_May_21 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_22 <- CoV_DC_df$average_May_22 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_23 <- CoV_DC_df$average_May_23 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_24 <- CoV_DC_df$average_May_24 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_25 <- CoV_DC_df$average_May_25 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_26 <- CoV_DC_df$average_May_26 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_27 <- CoV_DC_df$average_May_27 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_28 <- CoV_DC_df$average_May_28 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_29 <- CoV_DC_df$average_May_29 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_30 <- CoV_DC_df$average_May_30 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_May_31 <- CoV_DC_df$average_May_31 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_Jun_1 <- CoV_DC_df$average_Jun_1 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_Jun_2 <- CoV_DC_df$average_Jun_2 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_Jun_3 <- CoV_DC_df$average_Jun_3 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_Jun_4 <- CoV_DC_df$average_Jun_4 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_Jun_5 <- CoV_DC_df$average_Jun_5 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_Jun_6 <- CoV_DC_df$average_Jun_6 / CoV_DC_df$`Population_(2018_ACS)` * 1000
CoV_DC_df$average.rate_Jun_7 <- CoV_DC_df$average_Jun_7 / CoV_DC_df$`Population_(2018_ACS)` * 1000

# Testing per day
CoV_DC_df$testing_May_21 <- ifelse(CoV_DC_df$Tested_May_21 - CoV_DC_df$Tested_May_20 < 0, NA, CoV_DC_df$Tested_May_21 - CoV_DC_df$Tested_May_20)
CoV_DC_df$testing_May_22 <- NA # Missing data
CoV_DC_df$testing_May_23 <- NA # Missing data
CoV_DC_df$testing_May_24 <- NA # Missing data
CoV_DC_df$testing_May_25 <- NA # Missing data
CoV_DC_df$testing_May_26 <- ifelse(CoV_DC_df$Tested_May_26 - CoV_DC_df$Tested_May_25 < 0, NA, CoV_DC_df$Tested_May_26 - CoV_DC_df$Tested_May_25)
CoV_DC_df$testing_May_27 <- NA # Missing data
CoV_DC_df$testing_May_28 <- NA # Missing data
CoV_DC_df$testing_May_29 <- ifelse(CoV_DC_df$Tested_May_29 - CoV_DC_df$Tested_May_28 < 0, NA, CoV_DC_df$Tested_May_29 - CoV_DC_df$Tested_May_28)
CoV_DC_df$testing_May_30 <- ifelse(CoV_DC_df$Tested_May_30 - CoV_DC_df$Tested_May_29 < 0, NA, CoV_DC_df$Tested_May_30 - CoV_DC_df$Tested_May_29)
CoV_DC_df$testing_May_31 <- ifelse(CoV_DC_df$Tested_May_31 - CoV_DC_df$Tested_May_30 < 0, NA, CoV_DC_df$Tested_Jun_1 - CoV_DC_df$Tested_May_31)
CoV_DC_df$testing_Jun_1 <- ifelse(CoV_DC_df$Tested_Jun_1 - CoV_DC_df$Tested_May_31 < 0, NA, CoV_DC_df$Tested_Jun_1 - CoV_DC_df$Tested_May_31)
CoV_DC_df$testing_Jun_2 <- ifelse(CoV_DC_df$Tested_Jun_2 - CoV_DC_df$Tested_Jun_1 < 0, NA, CoV_DC_df$Tested_Jun_3 - CoV_DC_df$Tested_Jun_2)
CoV_DC_df$testing_Jun_3 <- ifelse(CoV_DC_df$Tested_Jun_3 - CoV_DC_df$Tested_Jun_2 < 0, NA, CoV_DC_df$Tested_Jun_3 - CoV_DC_df$Tested_Jun_2)
CoV_DC_df$testing_Jun_4 <- ifelse(CoV_DC_df$Tested_Jun_4 - CoV_DC_df$Tested_Jun_3 < 0, NA, CoV_DC_df$Tested_Jun_4 - CoV_DC_df$Tested_Jun_3)
CoV_DC_df$testing_Jun_5 <- ifelse(CoV_DC_df$Tested_Jun_5 - CoV_DC_df$Tested_Jun_4 < 0, NA, CoV_DC_df$Tested_Jun_5 - CoV_DC_df$Tested_Jun_4)
CoV_DC_df$testing_Jun_6 <- ifelse(CoV_DC_df$Tested_Jun_6 - CoV_DC_df$Tested_Jun_5 < 0, NA, CoV_DC_df$Tested_Jun_6 - CoV_DC_df$Tested_Jun_5)
CoV_DC_df$testing_Jun_7 <- ifelse(CoV_DC_df$Tested_Jun_7 - CoV_DC_df$Tested_Jun_6 < 0, NA, CoV_DC_df$Tested_Jun_7 - CoV_DC_df$Tested_Jun_6)

# Daily test positivity
CoV_DC_df$positivity_May_21 <- CoV_DC_df$incident_May_21 / CoV_DC_df$testing_May_21
CoV_DC_df$positivity_May_22 <- NA # Missing data
CoV_DC_df$positivity_May_23 <- NA # Missing data
CoV_DC_df$positivity_May_24 <- NA # Missing data
CoV_DC_df$positivity_May_25 <- NA # Missing data
CoV_DC_df$positivity_May_26 <- CoV_DC_df$incident_May_26 / CoV_DC_df$testing_May_26
CoV_DC_df$positivity_May_27 <- NA # Missing data
CoV_DC_df$positivity_May_28 <- NA # Missing data
CoV_DC_df$positivity_May_29 <- CoV_DC_df$incident_May_29 / CoV_DC_df$testing_May_29
CoV_DC_df$positivity_May_30 <- CoV_DC_df$incident_May_30 / CoV_DC_df$testing_May_30
CoV_DC_df$positivity_May_31 <- CoV_DC_df$incident_May_31 / CoV_DC_df$testing_May_31
CoV_DC_df$positivity_Jun_1 <- CoV_DC_df$incident_Jun_1 / CoV_DC_df$testing_Jun_1
CoV_DC_df$positivity_Jun_2 <- CoV_DC_df$incident_Jun_2 / CoV_DC_df$testing_Jun_2
CoV_DC_df$positivity_Jun_3 <- CoV_DC_df$incident_Jun_3 / CoV_DC_df$testing_Jun_3
CoV_DC_df$positivity_Jun_4 <- CoV_DC_df$incident_Jun_4 / CoV_DC_df$testing_Jun_4
CoV_DC_df$positivity_Jun_5 <- CoV_DC_df$incident_Jun_5 / CoV_DC_df$testing_Jun_5
CoV_DC_df$positivity_Jun_6 <- CoV_DC_df$incident_Jun_6 / CoV_DC_df$testing_Jun_6
CoV_DC_df$positivity_Jun_7 <- CoV_DC_df$incident_Jun_7 / CoV_DC_df$testing_Jun_7

# 5-day rolling average testing
CoV_DC_df$test.avg_May_25 <- rowMeans(CoV_DC_df[,204:208], na.rm = T)
CoV_DC_df$test.avg_May_26 <- rowMeans(CoV_DC_df[,205:209], na.rm = T)
CoV_DC_df$test.avg_May_27 <- rowMeans(CoV_DC_df[,206:210], na.rm = T)
CoV_DC_df$test.avg_May_28 <- rowMeans(CoV_DC_df[,207:211], na.rm = T)
CoV_DC_df$test.avg_May_29 <- rowMeans(CoV_DC_df[,208:212], na.rm = T)
CoV_DC_df$test.avg_May_30 <- rowMeans(CoV_DC_df[,209:213], na.rm = T)
CoV_DC_df$test.avg_May_31 <- rowMeans(CoV_DC_df[,210:214], na.rm = T)
CoV_DC_df$test.avg_Jun_1 <- rowMeans(CoV_DC_df[,211:215], na.rm = T)
CoV_DC_df$test.avg_Jun_2 <- rowMeans(CoV_DC_df[,212:216], na.rm = T)
CoV_DC_df$test.avg_Jun_3 <- rowMeans(CoV_DC_df[,213:217], na.rm = T)
CoV_DC_df$test.avg_Jun_4 <- rowMeans(CoV_DC_df[,214:218], na.rm = T)
CoV_DC_df$test.avg_Jun_5 <- rowMeans(CoV_DC_df[,215:219], na.rm = T)
CoV_DC_df$test.avg_Jun_6 <- rowMeans(CoV_DC_df[,216:220], na.rm = T)
CoV_DC_df$test.avg_Jun_7 <- rowMeans(CoV_DC_df[,217:221], na.rm = T)

# 5-day rolling average test positivity
CoV_DC_df$posit.avg_May_25 <- CoV_DC_df$average_May_25 / CoV_DC_df$test.avg_May_25
CoV_DC_df$posit.avg_May_26 <- CoV_DC_df$average_May_26 / CoV_DC_df$test.avg_May_26
CoV_DC_df$posit.avg_May_27 <- CoV_DC_df$average_May_27 / CoV_DC_df$test.avg_May_27
CoV_DC_df$posit.avg_May_28 <- CoV_DC_df$average_May_28 / CoV_DC_df$test.avg_May_28
CoV_DC_df$posit.avg_May_29 <- CoV_DC_df$average_May_29 / CoV_DC_df$test.avg_May_29
CoV_DC_df$posit.avg_May_30 <- CoV_DC_df$average_May_30 / CoV_DC_df$test.avg_May_30
CoV_DC_df$posit.avg_May_31 <- CoV_DC_df$average_May_31 / CoV_DC_df$test.avg_May_31
CoV_DC_df$posit.avg_Jun_1 <- CoV_DC_df$average_Jun_1 / CoV_DC_df$test.avg_Jun_1
CoV_DC_df$posit.avg_Jun_2 <- CoV_DC_df$average_Jun_2 / CoV_DC_df$test.avg_Jun_2
CoV_DC_df$posit.avg_Jun_3 <- CoV_DC_df$average_Jun_3 / CoV_DC_df$test.avg_Jun_3
CoV_DC_df$posit.avg_Jun_4 <- CoV_DC_df$average_Jun_4 / CoV_DC_df$test.avg_Jun_4
CoV_DC_df$posit.avg_Jun_5 <- CoV_DC_df$average_Jun_5 / CoV_DC_df$test.avg_Jun_5
CoV_DC_df$posit.avg_Jun_6 <- CoV_DC_df$average_Jun_6 / CoV_DC_df$test.avg_Jun_6
CoV_DC_df$posit.avg_Jun_7 <- CoV_DC_df$average_Jun_7 / CoV_DC_df$test.avg_Jun_7

# -------------------------- END OF CODE -------------------------- #  