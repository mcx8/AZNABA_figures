# Maxine Cruz
# tmcruz@arizona.edu
# Created: 21 September 2023
# Last modified: 26 September 2023




# ----- ABOUT THE SCRIPT -----

# Generates Figure 1 for AZNABA

# KATY NOTES:
  # Make black and white
  # Have 3 shapes for season sampled (Fall, Spring, Both)
    # Black circle, white circle, black and white circle
  # Data frame headers:
    # Site no. | Site name | Lat | Long | Elevation | Season | Years | Times

# Original figure is terrain map with different color for each site
  # and three different shapes (circle, square, triangle)




# ----- LOAD LIBRARIES -----

library(leaflet)
library(ggmap)
library(ggplot2)
library(usmap)
library(tidyverse)




# ----- LOAD DATA -----

# Coordinates and names of sample sites
site_coords <- read.csv("data/lat-long-site.csv")

# For filtering spring sample sites
spring_data <- read.csv("data/Spring_Analysis.csv")

# For filtering fall sample sites
fall_data <- read.csv("data/Fall_Analysis.csv")




# ----- DEAL WITH FALL SITES -----

# Subset columns from fall_data
fall_df <- select(fall_data, 
                  Site, Latitude, Longitude, Elevation, 
                  Year = year, 
                  Month = month, 
                  Day = day)

# Add column designating that these are fall samples
fall_df$Season_Sampled <- "Fall"




# ----- DEAL WITH SPRING SITES -----

# Subset columns from spring_data
spring_df <- select(spring_data, 
                  Site, Latitude, Longitude, Elevation, 
                  Year = year, 
                  Month = month, 
                  Day = day)

# Add column designating that these are spring samples
spring_df$Season_Sampled <- "Spring"





# ----- ORGANIZE DATA FRAME FOR FIGURE -----

# Combine filtered fall and spring data into one data frame
both_szn <- rbind(fall_df, spring_df)

# Sort both_szn alphabetically by site, and then by Year
both_szn <- both_szn[order(both_szn$Site, both_szn$Year), ]

# GrandCanyonSouthRim, McDowellSonoranPreserve, and SabinoCanyonAZ have samples 
# from fall and spring. We want to change their Season_Sampled column to
# Fall/Spring.
both_szn <- both_szn %>%
  mutate(Season_Sampled = 
           ifelse(Site == "GrandCanyonSouthRim" |
                    Site == "McDowellSonoranPreserve" |
                    Site == "SabinoCanyonAZ", 
                  "Fall/Spring", # If Site is named one of the above, then this
                  Season_Sampled)) # Otherwise, keep original season

# Create column for assigning site numbers to sites_coords
nums <- data.frame(Site_Number = cbind(seq(1, 13)))

# Remove sites not used in study from site_coords
site_coords <- site_coords[-c(12, 13, 14), ]

# Reorganize site_coords alphabetically by site
site_coords <- site_coords[order(site_coords$Site), ]

# Combine number assignment column to site_coords
site_coords <- cbind(nums, site_coords)

# Count number of years that each site was sampled
fig_df <- merge(site_coords, aggregate(Year ~ Site, 
                                       data = both_szn, 
                                       FUN = function(x) length(unique(x))))

# Count number of times that each site was sampled
fig_df <- merge(fig_df, aggregate(Month ~ Site, 
                                  data = both_szn, 
                                  FUN = length))

# Note to self: The difference between line 113 and line 118 is that
# 113 accounts for unique years, whereas 118 counts the number of year entries.
# Since using Year twice might get confusing, Month was used since it
# yielded the same results.

# Get Elevations and Season_Sampled of each site from both_szn
eleva_szn <- unique(select(both_szn,
                           Site, Elevation, Season_Sampled))

# Add column with elevations to fig_df
fig_df <- merge(fig_df, eleva_szn, all.x = TRUE)

# Reorganize column order in data frame
fig_df <- select(fig_df,
                 2, 1, 3, 4, 7, 8 , 5, 6)

# Rename some columns
names(fig_df)[7] <- "Number_Years_Sampled"
names(fig_df)[8] <- "Number_Times_Sampled"

# Save data frame
write_csv(fig_df, "data_mc/figure_1_data.csv")

# ----- GENERATE FIGURE 1 -----

# Get bounding box coordinates (from Google Maps)
az_bb <- c(
  left = -115.187847,
  top = 37.271670,
  bottom = 31.046601, 
  right = -108.502422
)

# Use bounding box to get map
az_stamen <- get_stamenmap(
  bbox = az_bb,
  zoom = 10,
  maptype = "terrain-background",
  color = "bw",
  force = TRUE
)

# Get state border lines
az_lines <- map_data("state", "arizona")

# Plot map
fig_1 <- ggmap(az_stamen) +
  geom_point(data = fig_df,
             aes(x = Longitude, y = Latitude, color = Site)) + # Adds background points so we can have a legend for Site
  geom_point(data = fig_df,
             aes(x = Longitude, y = Latitude, 
                 shape = Season_Sampled),
             size = 5) + # Layers on different shapes depending on when each site was sampled
  geom_polygon(data = az_lines,
               aes(x = long, y = lat, group = group),
               fill = NA,
               color = "black",
               linewidth = 0.7
  ) + # Adds border outline of Arizona
  geom_text(data = fig_df,
            aes(x = Longitude, y = Latitude, label = Site_Number),
            color = "white",
            size = 2) + # Adds Site_Number on top of each site point
  xlab("Longitude") +
  ylab("Latitude") +
  guides(shape = guide_legend(title = "Season Sampled")) + # Change legend title
  scale_color_manual(
    name = "Site",
    breaks = c("AtascosaHighlandsAZ", 
               "BoyceThompsonArboretum",
               "Cottonwood", 
               "GrandCanyonDesertView",
               "GrandCanyonNorthRim", 
               "GrandCanyonSouthRim",
               "McDowellSonoranPreserve", 
               "PatagoniaAZ", 
               "PortalAZ",
               "RamseyCanyonAZ", 
               "SabinoCanyonAZ",
               "SantaRitaMountains", 
               "SycamoreCreekAZ"
               ), # Generated legend names
    values = c("AtascosaHighlandsAZ" = "grey13", 
               "BoyceThompsonArboretum" = "grey12",
               "Cottonwood" = "grey11", 
               "GrandCanyonDesertView" = "grey10",
               "GrandCanyonNorthRim" = "grey9", 
               "GrandCanyonSouthRim" = "grey8",
               "McDowellSonoranPreserve" = "grey7", 
               "PatagoniaAZ" = "grey6", 
               "PortalAZ" = "grey5",
               "RamseyCanyonAZ" = "grey4", 
               "SabinoCanyonAZ" = "grey3",
               "SantaRitaMountains" = "grey2", 
               "SycamoreCreekAZ" = "grey1"
               ), # Make it look like they're all just bullet points
    labels = c("1 - Atascosa Highlands", 
               "2 - Boyce Thompson Arboretum",
               "3 - Cottonwood", 
               "4 - Grand Canyon Desert View",
               "5 - Grand Canyon North Rim", 
               "6 - Grand Canyon South Rim",
               "7 - McDowell Sonoran Preserve", 
               "8 - Patagonia", 
               "9 - Portal",
               "10 - Ramsey Canyon", 
               "11 - Sabino Canyon",
               "12 - Santa Rita Mountains", 
               "13 - Sycamore Creek"
               ) # Change legend names to these
    ) + # Modify legend with Site names
  theme(axis.title.x = element_text(margin = margin(t = 10), size = 12),
        axis.title.y = element_text(margin = margin(r = 10), size = 12)
  ) # Modify alignment of axis titles so they're not so close to plot

# Save plot
# Since ggsave is being silly, I am saving the plot from the Plots export




# ----- FIRST ATTEMPT MAPPING WITH LEAFLET THAT DID NOT WORK WELL -----

# This is just here for my own records.

# Add column to data for marker designation 
# (Since we want a different one per season)
# fig_df <- fig_df %>%
#  mutate(Icon = case_when(
#    Season_Sampled == "Fall" ~ "filled_circle",
#    Season_Sampled == "Spring" ~ "empty_circle",
#    Season_Sampled == "Fall/Spring" ~ "half_fill_circle"
#  ))

# Assign icons
# my_icons <- iconList(
#  "filled_circle" = makeIcon(iconUrl = "C:/Users/maxin/Documents/Prudic_Lab/AZNABA_figures/circle-solid.png",
#                             iconWidth = 14, iconHeight = 14),
#  "empty_circle" = makeIcon(iconUrl = "C:/Users/maxin/Documents/Prudic_Lab/AZNABA_figures/circle-regular.png",
#                            iconWidth = 14, iconHeight = 14),
#  "half_fill_circle" = makeIcon(iconUrl = "C:/Users/maxin/Documents/Prudic_Lab/AZNABA_figures/circle-half-stroke-solid.png",
#                                iconWidth = 14, iconHeight = 14)
# )

# Generate legend for map
# html_legend <- "<script src='https://unpkg.com/ionicons@latest/dist/ionicons.js'></script> <ion-icon name='ellipse'></ion-icon> Fall <br/> <ion-icon name='ellipse-outline'></ion-icon> Spring <br/> <ion-icon name='contrast-outline'></ion-icon> Fall/Spring"

# Generate map of sites
# leaflet(fig_df,
#        options = leafletOptions(zoomControl = FALSE, # No zoom button
#                                 attributionControl = FALSE)) %>%  # No banner
#  addProviderTiles("Esri.WorldTerrain") %>%
#  addProviderTiles("Stamen.TonerLines") %>%
#  setView(lng = -111.682299, lat = 34.296789, zoom = 6) %>%
#  addMarkers(
#    lng = fig_df$Longitude,
#    lat = fig_df$Latitude,
#    icon = my_icons[fig_df$Icon]
#  ) %>%
#  addControl(html = html_legend, position = "bottomleft") # Icons not showing??!



