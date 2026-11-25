# Script for making a map of the study area indicating spatial variation in forest cover
# from NLCD, and locations of 5 study sites in relation to 3 cities.

library(tigris)
library(sf)
library(terra)
library(curl)
library(maps)

# PART I - Get National Land Cover Data for central North Carolina
# 1. Set desired year (any year from 1985 to 2025)
yr <- 2025

# 2. Construct the MRLC download URL safely (avoids browser link-stripping)
host <- paste(c("www", "mrlc", "gov"), collapse = ".")
folder <- "downloads/sciweb1/shared/mrlc/data-bundles"
fname <- sprintf("Annual_NLCD_LndCov_%d_CU_C1V2", yr)
zip_url <- sprintf("https://%s/%s/%s.zip", host, folder, fname)

# Verify the URL looks like: https://mrlc.gov
print(zip_url)

# 3. Download the zip file (allow 10 minutes since CONUS zip is ~1.5 GB)
temp_zip <- tempfile(fileext = ".zip")
curl_download(zip_url, temp_zip, handle = new_handle(timeout = 600), quiet = FALSE)

# 4. Unzip the GeoTIFF and load with terra
tif_name <- paste0(fname, ".tif")
unzip(temp_zip, files = tif_name, exdir = tempdir())
nlcd_us <- rast(file.path(tempdir(), tif_name))

# 5. Crop and mask to North Carolina
nc_boundary <- states(cb = TRUE, resolution = "500k") |>
  subset(STUSPS == "NC")
nc_vect <- vect(st_transform(nc_boundary, crs(nlcd_us)))
nc_nlcd <- crop(nlcd_us, nc_vect, mask = TRUE)

# 6. Plot and save your local North Carolina file
plot(nc_nlcd, main = paste("North Carolina Land Cover", yr))
writeRaster(nc_nlcd, sprintf("NC_NLCD_%d.tif", yr), overwrite = TRUE)


# PART II - Crop the NLCD to an arbitrary lat-long bounding box (and project to geographic)
# 1. Define your exact target extent in WGS84 lat/long (xmin, xmax, ymin, ymax)
target_ext <- ext(-79.2, -78.6, 35.75, 36.15)

# 2. Add a small buffer (~0.1 degrees) before projecting to Albers so corners aren't clipped
padded_poly <- as.polygons(target_ext + 0.1, crs = "EPSG:4326")
padded_albers <- project(padded_poly, crs(nlcd_us))

# 3. Fast crop in native Albers projection (no mask needed yet)
nlcd_sub_albers <- crop(nlcd_us, padded_albers)

# 4. Reproject the small subset to WGS84 lat/long (EPSG:4326)
# CRITICAL: Always use method = "near" for categorical land cover integers!
nlcd_geo <- project(nlcd_sub_albers, "EPSG:4326", method = "near")

# 5. Crop cleanly to your exact geographic rectangle (zero NAs around the edges)
nlcd_rect <- crop(nlcd_geo, target_ext)

# 6. Verify it is an exact rectangle with EPSG:4326 coordinates
print(nlcd_rect)
plot(nlcd_rect, main = "NLCD 2025 (WGS84 Geographic Rectangle)")
writeRaster(nlcd_rect, "NC_BBox_WGS84_NLCD_2025.tif", overwrite = TRUE)


# PART III - Reclassify to 3 land cover classes: water, forest, non-forest
# 1. Create a reclassification matrix: [from, to, new_value]
# New classes: 1 = Water, 2 = Forest, 3 = Other
rcl_matrix <- matrix(c(
  11, 12, 1,   # Water -> 1
  21, 31, 3,   # Developed & Barren -> 3
  41, 43, 2,   # Deciduous, Evergreen, Mixed Forest -> 2
  51, 95, 3    # Shrub, Herbaceous, Agriculture, Wetlands -> 3
), ncol = 3, byrow = TRUE)

# 2. Reclassify the raster (others = NA preserves background NAs if any exist)
nlcd_3class <- classify(nlcd_rect, rcl = rcl_matrix, right = NA)

# 3. Attach human-readable category labels for the legend
levels(nlcd_3class) <- data.frame(
  value = c(1, 2, 3),
  cover = c("Water", "Forest", "Other")
)

# 4. Attach custom colors (Blue, Dark Green, Gray)
coltab(nlcd_3class) <- data.frame(
  value = c(1, 2, 3),
  col = c("#1f78b4", "forestgreen", "#bdbdbd") # Blue, Dark Green, Gray
)

# 5. Plot and save (color table and legend labels are preserved in the GeoTIFF!)
plot(nlcd_3class, main = "Land Cover: Water, Forest & Other (2025)")
writeRaster(nlcd_3class, "NC_Geographic_3Class_2025.tif", overwrite = TRUE)


# PART IV - Field site and city locations and attribures
# Field sites
sitemat = matrix(c(-79.0492, 35.9907,
          -79.0339, 35.8994,
          -79.0072, 36.0769,
          -79.0496, 35.9099,
          -78.7139, 35.8117), ncol = 2, byrow = TRUE)
          
sites = data.frame(name = c("Johnston Mill", "NC Botanical Garden",
                            "Eno River State Park", "UNC Chapel Hill",
                            "Prairie Ridge Ecostation"),
                   code = c("JOMI", "NCBG", "ERSP", "UNCH", "PREC"),
                   longitude = sitemat[,1], latitude = sitemat[,2],
                   xoffset = c(0, .035, 0, -0.035, 0),
                   yoffset = c(.015, -0.015, .015, -0.013, 0.015))

# Cities
cities_df <- data.frame(
  name = c("Chapel Hill", "Durham", "Raleigh"),
  lon  = c(-79.0558, -78.8986, -78.6382),
  lat  = c(35.9132,  35.9940,  35.7796)
)

cities_vect <- vect(cities_df, geom = c("lon", "lat"), crs = "EPSG:4326")


# PART V - Make map
png('figures/Figure2_map.png', height = 2400, width = 3200, res = 300)

par(cex = 1.3)

plot(nlcd_3class, legend = FALSE)
points(cities_vect, pch = 17, cex = 2)
text(cities_df$lon, cities_df$lat + 0.015, cities_df$name, cex = 1.25)

points(sites$longitude, sites$latitude, pch = 16, cex = 1.5, col = 'red')
text(sites$longitude + sites$xoffset, sites$latitude + sites$yoffset, sites$code, cex = 1.8)

# Add a 50 km scale bar in the bottom-left corner (over land in SC/NC border area)
sbar(
  d = 20,               # Length in kilometers (terra uses km by default for lon/lat)
  xy = "bottomleft" ,   # Exact lon/lat placement (or use "bottomleft")
  type = "bar",         # Alternates black and white segments
  divs = 4,             # Splits into 4 segments
  below = "km",         # Places "km" unit label underneath
  cex = 0.75,
  halo = TRUE           # White outline so text is readable over dark green/blue
)


# 2. Prepare Eastern US State Boundaries for the Inset
us_states_raw <-  st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
us_states <- terra::vect(us_states_raw)
crs(us_states) <- "EPSG:4326"

# Crop states to the Eastern US region around NC
east_us <- crop(us_states, ext(-85, -73, 30, 41))

# Isolate North Carolina to highlight it inside the inset
nc_region <- crop(us_states, ext(-84.6, -75, 33.4, 37.2))
nc_state <- east_us[east_us$ID == "north carolina", ]

# Convert your main raster's bounding box into a SpatVector polygon
study_box <- as.polygons(ext(nlcd_3class), crs = "EPSG:4326")

# 3. Draw the Inset in the Upper-Right Corner using par(fig)
op <- par(no.readonly = TRUE)

# [x1, x2, y1, y2] in 0-1 device coordinates (tweak x1/x2 slightly if near the legend)
par(fig = c(0.62, 0.88, 0.71, 0.92), new = TRUE, mar = c(0, 0, 0, 0))

# Draw white background rectangle first so the inset isn't transparent over forest/gray
plot(ext(nc_region), col = "white", border = NA, axes = FALSE, mar = 0)
plot(nc_region, col = "gray92", border = "gray60", add = TRUE)
plot(nc_state, col = "gray78", border = "gray25", lwd = 1.2, add = TRUE)
plot(study_box, border = "red", col = rgb(1, 0, 0, 0.25), lwd = 2, add = TRUE)
#box(col = "black", lwd = 1.2)

# Add a compact 200 km scale bar in the ocean area of the inset
sbar(
  d = 200,                # 200 km total length
  xy = c(-84, 34),    # Bottom-right ocean space inside nc_region
  type = "line",          # Simple clean line (less cluttered in small insets than "bar")
  label = c(0, "", 200),  # Label only the ends so text doesn't crowd
  below = "km",
  cex = 0.65,             # Smaller font size proportional to the inset
  lwd = 1.5
)

dev.off()
par(op) # Restore original graphics parameters
