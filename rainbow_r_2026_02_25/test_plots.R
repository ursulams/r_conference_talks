library(tidyverse)
library(sf)
library(elevatr)
library(raster)
library(rayshader)
library(magick)

nyc_border <- st_read("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Borough_Boundary_Water_Included/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson")

# add topography
elev <- get_elev_raster(nyc_border, z = 9)

# conform raster to city borders
elev_mask <- mask(elev, nyc_border)
nyc_matrix <- raster_to_matrix(elev_mask)

# add max water level
# add inundation zone
inundation_border <- st_read("https://data.cityofnewyork.us/resource/5xsi-dfpx.geojson")
inundation <- get_elev_raster(inundation_border, z = 9)
inundation_mask <- mask(inundation, inundation_border)
inundation_matrix <- raster_to_matrix(inundation_mask)

# add waterlines
# regular
nyc_water <- detect_water(nyc_matrix, zscale = 9)
# inundation



# animate flood (see https://www.tylermw.com/posts/data_visualization/3d-maps-with-rayshader.html)
n_frames <- 180
waterdepths <- min(inundation_matrix)/2 - min(nyc_matrix)/2 * cos(seq(0,2*pi,length.out = n_frames))
zscale <- 50
img_frames <- paste0("drain", seq_len(n_frames), ".png")

for (i in seq_len(n_frames)) {
  message(paste(" - image", i, "of", n_frames))
  nyc_matrix |> 
    sphere_shade(texture = "imhof4") |> 
    add_shadow(ray_shade(nyc_matrix)) |> 
    add_shadow(ambient_shade(nyc_matrix)) |>
    add_water(watermap, color = "imhof4") |>
    plot_3d(heightmap = nyc_matrix, solid = FALSE, shadow = TRUE, zscale = zscale, 
            water = TRUE, watercolor = "imhof3", wateralpha = 0.8, 
            waterlinecolor = "lightblue", waterlinealpha = 0.5,
            waterdepth = waterdepthvalues[i]/zscale, 
            theta = 5, phi = 90)
  render_snapshot(img_frames[i])
  rgl::clear3d()
}

image_write_gif(magick::image_read(img_frames), 
                path = "sandy_inundation.gif", 
                delay = 6/n_frames)





# point elevations
# elevpoints <- RSocrata::read.socrata("https://data.cityofnewyork.us/resource/9uxf-ng6q.json", 
#                            app_token = Sys.getenv("SOCRATA_KEY")) |> 
#     mutate(geometry = Map(function(x) st_point(x[1:2]), the_geom.coordinates),
#            elevation = round(as.numeric(elevation), 1)) |> 
#   st_as_sf()
# 
# 
# g <- ggplot(data = elevpoints, aes(geometry = geometry)) +
#   geom_sf(aes(color = `elevation`), 
#           size = 0.3) +
#   scale_color_continuous(palette = "Spectral") +
#   theme_void() +
#   theme(plot.title = element_text(color = "#355070", size = 14, hjust = 0.25),
#         legend.title = element_blank(),
#         legend.margin = margin(5, 5, 5, 5, "pt"))
# g
