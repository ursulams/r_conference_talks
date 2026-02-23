options(rgl.useNULL = TRUE, rgl.printRglwidget = TRUE, software_render = TRUE) #X11 doesnt run on macos tahoe
library(tidyverse)
library(sf)
library(OpenStreetMap)
library(elevatr)
library(raster)
library(rgl)
library(rayshader)

#nyc_border <- st_read("https://data.cityofnewyork.us/resource/wh2p-dxnf.geojson")
#st_write(nyc_border, "nyc_border.geojson")
nyc_border <- st_read("nyc_border.geojson")

# superstorm sandy inundation: https://data.cityofnewyork.us/Environment/Sandy-Inundation-Zone/uyj8-7rv5
#inundation_border <- st_read("https://data.cityofnewyork.us/resource/5xsi-dfpx.geojson")
#st_write(inundation_border, "inundation_border.geojson")
inundation_border <- st_read("inundation_border.geojson")


# all of nyc
# add topography limited to borders of city and inundation zone
create_topo <- function(st, bbox, z_value) {
  elev <- get_elev_raster(st, z = z_value) 
  elev_masked <- mask(elev, bbox) # remove NAs to reduce matrix dims
  st_matrix <- raster_to_matrix(elev_masked)
  return(st_matrix)
}

nyc_matrix <- create_topo(nyc_border, nyc_border, 10)
inundation_matrix <- create_topo(inundation_border, inundation_border, 10)


# point elevations
# elevpoints <- RSocrata::read.socrata("https://data.cityofnewyork.us/resource/9uxf-ng6q.json", 
#                            app_token = Sys.getenv("SOCRATA_KEY")) |> 
#     mutate(geometry = Map(function(x) st_point(x[1:2]), the_geom.coordinates),
#            elevation = round(as.numeric(elevation), 1)) |> 
#   st_as_sf()
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


# https://www.tylermw.com/posts/data_visualization/3d-maps-with-rayshader.html

zscale <- 10
n_frames <- 150
inundation_waterdepths <-  max(inundation_matrix, na.rm = TRUE)/2 - max(inundation_matrix, na.rm = TRUE)/2 * cos(seq(0,2*pi,length.out = n_frames))
img_frames <- paste0("surge", seq_len(n_frames), ".png")

for (i in seq_len(n_frames)) {
  message(paste(" - image", i, "of", n_frames))
  nyc_matrix |>
    sphere_shade(texture = create_texture("#b7f4f5", "#4e3e42","#8a9299","#195f60","#c2d1ce"), zscale = zscale) |>
    add_water(detect_water(nyc_matrix, cutoff = 1.3)) |>
    add_shadow(ray_shade(nyc_matrix, zscale = zscale)) |>
    plot_3d(heightmap = nyc_matrix,
            zscale = zscale,
            solid = FALSE,
            shadow = TRUE,
            water = TRUE,
            watercolor =  "#3289a0",
            theta = -5,
            phi = 65,
            zoom = 0.4,
            windowsize = c(1200, 900)
            )
   render_water(inundation_matrix, waterdepth = inundation_waterdepths[i]/zscale,
               watercolor ="#3289a0", zscale = zscale, remove_water = FALSE)
  render_snapshot(img_frames[i])
  rgl::clear3d()
}


magick::image_write_gif(magick::image_read(img_frames), 
                        path = "nyc_inundation_2.gif", 
                        delay = 5/n_frames)

file.remove(list.files(pattern = "surge"))



# riis beach
create_topo <- function(st, bbox, z_value) {
  elev <- get_elev_raster(st, z = z_value) 
  elev <- crop(elev, bbox)
  elev_masked <- mask(elev, bbox) # remove NAs to reduce matrix dims
  st_matrix <- raster_to_matrix(elev_masked)
  return(st_matrix)
}

riis_coords <- c(-73.893936,40.557853,-73.857586,40.578100)
names(riis_coords) = c("xmin", "ymin", "xmax", "ymax")
riis_bbox <- st_bbox(riis_coords)
riis_polygon <- st_as_sf(st_as_sfc(riis_bbox), crs = 4326)
riis_matrix <- create_topo(nyc_border, riis_polygon, 14)
riis_inundation_matrix <- create_topo(inundation_border, riis_polygon, 14)

n_frames <- 75
riis_inundation_waterdepths <-  max(riis_inundation_matrix, na.rm = TRUE)/2 - max(riis_inundation_matrix, na.rm = TRUE)/2 * cos(seq(0,2*pi,length.out = n_frames))

for (i in seq_len(n_frames)) {
  message(paste(" - image", i, "of", n_frames))
  
  riis_matrix |> 
    sphere_shade(texture = "imhof3", zscale = zscale) |> 
    add_water(detect_water(riis_matrix, cutoff = 1.3)) |>
    add_shadow(ray_shade(riis_matrix, zscale = zscale)) |> 
    plot_3d(heightmap = riis_matrix, 
            zscale = zscale, 
            solid = FALSE,
            shadow = FALSE,
            water = TRUE,
            watercolor =  "#3289a0",
            theta = -5, 
            phi = 65
    )
render_water(inundation_matrix, waterdepth = inundation_waterdepths[i]/zscale,  
    watercolor ="#3289a0", zscale = zscale, remove_water = FALSE)
render_snapshot(img_frames[i])
rgl::clear3d()
}

magick::image_write_gif(magick::image_read(img_frames), 
                       path = "riis_inundation.gif", 
                       delay = 5/n_frames)

file.remove(list.files(pattern = "surge"))


