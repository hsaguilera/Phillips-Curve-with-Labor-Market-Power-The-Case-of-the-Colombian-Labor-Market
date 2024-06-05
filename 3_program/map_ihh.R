library(sf)
library(ggplot2)

map <- st_read("C:/Investigation/wage phillips curve with labor market power/2_data/map.geojson")
map_for <- st_read("C:/Investigation/wage phillips curve with labor market power/2_data/map_for.geojson")
map_inf <- st_read("C:/Investigation/wage phillips curve with labor market power/2_data/map_inf.geojson")


# Labor Market
map_ihh = ggplot() +
  geom_sf(data = map, aes(fill = ihh)) +
  scale_fill_gradient(low = "dodgerblue3", high = "#9932CC", name = "Índice Herfindahl Hirschman (IHH)") +  
  theme_minimal() +
  theme(axis.title = element_blank(),
        plot.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),   
        panel.grid = element_blank())
ggsave("C:/Investigation/wage phillips curve with labor market power/4_results/graph/map_ihh.png", plot = map_ihh, width = 10, height = 8, dpi = 600)


# Formal Labor Market
map_ihh_for = ggplot() +
  geom_sf(data = map_for, aes(fill = ihh)) +
  scale_fill_gradient(low = "dodgerblue3", high = "#9932CC", name = "Índice Herfindahl Hirschman (IHH)") +  
  theme_minimal() +
  theme(axis.title = element_blank(),
        plot.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),   
        panel.grid = element_blank())
ggsave("C:/Investigation/wage phillips curve with labor market power/4_results/graph/map_ihh_for.png", plot = map_ihh_for, width = 10, height = 8, dpi = 600)


# Informal Labor Market
map_ihh_inf = ggplot() +
  geom_sf(data = map_inf, aes(fill = ihh)) +
  scale_fill_gradient(low = "dodgerblue3", high = "#9932CC", name = "Índice Herfindahl Hirschman (IHH)") +  
  theme_minimal() +
  theme(axis.title = element_blank(),
        plot.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),   
        panel.grid = element_blank())
ggsave("C:/Investigation/wage phillips curve with labor market power/4_results/graph/map_ihh_inf.png", plot = map_ihh_inf, width = 10, height = 8, dpi = 600)
