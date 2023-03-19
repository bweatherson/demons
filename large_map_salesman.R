require(tidyverse)
require(TSP)
require(maps)
require(geosphere)

st_crosswalk <- tibble(state = state.name) %>%
  bind_cols(tibble(abb = state.abb)) %>% 
  bind_rows(tibble(state = "District of Columbia", abb = "DC"))

uscities <- read_csv("geo_coord/uscities.csv")

theme_map <- function(base_size=9, base_family="") {
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
    )
}

theme_set(theme_map())

all_states <- map_data("state") %>% 
  group_by(region) %>% 
  tally() %>% 
  select(state = region)

all_states$code <- c("AL", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA",
                     "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", 
                     "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", 
                     "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
                     "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

used_states <- 1:49

long_states <- all_states$state[used_states]
short_states <- all_states$code[used_states]

used_cities <- uscities |> 
  filter(source == "shape") |> 
  select(city, state_id, state_name, lat, lng, population) |> 
  filter(!state_id %in% c("HI", "AK", "PR")) |> 
  group_by(state_id) |>
  slice(1:40) |> 
  mutate(code = paste0(city, ", ", state_id)) |>
  ungroup() |>
  rowid_to_column()

# city_lengths <- as_tibble(matrix(nrow = length(used_cities$code), 
#                                  ncol = length(used_cities$code)), 
#                                 .name_repair = ~ used_cities$code)
# 
# for (i in 1:length(used_cities$code)){
#   for(j in 1:length(used_cities$code)){
#     city_lengths[[i, j]] <- as.integer(
#       distHaversine(
#         c(used_cities$lng[i], used_cities$lat[i]),
#         c(used_cities$lng[j], used_cities$lat[j])
#       )
#     )
#   }
# }

load("city_lengths.RData")

city_lengths <- as.matrix(city_lengths)

rownames(city_lengths) <- used_cities$code

tour_line <- solve_TSP(as.TSP(city_lengths), method = "nearest_insertion", start=739)
tour_line <- solve_TSP(as.TSP(city_lengths), method = "two_opt", tour = tour_line)

paths <- tribble(
  ~step, ~property, ~rowid, ~lng, ~lat
)

for (i in 1:nrow(used_cities)){
  x <- tour_line[i]
  first_city <- used_cities %>% slice(x)
  paths <- paths %>%
    add_row(step = i, 
            property = "from", 
            rowid = first_city$rowid[1], 
            lng = first_city$lng[1], 
            lat = first_city$lat[1])
}

state_map_data <- map_data("state") |>
  filter(region %in% long_states) 

tour_map <- ggplot(state_map_data, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "grey90") + 
  geom_point(data = used_cities |>
               select(lng, lat),
             aes(x = lng, y = lat), 
             size = 0.1, 
             inherit.aes = FALSE,
             alpha=0.3) +
  geom_path(data = paths |> select(lng, lat),
            aes(x = lng, y = lat),
            inherit.aes = FALSE,
            colour = "grey30",
            alpha = 0.3 ) +
  coord_quickmap() +
  labs(x = paste0("Tour length: ", tour_length(tour_line), " metres.")) +
  theme(axis.title.x = element_text())

tour_map

