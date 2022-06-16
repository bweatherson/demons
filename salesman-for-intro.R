require(tidyverse)
require(TSP)
require(maps)

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

data("USCA312")
data("USCA312_GPS")

cities <- as_tibble(as.matrix(USCA312))

city_numbers <- tibble(
  id = 1:312,
  thecities = colnames(cities)
) %>% 
  mutate(used_city = case_when(str_sub(thecities, -2) %in% short_states  ~ 1,
                               TRUE ~ 0))

the_city_numbers <- filter(city_numbers, used_city == 1)$id


our_cities <- cities %>% 
  select(all_of(the_city_numbers)) %>% 
  slice(the_city_numbers)

our_gps <- USCA312_GPS %>% 
  slice(the_city_numbers) %>% 
  rowid_to_column()

city_matrix <- as.matrix(our_cities)

rownames(city_matrix) <- filter(city_numbers, used_city == 1)$thecities

## Fine tour
#tour_line <- solve_TSP(as.TSP(city_matrix), method="farthest_insertion")
#tour_line <- solve_TSP(as.TSP(city_matrix), method="two_opt", tour = tour_line)

## Not good tour
#tour_line <- solve_TSP(as.TSP(city_matrix), method="cheapest_insertion", start = 17) # - Very messy

## Generate tour by longitude - really bad
#tour_line <- TOUR(arrange(our_gps, long)$rowid, tsp = as.TSP(city_matrix))

## Best tour
load("tour_line.RData")

## Turn tour to map path
# paths <- tribble(
#   ~step, ~property, ~rowid, ~long, ~lat
# )
# 
# for (i in 1:nrow(our_gps)){
#   x <- tour_line[i]
#   first_city <- our_gps %>% slice(x)
#   next_city <- our_gps %>% slice(x %% 31)
#   paths <- paths %>% 
#     add_row(step = i, property = "from", rowid = first_city$rowid[1], long = first_city$long[1], lat = first_city$lat[1])# %>% 
#   #    add_row(step = i, property = "to", rowid = next_city$rowid[1], long = next_city$long[1], lat = next_city$lat[1])
# }
# 
# x <- tour_line[1]
# 
# paths <- paths %>% add_row(step = 24, property = "from", rowid = our_gps$rowid[x], long = our_gps$long[x], lat = our_gps$lat[x])


state_map_data <- map_data("state") %>%
  #  filter(subregion != "north" | is.na(subregion)) %>%
  filter(region %in% long_states) 

tour_map <- ggplot(state_map_data, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "grey90") + 
  geom_point(data = our_gps %>% select(long, lat), aes(x = long, y = lat), size = 0.25, inherit.aes = FALSE) +
#  geom_path(data = paths %>% select(long, lat), aes(x = long, y = lat), inherit.aes = FALSE, colour = "grey30", alpha = 0.5 ) + 
  coord_quickmap()
#tour_length(tour_line)
tour_map

#str_c(our_gps$name, sep = "; ", collapse = "; ")
