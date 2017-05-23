# This script generates some dummy data for agents who have visited a certain location

library(tidyverse)

gen_points <- function(rect_coords, n, n_agents, start_id){
  # generates n points within the rectangle
  # for n_agents unique agents starting with start_id
  
  x <- sample(c(rect_coords[1]:rect_coords[2]), n, replace=T)
  y <- sample(c(rect_coords[3]:rect_coords[4]), n, replace=T)
  df <- data.frame(lat= x/1000, lon= y/1000) %>% 
    cbind(agent_id = seq(start_id, length.out= n_agents)) %>% 
    arrange(agent_id)
  
  return(df)
}

set.seed(2102)
# define a rectangle of activity; coordinates in integer
north <- c(51000, 54000, 6500, 14000) # roughly Northern Germany
south <- c(47500, 51000, 6500, 14000) # roughly Southern Germany

visited_north <- gen_points(north, 1000, 200, 1)
visited_south <- gen_points(south, 1000, 200, 201)

visited_points <- rbind(visited_north, visited_south)
write.csv(visited_points, "dummy_geo_data.csv", row.names = F)


