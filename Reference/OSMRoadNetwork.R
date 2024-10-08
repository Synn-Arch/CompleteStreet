library(sf)
library(osmdata)
library(sfnetworks)
library(dplyr)
library(tidygraph)
library(magrittr)
library(tmap)

######### (1) Road networks 받을 대상지 지정하기 #########
### In this case, 하나의 point (lat, lon)를 입력한 다음 boundary box를 설정함
## point 입력
point <- st_point(c(-84.36312, 33.85015))
point_sf <- st_sfc(point, crs = 4326)

## boundary box 그리기 (거리 단위는 미터)
bbox_x <- st_buffer(st_transform(point_sf, crs = 3857), dist = 2000) %>% 
  # st_transform(crs = 3857) 함수를 이용한 이유는 미터를 이용하기 위함.
  # 좌표계마다 취급하는 단위가 다름. 예로 3857은 미터에 최적화되어있는 반면,
  # 위/경도를 이용하는 4326은 degree 개념을 사용
  st_transform(crs = 4326) %>%
  # boundary box를 그린 다음 다시 좌표계를 4326으로 맞춤
  st_bbox()
bbox_x.sf <- st_as_sfc(bbox_x)

######### (2) Road networks 받기 #########
## 따로 osm 관련 패키지 내용 공부하시면 훨씬 도움되겠지만 일단 아래 코드로도 충분히 도로를 따올 수 있습니다
osm_road <- opq(bbox = bbox_x) %>%
  add_osm_feature(key = 'highway',
                  value = c("motorway", "trunk", "primary",
                            "secondary", "tertiary",
                            "residential")) %>%
  osmdata_sf() %>%
  osm_poly2line()

osm_road <- osm_road$osm_lines[bbox_x.sf,] %>% select(osm_id, highway)
net <- sfnetworks::as_sfnetwork(osm_road, directed = FALSE)
simple_net <- net %>%
  activate("edges") %>%
  filter(!edge_is_multiple()) %>%
  filter(!edge_is_loop())
subdiv_net <- convert(simple_net, sfnetworks::to_spatial_subdivision)
subdiv_net <- subdiv_net %>%
  activate("nodes") %>%
  mutate(custom_id = seq(1, subdiv_net %>% st_as_sf("nodes") %>% nrow()),
         is.new = case_when(is.na(.tidygraph_node_index) ~ "new nodes",
                            TRUE ~ "existing nodes"),
         is.new = factor(is.new))
smoothed_net <- convert(subdiv_net, sfnetworks::to_spatial_smooth)

filtered_roads <- smoothed_net %>%
  activate("edges") %>%
  st_as_sf()



### Visualize the road networks in Open Street Map
tmap_mode("view")  # Switch to interactive mode

tm_shape(filtered_roads) + 
  tm_lines(col = "blue", lwd = 2) +  # Visualize roads
  tm_basemap("OpenStreetMap")  # Add basemap for context
