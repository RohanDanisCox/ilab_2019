# Working with Mesh Blocks - this information could be useful for green spaces

mesh_blocks <- read_csv("Data/ABS/Mesh Blocks Category.csv")
mesh_blocks1 <- read_csv("Data/ABS/Mesh blocks to State Suburb.csv")

mesh_blocks2 <- mesh_blocks %>%
  left_join(mesh_blocks1, by = c("MB_CODE_2016", "STATE_CODE_2016", "STATE_NAME_2016", "AREA_ALBERS_SQKM")) %>%
  select(MB_CODE_2016,MB_CATEGORY_NAME_2016,SSC_NAME_2016,AREA_ALBERS_SQKM)

mesh_blocks3 <- mesh_blocks2 %>%
  group_by(SSC_NAME_2016, MB_CATEGORY_NAME_2016) %>%
  summarise(area = sum(AREA_ALBERS_SQKM)) 

mesh_blocks4 <- mesh_blocks3 %>%
  group_by(SSC_NAME_2016) %>%
  mutate(total_area = sum(area)) %>%
  mutate(proportion = round((area/total_area),4))
  

haberfield <- mesh_blocks4 %>%
  filter(SSC_NAME_2016 == "Haberfield")
