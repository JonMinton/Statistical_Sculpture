# temp script for dta_hmd extracting by categories


tmp <- dta_hmd %>% 
  filter(country %in% c("gbr_sco", "gbrcenw")) %>% 
  filter(year == 2011) %>% 
  filter(sex != "total") %>% 
  group_by(year, age, sex) %>% 
  summarise(
    population_count = sum(population_count, na.rm=T),
    death_count = sum(death_count, na.rm=T)
  ) %>% 
  mutate(
    age_group_male  = cut(
      x = age,
      breaks = c(0, 19, 29, 39, 49, 59, 64, 110),
      include.lowest = T
    ),
    age_group_female  = cut(
      x = age,
      breaks = c(0, 19, 29, 39, 49, 59, 110),
      include.lowest = T
    )
  ) %>% 
  ungroup()



tmp %>% 
  filter(sex == "male") %>% 
  group_by(age_group_male) %>% 
  summarise(population_count = sum(population_count)) %>% 
  write.csv("clipboard")

tmp %>% 
  filter(sex == "female") %>% 
  group_by(age_group_female) %>% 
  summarise(population_count = sum(population_count)) %>% 
  write.csv("clipboard")





