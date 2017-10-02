# Perozzo replication 

rm(list = ls())

require(magrittr)
require(tidyverse)
require(r2stl)

read_csv("data/tidy/hmd/lexis_square_combined.csv") %>% 
  filter(country == "swe") %>% 
  filter(year <= 1875) %>% 
  filter(sex == "total") %>% 
  filter(age <= 100) %>% 
  select(year, age, population_count) -> swe_dta

swe_dta %>% 
  spread(year, population_count) -> tmp

ages <- tmp %>% pull(age)
years <- tmp %>% select(-age) %>% names() %>% as.numeric()

tmp %>% 
  select(-age) %>% 
  as.matrix() -> swe_mat

colnames(swe_mat)
rownames(swe_mat) 
rownames(swe_mat) <- ages

swe_mat %>% 
  r2stl::r2stl(
    x=as.numeric(rownames(.)),
    y=as.numeric(colnames(.)), 
    z=.,
    z.expand=TRUE, 
    show.persp=T,
    filename=paste0(
      "stl/perrozo_replication.stl"
    )
  )




  
