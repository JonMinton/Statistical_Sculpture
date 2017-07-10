rm(list = ls())

pacman::p_load(
  tidyverse,
  readxl,
  r2stl
)

dta <- read_excel(path = "data/ulrich/Berlin 2005-30.xlsx", skip = 3, col_names = F)

yrs <- rep(2005:2030, each = 2)
sx <- c("male", "female")

nms <- paste(sx, yrs, sep = "_")
nms <- c("age", nms)
names(dta) <- nms

dta_tidied <- dta %>% 
  gather(key = "s_y", value = "count", -age) %>% 
  separate(col = "s_y", sep = "_", into = c("sex", "year")) %>% 
  mutate(age = as.numeric(age), year = as.numeric(year)) %>% 
  select(age, year, sex, count)

dta_male <- dta_tidied %>% 
  filter(sex == "male") %>%
  select(age, year, count)

dta_female <- dta_tidied %>% 
  filter(sex == "female") %>%
  select(age, year, count)

dta_male %>% 
  spread(year, count) -> tmp
ags <- tmp$age
yrs <- names(tmp)[-1]
tmp[-1] %>% 
  as.matrix -> tmp
rownames(tmp) <- ags

r2stl(
  x = as.numeric(rownames(tmp)),
  y = as.numeric(colnames(tmp)),
  z = tmp,
  z.expand=TRUE, 
  show.persp=T,
  filename= "stl/ulrich/pop_proj_males.stl"
)

dta_female %>% 
  spread(year, count) -> tmp
ags <- tmp$age
yrs <- names(tmp)[-1]
tmp[-1] %>% 
  as.matrix -> tmp
rownames(tmp) <- ags

r2stl(
  x = as.numeric(rownames(tmp)),
  y = as.numeric(colnames(tmp)),
  z = tmp,
  z.expand=TRUE, 
  show.persp=T,
  filename= "stl/ulrich/pop_proj_females.stl"
)
