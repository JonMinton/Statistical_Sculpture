rm(list=ls())

# Load packages -----------------------------------------------------------


require(tidyr)
require(stringr)
require(plyr)
require(dplyr)

require(r2stl)

require(rgl)
require(ggplot2)
require(lattice)
require(latticeExtra)




# Load Data ---------------------------------------------------------------


dta_hmd <- read.csv("data/tidy/hmd/lexis_square_combined.csv") %>% tbl_df
dta_hfd <- read.csv("data/tidy/hfd/lexis_square_combined.csv") %>% tbl_df

dta_hfd$code <- tolower(dta_hfd$code)


# Create germany (deut) based on counts from east and west germany --------


counts_germany <- dta_hmd  %>% filter(country %in% c("deute", "deutw"))

counts_p <- counts_germany %>%
  select(-death_count) %>%
  spread(key=country, value=population_count) %>%
  mutate(country= "deut", deut=deute + deutw) %>%
  select(country, year, age, sex, population_count = deut)

counts_d <- counts_germany %>%
  select(-population_count) %>%
  spread(key=country, value=death_count) %>%
  mutate(country= "deut", deut=deute + deutw) %>%
  select(country, year, age, sex, death_count = deut)


counts_deut <- counts_p %>%
  inner_join(counts_d)

dta_hmd <- dta_hmd  %>% bind_rows(counts_deut)

rm(counts_germany, counts_deut, counts_p, counts_d)

# hfd

counts_germany <- dta_hfd %>% filter(code %in% c("deute", "deutw"))

counts_total <- counts_germany %>%
  select(code, year, age, total) %>%
  spread(key=code, value= total) %>%
  mutate(code = "deut", deut=deute + deutw) %>%
  select(code, year, age, total=deut)

counts_exposure <- counts_germany %>%
  select(code, year, age, exposure) %>%
  spread(key=code, value= exposure) %>%
  mutate(code = "deut", deut=deute + deutw) %>%
  select(code, year, age, exposure=deut)


counts_deut <- counts_total %>%
  inner_join(counts_exposure) 

counts_deut <- counts_deut %>%
  mutate(
    asfr=total/exposure, 
    cpfr=NA
    ) %>%
  select(code, year, age, asfr, total, cpfr, exposure)


dta_hfd <- dta_hfd  %>% bind_rows(counts_deut)

rm(counts_deut, counts_total, counts_exposure)
# Individual Spooling -----------------------------------------------------

dir.create("stl/individual/populations", recursive=TRUE)
fn <- function(x){
  this_country <- x$country[1] 
  this_sex <- x$sex[1]
  year_min <- x$year %>% min
  year_max <- x$year %>% max
  
  dta <- x  %>% select(year, age, population_count) %>%
    mutate(population_count = population_count + 0.02 * max(population_count)) %>%
    spread(age, population_count)
  rownames(dta) <- dta$year
  dta$year <- NULL
  dta <- as.matrix(dta)
  
  r2stl(
    x=as.numeric(rownames(dta)),
    y=as.numeric(colnames(dta)), 
    z=dta,
    z.expand=TRUE, 
    show.persp=F,
    filename=paste0(
      "stl/individual/populations/", this_country, "_", this_sex, "_(",year_min, "-", year_max, ").stl"
      )
    )
    
}

dta_hmd %>%
  filter(age <=90 & sex !="total") %>%
  d_ply(., .(country, sex), fn, .progress="text")


# log death rate

dir.create("stl/individual/lmorts", recursive=TRUE)
fn <- function(x){
  this_country <- x$country[1] 
  this_sex <- x$sex[1]
  year_min <- x$year %>% min
  year_max <- x$year %>% max
  
  dta <- x  %>% mutate(
    death_rate=(death_count+0.5)/(population_count+0.5),
    ldeath_rate =log(death_rate))  %>%  
  select(year, age, ldeath_rate) %>%
    mutate(ldeath_rate = ldeath_rate + 0.02 * max(ldeath_rate)) %>%
    spread(age, ldeath_rate)
  rownames(dta) <- dta$year
  dta$year <- NULL
  dta <- as.matrix(dta)
  
  r2stl(
    x=as.numeric(rownames(dta)),
    y=as.numeric(colnames(dta)), 
    z=dta,
    z.expand=TRUE, 
    show.persp=F,
    filename=paste0(
      "stl/individual/lmorts/", this_country, "_", this_sex, "_(",year_min, "-", year_max, ").stl"
    )
  )
  
}

dta_hmd %>%
  filter(age <=90 & sex !="total") %>%
  d_ply(., .(country, sex), fn, .progress="text")


# fertility rate

dir.create("stl/individual/fertility", recursive=TRUE)
fn <- function(x){
  this_country <- x$code[1] 
  year_min <- x$year %>% min
  year_max <- x$year %>% max
  
  dta <- x  %>% 
    select(year, age, asfr) %>%
    mutate(asfr = asfr + 0.02 * max(asfr)) %>%
    spread(age, asfr)
  rownames(dta) <- dta$year
  dta$year <- NULL
  dta <- as.matrix(dta)
  
  r2stl(
    x=as.numeric(rownames(dta)),
    y=as.numeric(colnames(dta)), 
    z=dta,
    z.expand=TRUE, 
    show.persp=F,
    filename=paste0(
      "stl/individual/fertility/", this_country, "_(",year_min, "-", year_max, ").stl"
    )
  )
  
}

d_ply(dta_hfd, .(code), fn, .progress="text")


# Grouped Spooling --------------------------------------------------------

dir.create("stl/groups/populations", recursive=TRUE)
fn <- function(x){
  this_country <- x$country[1] 
  year_min <- x$year %>% min
  year_max <- x$year %>% max
  
  dta_m <- x  %>% 
    filter(sex=="male") %>%
    select(year, age, population_count) %>%
    mutate(population_count = population_count + 0.02 * max(population_count)) %>%
    spread(age, population_count)
  rownames(dta_m) <- dta_m$year
  dta_m$year <- NULL
  dta_m <- as.matrix(dta_m)
  
  dta_f <- x  %>% 
    filter(sex=="female") %>%
      select(year, age, population_count) %>%
      mutate(population_count = population_count + 0.02 * max(population_count)) %>%
      spread(age, population_count)
    rownames(dta_f) <- dta_f$year
    dta_f$year <- NULL
    dta_f <- as.matrix(dta_f)

  n_row <- dim(dta_f)[1]
  n_col <- dim(dta_f)[2]
  
  dta_j <- matrix(
    data=max(data=x$population_count) * 0.015,
    nrow= 2 * n_row + 7, 
    ncol=     n_col + 4
  )
  
  dta_j[
    2+ 1:n_row,
    2+ 1:n_col
    ] <- dta_f
  
  dta_j[
    5 + n_row + 1:n_row,
    2 + 1:n_col
    ] <- dta_m
  
  r2stl(
    x=1:nrow(dta_j),
    y=1:ncol(dta_j), 
    z=dta_j,
    z.expand=TRUE, 
    show.persp=F,
    filename=paste0(
      "stl/groups/populations/", this_country, "_(",year_min, "-", year_max, ").stl"
    )
  )
  
}

dta_hmd %>%
  filter(age <= 90 & sex !="total") %>%
  d_ply(., .(country), fn, .progress="text")




dir.create("stl/groups/lmorts", recursive=TRUE)
fn <- function(x){
  this_country <- x$country[1] 
  year_min <- x$year %>% min
  year_max <- x$year %>% max
  
  x <- x %>%
    mutate(death_rate=(death_count+0.5) / (population_count+0.5),
           ldeath_rate=log(death_rate),
           ldeath_rate=ldeath_rate - min(ldeath_rate),
           ldeath_rate=ldeath_rate + 0.02 * max(ldeath_rate)
           )
  
  dta_m <- x  %>% 
    filter(sex=="male") %>%
    select(year, age, ldeath_rate) %>%
    spread(age, ldeath_rate)
  
  rownames(dta_m) <- dta_m$year
  dta_m$year <- NULL
  dta_m <- as.matrix(dta_m)
  
  dta_f <- x  %>% 
    filter(sex=="female") %>%
    select(year, age, ldeath_rate) %>%
    spread(age, ldeath_rate)
  
  rownames(dta_f) <- dta_f$year
  dta_f$year <- NULL
  dta_f <- as.matrix(dta_f)
  
  n_row <- dim(dta_f)[1]
  n_col <- dim(dta_f)[2]
  
  dta_j <- matrix(
    data=max(data=x$ldeath_rate) * 0.015,
    nrow= 2 * n_row + 7, 
    ncol=     n_col + 4
  )
  
  dta_j[
    2+ 1:n_row,
    2+ 1:n_col
    ] <- dta_m
  
  dta_j[
    5 + n_row + 1:n_row,
    2 + 1:n_col
    ] <- dta_f
  
  r2stl(
    x=1:nrow(dta_j),
    y=1:ncol(dta_j), 
    z=dta_j,
    z.expand=TRUE, 
    show.persp=F,
    filename=paste0(
      "stl/groups/lmorts/", this_country, "_(",year_min, "-", year_max, ").stl"
    )
  )
  
}

dta_hmd %>%
  filter(age <= 90 & sex !="total") %>%
  d_ply(., .(country), fn, .progress="text")





