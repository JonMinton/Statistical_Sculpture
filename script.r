rm(list=ls())

# Load packages -----------------------------------------------------------


require(tidyr)
require(stringr)
require(plyr)
require(dplyr)

require(r2stl)

require(rgl)
require(ggplot2)


# Load Data ---------------------------------------------------------------


dta_hmd <- read.csv("data/tidy/hmd/lexis_square_combined.csv") %>% tbl_df
dta_hfd <- read.csv("data/tidy/hfd/lexis_square_combined.csv") %>% tbl_df

dta_hfd$code <- tolower(dta_hfd$code)

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
    dta_f <- apply(dta_f, 2, rev)

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
  dta_f <- apply(dta_f, 2, rev)
  
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



#########################################################################################
#### Mortality rates: log and identity scale
#########################################################################################

#
fn <- function(x){
  ages <- x$age
  x$age <- NULL
  x <- as.matrix(x)
  rownames(x) <- ages
  return(x)
}

# automated:
stl_spooler <- function(x, min_age=0, max_age = 80){
  x <- subset(x, age >= min_age & age <= max_age)
  x <- mutate(
    x, 
    death_rate = (death_count + 0.5)/ (population_count + 0.5), # CORRECTION ADDED 
    ldeath_rate = log(death_rate)
  )
  
  x_rate <- recast(
    subset(x, select=c("year" ,"age" ,"death_rate")),
    age ~ year,
    id.var=c("age", "year"),
    measure="death_rate"
    )
  
  x_lrate <- recast(
    subset(x, select=c("year", "age", "ldeath_rate")), 
    age ~ year, 
    id.var=c("age", "year"), 
    measure="ldeath_rate"
  )
  
  x_rate <- fn(x_rate)
  x_lrate <- fn(x_lrate)
  
  r2stl(
    x=as.numeric(rownames(x_rate)),
    y=as.numeric(colnames(x_rate)),
    z=x_rate,
    
    filename=paste0(
      "stl/identity/death_rate_",
      x$country[1], "_",
      x$sex[1], ".stl"
      ),
    z.expand=T,
    show.persp=F
  )

  r2stl(
    x=as.numeric(rownames(x_lrate)),
    y=as.numeric(colnames(x_lrate)),
    z=x_lrate,
    
    filename=paste0(
      "stl/log/ldeath_rate_",
      x$country[1], "_",
      x$sex[1], ".stl"
    ),
    z.expand=T,
    show.persp=F
  )
  
}


d_ply(
  counts, 
  .(country, sex),
  stl_spooler,
  .progress="text"
  )



####################################################################################
### EAST AND WEST GERMANY ONLY #####################################################
####################################################################################

#DEUTE - east germany
#DEUTW - west germany

# Mortality rates;
# males, females
# east, west
# logged

east_germany <- subset(counts, subset=country=="DEUTE" & age <=80)
west_germany <- subset(counts, subset=country=="DEUTW" & age <=80)


east_germany <- mutate(
  east_germany, 
  death_rate = death_count / population_count,
  ldeath_rate = log(death_rate)
)

west_germany <- mutate(
  west_germany, 
  death_rate = death_count / population_count,
  ldeath_rate = log(death_rate)
)

east_f_matrix <- recast(
  subset(east_germany, subset=sex=="female", select=c("year", "age", "ldeath_rate")), 
  age ~ year, 
  id.var=c("age", "year"), 
  measure="ldeath_rate"
)

east_m_matrix <- recast(
  subset(east_germany, subset=sex=="male", select=c("year", "age", "ldeath_rate")), 
  age ~ year, 
  id.var=c("age", "year"), 
  measure="ldeath_rate"
)

west_f_matrix <- recast(
  subset(west_germany, subset=sex=="female", select=c("year", "age", "ldeath_rate")), 
  age ~ year, 
  id.var=c("age", "year"), 
  measure="ldeath_rate"
)

west_m_matrix <- recast(
  subset(west_germany, subset=sex=="male", select=c("year", "age", "ldeath_rate")), 
  age ~ year, 
  id.var=c("age", "year"), 
  measure="ldeath_rate"
)


fn <- function(x){
  ages <- x$age
  x$age <- NULL
  x <- as.matrix(x)
  x - min(x)
  rownames(x) <- ages
  return(x)
}


east_f_matrix <- fn(east_f_matrix)
east_m_matrix <- fn(east_m_matrix)
west_f_matrix <- fn(west_f_matrix)
west_m_matrix <- fn(west_m_matrix)



r2stl(
  x=as.numeric(rownames(east_f_matrix)),
  y=as.numeric(colnames(east_f_matrix)),
  z=east_f_matrix,
  
  filename="stl/germany/east_f.stl",
  z.expand=T,
  show.persp=F
)

r2stl(
  x=as.numeric(rownames(east_m_matrix)),
  y=as.numeric(colnames(east_m_matrix)),
  z=east_m_matrix,
  
  filename="stl/germany/east_m.stl",
  z.expand=T,
  show.persp=T
)

r2stl(
  x=as.numeric(rownames(east_f_matrix)),
  y=as.numeric(colnames(east_f_matrix)),
  z=east_f_matrix,
  
  filename="stl/germany/west_f.stl",
  z.expand=T,
  show.persp=F
)

r2stl(
  x=as.numeric(rownames(east_m_matrix)),
  y=as.numeric(colnames(east_m_matrix)),
  z=east_m_matrix,
  
  filename="stl/germany/west_m.stl",
  z.expand=T,
  show.persp=T
)




##############################################################################################################
# Population counts:

#########################################################################################
#########################################################################################
## To Automate the process ##############################################################


#########################################################################################
#### Mortality rates: log and identity scale
#########################################################################################

#
fn <- function(x){
  ages <- x$age
  x$age <- NULL
  x <- as.matrix(x)
  x - min(x)
  rownames(x) <- ages
  return(x)
}

# automated:
stl_spooler <- function(x, min_age=0, max_age = 100){
  x <- subset(x, age >= min_age & age <= max_age)
  
  x_rate <- recast(
    subset(x, select=c("year" ,"age" ,"population_count")),
    age ~ year,
    id.var=c("age", "year"),
    measure="population_count"
  )
  
  
  x_rate <- fn(x_rate)
  
  r2stl(
    x=as.numeric(rownames(x_rate)),
    y=as.numeric(colnames(x_rate)),
    z=x_rate,
    
    filename=paste0(
      "stl/population/population_",
      x$country[1], "_",
      x$sex[1], ".stl"
    ),
    z.expand=T,
    show.persp=F
  )
  
  
}


d_ply(
  counts, 
  .(country, sex),
  stl_spooler,
  .progress="text"
)

dir.create("stl/hfd/", recursive=T)
fn <- function(x){
  
  this_country <- x$country[1]
  
  mtrx <- x %>%
    select(year, age, asfr) %>%
    mutate(asfr=asfr+0.02 * max(asfr)) %>% # Add 2% to ASFR values as a minimum height is needed
    spread(key=age, value=asfr) 
  
  years <- mtrx$year
  
  mtrx$year <- NULL
  
  mtrx <- as.matrix(mtrx)
  rownames(mtrx) <- years
  
  
  r2stl(
    x=as.numeric(rownames(mtrx)),
    y=as.numeric(colnames(mtrx)),
    z=mtrx,
    
    filename=paste0("stl/hfd/", this_country, "_asfr.stl"),
    z.expand=T,
    show.persp=F
  )
}


d_ply(
  data_hfd, 
  .(country),
  fn,
  .progress="text"
)