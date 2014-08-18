rm(list=ls())

# To do
# 1) add back imputation research


source("scripts/LoadPackages.R")

RequiredPackages(
  c(
    "reshape",
    "lattice",
    "r2stl",
    "plyr",
    "rgl"
    )
  )

#source("scripts/old_functions.r")

counts <- read.csv("data/tidy/counts.csv")


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
stl_spooler <- function(x, min_age=0, max_age = 80){
  x <- subset(x, age >= min_age & age <= max_age)
  x <- mutate(
    x, 
    death_rate = death_count / population_count,
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


#############################################################################################
########## A little toy example 

x1 <- matrix(
  c(0.45, NA, 0.50,
    0.60, 0.55, 0.62,
    0.58, 0.56, 0.66
    ),
  nrow=3, byrow=T
  )

source("scripts/imputation_functions.r")

y1 <- impute_matrix(x1, repeat.it=T)
exp(y1$output)

y1a <- impute_matrix(x1, uselog=T, repeat.it=T)
y1a$output



x2 <- matrix(
  c(0.45, NA, 0.50, NA,
    0.60, 0.55, 0.62, 0.66,
    NA, NA, 0.66, 0.59,
    NA, NA, 0.64, 0.70
  ),
  nrow=4, byrow=T
)

y2 <- impute_matrix(x2, uselog=T, repeat.it=T)
# produces error messages but they are successfully caught
exp(y2$output)

y2a <- impute_matrix(x2, uselog=F, repeat.it=T)
# produces error messages but they are successfully caught
y2a$output



x3 <- matrix(
  c(0.45, NA, 0.50, 0.71,
    Inf, 0.55, 0.62, 0.66,
    0.55, 0.62, -Inf, 0.59,
    0.63, 0.59, 0.64, 0.70
  ),
  nrow=4, byrow=T
)

y3 <- impute_matrix(x3, uselog=F, repeat.it=T)

#########################################################################################
#### Log mortality rate differences
#########################################################################################
# Note : this is not working properly yet

# Ideas: 
# I think the problem is caused by fn being thrown off by the presence of -Inf values
# meaning that other values cannot be normalised appropriately. This suggests that they need 
# to be removed prior to passing to fn rather than at the end, prior to passing 

# NOTE : At present this does not work

source("scripts/imputation_functions.r")

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
stl_dif_spooler <- function(x, min_age=0, max_age = 80){
  x <- subset(x, age >= min_age & age <= max_age)
  this.country <- as.character(x$country[1])
  x <- mutate(
    x, 
    death_rate = death_count / population_count,
    ldeath_rate = log(death_rate)
  )
  
  x <- subset(x, select=c("year", "age", "sex", "ldeath_rate"))
  x <- recast(x, id.var=c("year", "age", "sex"), measure.var="ldeath_rate", formula=year + age ~ sex)
  x <- mutate(x, ldif=male-female)
  class(x) <- "data.frame" # usual issue with cast_df attribute messing things up

  x <- subset(x, select=c("year", "age", "ldif"))

  x <- recast(x,
              age ~ year,
              id.var=c("age", "year"),
              measure="ldif"
              )
  
  x <- impute_matrix(x, uselog=F, repeat.it=T)$output
  
  x <- fn(x)

  

  r2stl(
    x=as.numeric(rownames(x)),
    y=as.numeric(colnames(x)),
    z=x,
    filename=paste0(
      "stl/ldif/dif_ldeath_rate_",
      this.country, ".stl"
      )
    )

  
}


d_ply(
  counts, 
  .(country),
  stl_dif_spooler,
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


###################################################################################
####################################################################################
### EAST AND WEST GERMANY ONLY #####################################################
####################################################################################
# IDENTITY RATHER THAN LOG

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
  subset(east_germany, subset=sex=="male", select=c("year", "age", "death_rate")), 
  age ~ year, 
  id.var=c("age", "year"), 
  measure="death_rate"
)

west_f_matrix <- recast(
  subset(west_germany, subset=sex=="female", select=c("year", "age", "death_rate")), 
  age ~ year, 
  id.var=c("age", "year"), 
  measure="death_rate"
)

west_m_matrix <- recast(
  subset(west_germany, subset=sex=="male", select=c("year", "age", "death_rate")), 
  age ~ year, 
  id.var=c("age", "year"), 
  measure="death_rate"
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
  
  filename="stl/germany/east_f_identity.stl",
  z.expand=T,
  show.persp=F
)

r2stl(
  x=as.numeric(rownames(east_m_matrix)),
  y=as.numeric(colnames(east_m_matrix)),
  z=east_m_matrix,
  
  filename="stl/germany/east_m_identity.stl",
  z.expand=T,
  show.persp=T
)

r2stl(
  x=as.numeric(rownames(east_f_matrix)),
  y=as.numeric(colnames(east_f_matrix)),
  z=east_f_matrix,
  
  filename="stl/germany/west_f_identity.stl",
  z.expand=T,
  show.persp=F
)

r2stl(
  x=as.numeric(rownames(east_m_matrix)),
  y=as.numeric(colnames(east_m_matrix)),
  z=east_m_matrix,
  
  filename="stl/germany/west_m_identity.stl",
  z.expand=T,
  show.persp=T
)


###########################################################################
##################################################################

#Italy 

counts_ita <- subset(counts, subset=country=="ITA")

counts_ita <- mutate(counts_ita, death_rate = death_count/population_count)

tmp <- subset(counts_ita, subset=sex=="female" & age <=80, select=c("year", "age", "death_rate"))
tmp2 <- recast(tmp, year ~ age, measure.var="death_rate")

#############################################################################
#############################################################################

# Now to do a comparison between England & Wales and Scotland

counts_subset <- subset(counts, subset=country=="GBR_SCO" | country=="GBRTENW")
counts_subset <- mutate(counts_subset, death_rate = death_count/population_count)
years_minmax <- ddply(counts_subset, .(country), function(x) c(min=min(x$year), max=max(x$year)))
# from 1855 to 2011

counts_subset <- subset(counts_subset, subset=year>=1855 & age <=80)

counts_subset$death_count <- NULL
counts_subset$population_count <- NULL

counts_wide <- recast(counts_subset, year + age + sex ~ country, 
                      id.var=c("year", "age", "sex", "country"), 
                      measure="death_rate"
                      )

counts_wide <- rename(counts_wide, c("GBR_SCO"="scotland", "GBRTENW"="england_wales"))
counts_wide <- mutate(counts_wide, 
                      difference=scotland-england_wales, 
                      dif_log = log(scotland) - log(england_wales)
                      )


### To do: apply contour map code with previous arguments to these data
data_ss <- subset(counts_wide, subset=sex=="male")

dif_abs_max <- max(abs(data_ss$dif_log))

contourplot(dif_log ~ year * age, 
            data=data_ss, 
            region=T, col.regions=rev(heat.colors(200)), cuts=10)                   

contourplot(england_wales ~ year * age | sex, data=counts_wide, region=T, col.regions=rev(heat.colors(200)), cuts=20)                   

# ss <- subset(counts_wide, subset=sex=="male")
# > contourplot(difference ~ age + year, ss)
# > contourplot(difference ~ year + age, ss)
# > contourplot(log(difference) ~ year + age, ss)
# Warning message:
#   In log(difference) : NaNs produced
# > contourplot(scotland ~ year + age, ss)
# > contourplot(england ~ year + age, ss)
# Error in eval(expr, envir, enclos) : object 'england' not found
# > contourplot(england_wales ~ year + age, ss)
# 



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

