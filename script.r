rm(list=ls())

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

source("scripts/old_functions.r")

counts <- read.csv("data/tidy/counts.csv")


###########################################################################

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
  
  filename="stl/east_f.stl",
  z.expand=T,
  show.persp=F
)

r2stl(
  x=as.numeric(rownames(east_m_matrix)),
  y=as.numeric(colnames(east_m_matrix)),
  z=east_m_matrix,
  
  filename="stl/east_m.stl",
  z.expand=T,
  show.persp=T
)

r2stl(
  x=as.numeric(rownames(east_f_matrix)),
  y=as.numeric(colnames(east_f_matrix)),
  z=east_f_matrix,
  
  filename="stl/west_f.stl",
  z.expand=T,
  show.persp=F
)

r2stl(
  x=as.numeric(rownames(east_m_matrix)),
  y=as.numeric(colnames(east_m_matrix)),
  z=east_m_matrix,
  
  filename="stl/west_m.stl",
  z.expand=T,
  show.persp=T
)


###########################################################################
##################################################################

Italy 

counts_ita <- subset(counts, subset=country=="ITA")

counts_ita <- mutate(counts_ita, death_rate = death_count/population_count)

tmp <- subset(counts_ita, subset=sex=="female" & age <=80, select=c("year", "age", "death_rate"))
tmp2 <- recast(tmp, year ~ age, measure.var="death_rate")


##################################################################################


######################################################################################################
## 5/2/2014
## Work on creating STL file for 3d printing
######################################################################################################
# EPA Graphics

# Prep work
rm(list=ls())
current.wd <- "X:/REFERENCE/1918 Cohort/1918/main/Data/"
setwd(current.wd)
source("Functions.R")
load("DerivedData.rData")

# Logged surface plot
Make3dPlot(DeathRates,country="ITA", sex="male", log.it=T, return.valmat=T)

# Want to return the output of the rgl persp3d function called in Make3dPlot
# col="lightgrey" 
# specular="black" 
# axes=F 
# box=F 
# xlab="" 
# ylab="" 
# zlab="" 
# 
# tmp2 <- persp3d(tmp,
#         col=col,
#         specular=specular, axes=axes, 
#         box=box, xlab=xlab, ylab=ylab, zlab=zlab)    

require("r2stl")

z <- tmp
x <- 1:nrow(tmp)
y <- 1:ncol(tmp)

z <- z - min(z)

z <- z / max(z)
x <- x - min(x) ; x <- x / max(x)
y <- y - min(y) ; y <- y / max(y)



r2stl(
  x=x,
  y=y,
  z=z,
  
  filename="Italy_Males_Logged_Mort.stl",
  z.expand=T,
  show.persp=T
)
