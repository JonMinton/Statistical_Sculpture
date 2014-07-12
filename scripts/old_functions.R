Unpack_HMD <- function(
    zipfile,
    outlocation
    ){
    dir.create(outlocation, recursive=T)
    
    command_for_system <- paste0(
        "unzip ", 
        zipfile,
        " -d ",
        outlocation
        )
    
    system(
        command_for_system, 
        wait=T # Change to wait =F if this does not work
        )
}




###################################################################################################
# Make derived data file 
# Jon Minton
# 12 May 2014

Make_Derived_Data <- function(
    HMD_Location,
    Country.Codes,
    Outfile_Location, 
    Outfile_Name="Derived_Data.RData",
    old_HMD=FALSE
){
    # directories for each country
    N.groups <- dim(Country.Codes)[1]
    Populations <- Deaths <- Life.Expectancies <- vector("list", N.groups)
    names(Populations) <- names(Deaths) <- names(Life.Expectancies) <- Country.Codes[,1]
    
    if (old_HMD){
        country.Directories <- paste(
            HMD_Location, 
            Country.Codes[,1], 
            "STATS/", 
            sep="/"
        )
        # Population
        
        # one for each listed country
        
        for (i in 1:N.groups){
            Populations[[i]] <- read.table(file=paste(country.Directories[i], "Population.txt", sep=""), sep="", skip=2, header=T, na.strings=".")
            Deaths[[i]] <- read.table(file=paste(country.Directories[i], "Deaths_1x1.txt", sep=""), sep="", skip=2, header=T, na.strings=".")
            Life.Expectancies[[i]] <- read.table(file=paste(country.Directories[i], "E0per.txt", sep=""), sep="", skip=2, header=T, na.strings=".")
        }
        
    } else{
        # Do this if using a newer version of the HMD
        pop_files <- paste0(
            HMD_Location,
            "/population/Population/",
            Country.Codes[,1], 
            ".Population.txt"
        )
        death_files <- paste0(
            HMD_Location,
            "/deaths/Deaths_1x1/",
            Country.Codes[,1],
            ".Deaths_1x1.txt"
        )
        le_files <- paste0(
            HMD_Location,
            "/e0_per/E0per/",
            Country.Codes[,1],
            ".E0per.txt"
        )
        
        
        for (i in 1:N.groups){
            Populations[[i]] <- read.table(file=pop_files[i], sep="", skip=2, header=T, na.strings=".")
            Deaths[[i]] <- read.table(file=death_files[i], sep="", skip=2, header=T, na.strings=".")
            Life.Expectancies[[i]] <- read.table(file=le_files[i], sep="", skip=2, header=T, na.strings=".")
        }
    }
    
    Populations.numeric <- lapply(Populations, function(x) MakeNumeric(x, T))
    Deaths.numeric <- lapply(Deaths, function(x) MakeNumeric(x, T))
    
    # LOAD THE TWO FUNCTIONS
    
    DeathRates <- DeathRates.EV <- Deaths.EV <- Deaths.EVexcInfants <- vector("list", length(names(Populations)))
    names(DeathRates) <- names(DeathRates.EV) <- names(Deaths.EV) <- names(Deaths.EVexcInfants) <- names(Populations)
    
    for (i in 1:length(names(Populations))){
        #  print(i)
        this.country <- names(DeathRates)[i]
        DeathRates[[this.country]] <- findDeathRates(Pop=Populations[[this.country]], Deaths=Deaths[[this.country]])
        DeathRates.EV[[this.country]] <- findMeanVar(DeathRates[[this.country]])
        Deaths.EV[[this.country]] <- findMeanVar(Deaths[[this.country]])
        Deaths.EVexcInfants[[this.country]] <- findMeanVar(Deaths[[this.country]], excChild=T)    
    }
    
    
    save(
        Populations, 
        Populations.numeric, 
        Deaths, 
        Deaths.numeric, 
        Life.Expectancies, 
        Country.Codes, 
        DeathRates, 
        DeathRates.EV, 
        Deaths.EV, 
        Deaths.EVexcInfants, 
        file=paste(
            Outfile_Location,
            Outfile_Name,
            sep="/"
        )
    )
    
}


Make_Country_DF <- function(
    directory
){
    # For each file in the directory, 
    # Find the short country code by truncating before the first .
    # Find the full name by reading in the first line and 
    # truncating at the first ,
    files <- list.files(directory, pattern="*.txt")
    tmp <- sapply(
        strsplit(files, "[.]"), 
        function(x) x[1]
    )
    full_names <- paste0(
        directory,
        files
    )
    N.groups <- length(full_names)
    DF_out <- data.frame(
        short=tmp,
        long=NA
    )
    
    for (i in 1:N.groups){
        this.line <- readLines(
            full_names[i],
            n=1
        )
        tmp <- sapply(
            strsplit(this.line, "[,]"),
            function(x) x[1]
        )
        DF_out[i,"long"] <- tmp
    }
    
    return(DF_out)
}

# Data storage and management


# 13 July 2011

# Set wd

# 11 July 2011
# Tasks:

# NOTE: RUN this script from 'Scripts.R'


#rm(list=ls())

#current.wd <- "X:/1918 Cohort/1918/main/Data" 

GetAgeRateDif <- function(df, maxage=80, k=1){
    # df is a dataframe extracted from DeathRates
    # k is the number of years difference the age rates should be compared between
    
    makedif <- function(d){
        tmp <- try(
            with(d,
                 d[Year==y & Age==a, c("Female", "Male", "Total")] - d[Year==(y-k) & Age==a, c("Female", "Male", "Total")]
            ), silent=T 
        )
        
        return(
            ifelse(
                class(tmp)=="try-error"
                , 
                rep(NA, 3), 
                tmp)
        )
    }
    
    df <- subset(df, Age <=maxage)
    df2 <- df[df$Year!=min(df$Year),]
    df2$Female <- NA
    df2$Male <- NA
    df2$Total <- NA
    
    toages <- 0:maxage  
    
    years <- quantile(unique(df$Year), c(0,1))
    years[1] <- years[1] + 1
    years <- years[1]:years[2]
    
    for (y in years){
        for (a in toages){
            df2[df2$Year==y & df2$Age==a, c("Female", "Male", "Total")] <- makedif(df)      
        }
    }      
    return(df2)
}


# FUNCTIONS

findMeanVar <- function(Input, getFromFile=F, excChild=F){
    
    #print("Entered findMeanVar")
    #nb option needed for excluding under 5s
    
    if (getFromFile==T){
        Data <- read.table(file=Input, sep="", skip=2, header=T)
    }
    
    if (getFromFile==F) {
        Data <- Input
    }
    
    levels(Data$Age)[levels(Data$Age)=="110+"] <- "110"
    Data$Age <- as.numeric(as.character(Data$Age))
    #n.b. as.numeric(levels(Data$Age))[Data$Age] should also work, and is supposed to be slightly more computationally efficient
    
    
    if (excChild==T) {
        Data <- subset(Data, subset=Age>4)
    }
    
    
    MeanMale <- as.vector(by(Data, Data$Year, function(x) (sum(x$Age*x$Male) / sum(x$Male)), simplify=T))
    MeanFemale <- as.vector(by(Data, Data$Year, function(x) (sum(x$Age*x$Female) / sum(x$Female)), simplify=T))
    MeanTotal <- as.vector(by(Data, Data$Year, function(x) (sum(x$Age*x$Total) / sum(x$Total)), simplify=T))
    
    VarParMale <- as.vector(by(Data, Data$Year, function(x) (sum(x$Male*x$Age*x$Age) / sum(x$Male))))
    VarParFemale <- as.vector(by(Data, Data$Year, function(x) (sum(x$Female*x$Age*x$Age) / sum(x$Female))))
    VarParTotal <- as.vector(by(Data, Data$Year, function(x) (sum(x$Total*x$Age*x$Age) / sum(x$Total))))
    
    
    VarMale <- VarParMale - MeanMale^2
    VarFemale <- VarParFemale - MeanFemale^2
    VarTotal <- VarParTotal - MeanTotal^2
    
    #print("Exiting findMeanVar")
    
    OutData <- data.frame(Year=unique(Data$Year), 
                          MaleMean=MeanMale, 
                          MaleVar=VarMale, 
                          FemaleMean=MeanFemale, 
                          FemaleVar=VarFemale, 
                          TotalMean=MeanTotal, 
                          TotalVar=VarTotal)
}

############

findDeathRates <- function(Pop, Deaths){
    
    #print("Entered findDeathRates")
    
    A <- Deaths
    
    temp <- A$Age
    levels(temp)[levels(temp)=="110+"] <- "110"
    temp <- as.numeric(as.character(temp))
    A$Age <- temp
    
    A <- data.frame(A, code=paste(A$Year, "_", A$Age, sep=""))
    
    B <- Pop
    temp <- B$Age
    levels(temp)[levels(temp)=="110+"] <- "110"
    temp <- as.numeric(as.character(temp))
    B$Age <- temp
    
    B <- data.frame(B, code=paste(B$Year, "_", B$Age, sep=""))
    
    matched.codes <- intersect(A$code, B$code)
    
    A <- A[A$code %in% matched.codes,]
    B <- B[B$code %in% matched.codes,]
    
    A <- A[order(A$Year, A$Age),]
    B <- B[order(B$Year, B$Age),]
    
    
    
    AB <- A[1:dim(A)[1], 3:5] / B[1:dim(B)[1], 3:5]
    AB <- data.frame(Year=A$Year, Age=A$Age, AB)
    
    #print("Exiting findDeathRates")
    #remove missing/erroneous values
    AB <- subset(AB, subset=is.finite(Male) & is.finite(Female) & is.finite(Total)& Male !=0 & Female !=0 & Total != 0)
    
}


pullnums <- function(lbls){
    tmp <- strsplit(lbls, "[.]")
    tmp <- sapply(tmp, function(x) x[2])
    tmp <- as.numeric(tmp)
    return(tmp)
}

makematrix <- function(df, byx, byy, z){
    df2 <- df[,c(byx, byy, z)]
    m <- reshape(df2, direction="wide", idvar=byx, timevar=byy)
    x <- m[,1]
    y <- pullnums(colnames(m[,-1]))
    
    m <- m[,-1]
    m <- as.matrix(m)
    rownames(m) <- x
    colnames(m) <- y
    return(m)
}


difmat <- function(m, dx=1, dy=1){
    nx <- dim(m)[1]
    ny <- dim(m)[2]
    
    m1 <- m[-(nx - dx + 1), -(ny - dx + 1)]
    m2 <- m[-dx, -dy]
    md <- m2 - m1
    return(md)
}

makelong <- function(m, rowvar, colvar){
    require(reshape)
    assign(rowvar, rownames(m))
    d <- data.frame(rowvar, m)
    d2 <- melt(d, rowvar)
    return(d2)
}


# New (as of 19/12/2012)
# Find population size

MakeNumeric <- function(input, debug=F){
    
    # Need to check whether years are factors or numeric
    # if factors, then identify numbers with -/+ suffixes
    # convert to numeric by taking mean of estimates for - and +
    # Total = Male + Female
    output <- input
    
    temp <- output$Age
    levels(temp)[levels(temp)=="110+"] <- "110"
    temp <- as.numeric(levels(temp))[temp]
    output$Age <- temp
    
    
    if(is.numeric(output$Year)){
        if(debug==T){cat("Year is numeric\n")}
    } else if (is.factor(input$Year)){
        # want to know which years are XXXX- and XXXX+
        if(debug==T){cat("Year is factor\n")}
        
        # need to know how many pairs of estimates are in the dataset
        pairs.factors <- levels(output$Year)[grep("[[:digit:]]{4}[[:punct:]]{1}", levels(output$Year))]
        n.pairs <- length(pairs.factors)/2
        if(debug==T){
            if(n.pairs==round(n.pairs)){cat("n.pairs is an integer: ", n.pairs, "\n")}
        }
        
        # want to identify which rows of the dataframe are not 'difficult'
        good.years <- levels(output$Year)[!(levels(output$Year) %in% pairs.factors)]
        out.good <- output[which(output$Year %in% good.years),]    
        out.bad <- data.frame(Year=NA, Age=NA, Female=NA, Male=NA, Total=NA)
        for (i in 1:n.pairs){
            year.factor.A <- pairs.factors[1 + (i - 1) * 2]
            year.factor.B <- pairs.factors[2 + (i - 1) * 2]
            
            common.year.A <- substr(year.factor.A, 1, 4)
            common.year.B <- substr(year.factor.B, 1, 4)
            
            common.year <- ifelse(common.year.A==common.year.B, common.year.A, NULL)
            
            if(debug==T){ if(is.null(common.year)){cat("Common year identification failed\n") 
            } else {cat("Common year is ", common.year,"\n")}
            }
            
            # check if the same ages are stored for both year- and year+
            ages.year.factor.A <- output[output$Year==year.factor.A, "Age"]
            ages.year.factor.B <- output[output$Year==year.factor.B, "Age"]
            
            common.ages <- intersect(ages.year.factor.A, ages.year.factor.B)
            either.ages <- union(ages.year.factor.A, ages.year.factor.B)
            
            ages.A.only <- ages.year.factor.A[!(ages.year.factor.A %in% common.ages)]
            ages.B.only <- ages.year.factor.B[!(ages.year.factor.A %in% common.ages)]      
            
            tmp.output <- data.frame(Year=common.year, Age=either.ages, Female=NA, Male=NA) # need to add total afterwards
            tmp.output[tmp.output$Age%in%common.ages, "Female"] <- (
                subset(output, subset=Year==year.factor.A, select=Female) +
                    subset(output, subset=Year==year.factor.B, select=Female)
            ) / 2
            
            tmp.output[tmp.output$Age%in%common.ages, "Male"] <- (
                subset(output, subset=Year==year.factor.A, select=Male) +
                    subset(output, subset=Year==year.factor.B, select=Male)
            ) / 2
            
            if (all(common.ages==either.ages)){
                if(debug==T){
                    cat("Both year versions contain the same number of ages\n")
                }
            }       
            
            if (length(ages.A.only) > 0){
                tmp.output[tmp.output$Age%in%ages.A.only, "Female"] <- subset(output, subset=Year==year.factor.A, select=Female) 
                tmp.output[tmp.output$Age%in%ages.A.only, "Male"] <-   subset(output, subset=Year==year.factor.A, select=Male) 
            }
            
            if (length(ages.B.only) > 0){
                tmp.output[tmp.output$Age%in%ages.B.only, "Female"] <- subset(output, subset=Year==year.factor.B, select=Female) 
                tmp.output[tmp.output$Age%in%ages.B.only, "Male"] <-   subset(output, subset=Year==year.factor.B, select=Male)     
            }
            
            tmp.output <- transform(tmp.output, Total=Male + Female)
            out.bad <- rbind(out.bad, tmp.output)
        }
        output <- rbind(out.bad, out.good)
    }
    # remove NAs somehow introduced
    output <- output[!apply(output, 1, function(x) any(is.na(x))),]
    output <- output[order(output$Year, output$Age),]
    return(output)   
}

GetValMat <- function(Datablock, country="", sex="", ages=0:80){
    Dataset <- Datablock[[country]]
    years <- unique(Dataset$Year)
    
    valMatrix <- matrix(NA, nrow=length(ages), ncol=length(years))
    
    
    for (i in 1:length(ages)){
        for (j in 1:length(years)){
            if(sex=="male"){
                tmp <- Dataset$Male[Dataset$Age==ages[i] & Dataset$Year==years[j] ]
                if (length(tmp)==1){ valMatrix[i, j] <- tmp}
            }
            if(sex=="female"){
                tmp <- Dataset$Female[Dataset$Age==ages[i] & Dataset$Year==years[j] ]
                if (length(tmp)==1) { valMatrix[i, j] <- tmp}             
            }
            if(sex=="both"){
                tmp <- Dataset$Total[Dataset$Age==ages[i] & Dataset$Year==years[j] ]
                if (length(tmp==1)) { valMatrix[i, j] <- tmp}               
            }
            if(sex=="diff"){
                tmp <- (Dataset$Male[Dataset$Age==ages[i] & Dataset$Year==years[j]] - 
                    Dataset$Female[Dataset$Age==ages[i] & Dataset$Year==years[j]])
                if (length(tmp==1)) {valMatrix[i,j] <- tmp}
                
            }
            
        }
        
    }
    
    rownames(valMatrix) <- ages
    colnames(valMatrix) <- years
    
    return(valMatrix)
}

ErrorCatch <- function(input){
    checker <- try(input)
    if (class(checker)=="try-error"){
        output <- NA
    } else {
        output <- input
    }
    return(output)
}

# Simple imputation : impute the average of contiguous cells (log scale)

GraphImpute <- function(X, uselog=T, repeat.it=F){
    ifelse(uselog==T, input <- log(X), input <- X )
    
    
    output <- input
    
    dim.x <- dim(input)[1]
    dim.y <- dim(input)[2]
    
    imputationMatrix <- matrix(0, dim.x, dim.y)
    imputationCoords <- which(is.na(X), T)
    numValsToImpute <- dim(imputationCoords)[1]
    
    for (i in 1:numValsToImpute){
        this.x <- imputationCoords[i,1]
        this.y <- imputationCoords[i,2]
        imputationMatrix[
            this.x, 
            this.y
            ] <- 1
    }
    
    for (i in 1:numValsToImpute){
        this.x <- imputationCoords[i,1]
        this.y <- imputationCoords[i,2]
        
        
        output[
            this.x, 
            this.y
            ] <- mean(
                c(
                    ErrorCatch(input[this.x - 1, this.y - 1]),
                    ErrorCatch(input[this.x - 1, this.y    ]),
                    ErrorCatch(input[this.x - 1, this.y + 1]),
                    ErrorCatch(input[this.x    , this.y - 1]),
                    ErrorCatch(input[this.x    , this.y    ]),
                    ErrorCatch(input[this.x    , this.y + 1]),
                    ErrorCatch(input[this.x + 1, this.y - 1]),
                    ErrorCatch(input[this.x + 1, this.y    ]),
                    ErrorCatch(input[this.x + 1, this.y + 1])
                ),
                na.rm=T
            )
    }
    
    remainingValsToImpute<- dim(which(is.na(output), T))[1]
    #  browser()
    if(repeat.it==T & remainingValsToImpute > 0){
        tmp <- GraphImpute(output, uselog=F, repeat.it=T)
        input <- tmp[["input"]]
        imputed <- tmp[["imputationCoords"]]
        output <- tmp[["output"]]
    }
    
    return(
        list(
            input=input,
            imputed=imputationCoords,
            output=output,
            numValsToImpute=numValsToImpute
        )
    )
}



Make_3D_Plot <-function(
  Datablock, 
  country="GBRTENW", 
  sex="male", 
  col="lightgrey", 
  specular="black", 
  return.valmat=F, 
  ages=0:80, axes=F, box=F, xlab="", ylab="", zlab="", impute=F, 
  repeat.it=F, log.it=F
  ){
    
    valMatrix <- GetValMat(Datablock, country=country, sex=sex, ages=ages)
    
    if (impute==T){
        valMatrix <- exp(GraphImpute(valMatrix, repeat.it=repeat.it)[["output"]])  
    }
    
    
    
    as.numeric(rownames(valMatrix)) -> ages
    as.numeric(colnames(valMatrix)) -> years
    
    
    # reversing the order as persp3d displays years opposite to how I was expecting
    
    valMatrix2 <- matrix(NA, nrow=length(ages), ncol=length(years))
    rownames(valMatrix2) <- rev(ages)
    colnames(valMatrix2) <- rev(years)
    for (j in 1:length(years)) {valMatrix2[,dim(valMatrix)[2] - j +1] <- valMatrix[,j]}
    #  dimnames(valMatrix2) <- list(ages, sort(years, T))
    
    
    if(log.it==T){
        valMatrix2 <- log(valMatrix2)
    }
    
    require(rgl)
    
    persp3d(
             x=valMatrix2,

            # persp3d(x=ages, y=years, z=valMatrix2,
            col=col,
            specular=specular, axes=axes, 
            box=box, xlab=xlab, ylab=ylab, zlab=zlab)    
    if (return.valmat==F){
        Output <- list(start.Year=min(years), end.year=max(years)) 
    } else {
        Output <- list(start.Year=min(years), end.year=max(years), valmat=valMatrix2)
    }
    return(Output)
}


Standardise <- function(x){
    out <- (x - mean(x))/sd(x)
    return(out)
}

CalcCommon <- function(Dta.inc, Dta.exc){
    mm.inc <- Standardise(Dta.inc$MaleMean)
    mv.inc <- - Standardise(Dta.inc$MaleVar)
    fm.inc <- Standardise(Dta.inc$FemaleMean)
    fv.inc <- - Standardise(Dta.inc$FemaleVar)
    
    mm.exc <- Standardise(Dta.exc$MaleMean)
    mv.exc <- - Standardise(Dta.exc$MaleVar)
    fm.exc <- Standardise(Dta.exc$FemaleMean)
    fv.exc <- - Standardise(Dta.exc$FemaleVar)
    
    All <- cbind(mm.inc, mv.inc, fm.inc, fv.inc, mm.exc, mv.exc, fm.exc, fv.exc)
    
    mean.of.all <- apply(All, 1, mean)
    
    return(list(common=mean.of.all, All=All, years=Dta.inc$Year))
}

GetCohortDeathRates <- function(Dta, sex="male", cohortYear=1918){
    
    cohortYears <- Dta$Year - Dta$Age
    
    rates <- switch(sex,
                    male=Dta$Male,
                    female=Dta$Female,
                    total=Dta$Total)
    
    cohortRates <- rates[cohortYears==cohortYear]
    cohortAges <- Dta$Age[cohortYears==cohortYear]
    
    Output <- data.frame(age=cohortAges, rate=cohortRates)
    
    return(Output)
}

# Get mortality rates for age x

GetAgeSpecificMortRates <- function(DtaBlock, years=NA, ages=NA){
    if(is.list(DtaBlock)){
        tmp <- expand.grid(year=years, age=ages)
        output <- data.frame(tmp, value=NA)
        browser()
        for (i in 1:length(ages)){
            this.rate <- sapply(DtaBlock, function(x) x$rate[x$age==ages[i]])
            output$value[output$age==ages[i]] <- this.rate
            browser()
        }  
    }  
    return(output)
}


###############################

# Comment from peer reviewer 2: 


#1.3) Examination of cohort effects:  The authors are keen to examine the cohorts effects and 
#this is examine in the contour plots by tracing diagonal lines with slope=1 from the year of 
#birth.  However, I was wondering that to understand the cohort effect it might be more useful 
#to plot the distributions of the risk of death at each age for the cohort of people born each 
#year.  So the x-axis is the year of birth, the y-axis the age of death and in the graph is the 
#probability of death, so for x=1945 y=20 in the graph would be the proportion of people that 
#died when they were 20 years old among those born in 1945.  The contour plots should be now 
#going downwards at younger ages and upwards at higher ages. And in any case I would suggest 
#using coloured areas (as explained below).
#For the first graphs (as 1 and 2) the mean and variance life expentancy could be plotted for each cohort.

# So, want to produce something that uses DeathRates to produce CohortDeathRates

getCohortDeathRates <- function(input, ages=0:80){
    years <- unique(input$Year)
    n.years <- length(years)
    n.ages <- length(ages)
    max.age <- max(ages)
    output <- input[input$Age<=max.age,]
    output$Female <- NA
    output$Male <- NA
    output$Total <- NA
    
    for (i in 1:n.years){
        for (j in 1:n.ages){
            tryval.female <- input$Female[input$Year==(years[i] + ages[j]) & input$Age==(ages[j])]
            tryval.male   <- input$Male[input$Year==(years[i] + ages[j]) & input$Age==(ages[j])]
            tryval.total  <- input$Total[input$Year==(years[i] + ages[j]) & input$Age==(ages[j])]
            if(length(tryval.female)==1){
                output$Female[output$Year==years[i] & output$Age==ages[j]] <- tryval.female
            }      
            if(length(tryval.male)==1){
                output$Male[output$Year==years[i] & output$Age==ages[j]] <- tryval.male
            }
            if(length(tryval.total)==1){
                output$Total[output$Year==years[i] & output$Age==ages[j]] <- tryval.total
            }
        }
    }
    
    return(output)
}



####

Make_3D_Plot.UI <- function(
  Datablock, 
  country="GBRTENW", 
  sex="male", 
  col="lightgrey", 
  specular="black", 
  return.valmat=F, 
  ages=0:80, axes=F, box=F, xlab="", ylab="", zlab=""
  
  ){
  
  countries <- sort(as.character(Country.Codes$Country.Name))
  
  this.country <- tk_select.list(
    choices=countries, 
    multiple=F, 
    title="Pick Country"
  )
  this.country_code <- Country.Codes$Country.Code[Country.Codes$Country.Name==this.country]
  # Now find subgroups without group
  
  sexes <- c(
    "male",
    "female",
    "total"
    )
  
  this.sex <- tk_select.list(
    choices=sexes,
    multiple=F,
    title="Pick sex"
  )
  
  this.logchoice <- tk_select.list(
    choices=c("yes" ,"no"),
    multiple=F,
    title="Log values?"
    )
  
  this.logchoice <- switch(
      this.logchoice,
      "yes"=TRUE,
      "no"=FALSE
      )
  
  this.spinchoice <- tk_select.list(
      choices=c("yes", "no"),
      multiple=F,
      title="Spin animation?"
      )
  
  this.spinchoice <- switch(
      this.spinchoice,
      "yes"= TRUE,
      "no"=FALSE
      )
  
  output <- Make_3D_Plot(
    Datablock, 
    country=this.country_code, 
    sex=this.sex, 
    col="lightgrey", 
    specular="black", 
    return.valmat=F, 
    ages=0:80, axes=F, box=F, xlab="", ylab="", zlab="", 
    impute=T, 
    repeat.it=T, 
    log.it=this.logchoice    
    )
  
  if (this.spinchoice){
      play3d(spin3d())
  }
  return(output)
}
