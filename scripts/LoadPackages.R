#############################################
#'  @title Function for loading packages
#' 
#'  @description Useful for loading several required packages at the same time. 
#'  
#'  @details
#'  Checks to see which packages are already installed and tries to install new ones 
#'  and load them. No error handling, only report on what happened. 
#'  Idea from StackOverflow answer by Thierry  http://stackoverflow.com/a/5601150/801398
#'  @author Maja Zaloznik
#'  @param required - a character vector of names of the packages that need to be loaded
RequiredPackages <- function(required) {
    # check which packages are already installed:
    already.installed <- required %in% installed.packages()[,"Package"]
    # load the ones already installed
    for (i in required[already.installed])  {
        require(i, character.only = TRUE)}
    
    # check which packages need to be installed
    to.install <- !required %in% installed.packages()[,"Package"]
    # install packages and 
    for (package in required[to.install]) {
        message("\n Trying to install missing package ",package,"\n")
        install.packages(package)}
    
    # figure out which ones didn't install and which have and need to be loaded
    didnt.install <- !required %in% installed.packages()[,"Package"]
    newly.installed <- !(already.installed | didnt.install)
   
    # load newly installed packages
    for (i in required[newly.installed])  {
        require(i, character.only = TRUE)}
    
    # summarise what just happened in a relatively ugly way
    cat("PACKAGE INSTALLATION REPORT:\n")
    if (length(required[already.installed]) != 0) {
        cat(paste("The package", required[already.installed], "was already installed correctly.\n"), sep="")}
    if (length(required[newly.installed]) != 0) {
        cat(paste("The package",required[newly.installed], "was newly installed.\n"), sep="")  }
    if (length(required[didnt.install]) != 0) {
        cat(paste("The package",required[didnt.install], "didn't install!!\n", 
                  "-> Check the spelling or try doing it manually.\n"), sep="")}
}

