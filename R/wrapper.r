################################################################################
###                                                                          ###
###    INFORMATIONS                                                          ###
###    ---------------------------------                                     ###
###                                                                          ###
###       PACKAGE NAME        amsReliability                                 ###
###       MODULE NAME         wrapper.r                                      ###
###       VERSION             0.9                                            ###
###                                                                          ###
###       AUTHOR              Emmanuel Chery                                 ###
###       MAIL                emmanuel.chery@ams.com                         ###
###       DATE                2016/01/13                                     ###
###       PLATFORM            Windows 7 & Gnu/Linux 3.16                     ###
###       R VERSION           R 3.1.1                                        ###
###       REQUIRED PACKAGES   ggplot2, grid, MASS, nlstools, scales          ###
###       LICENSE             GNU GENERAL PUBLIC LICENSE                     ###
###                           Version 3, 29 June 2007                        ###
###                                                                          ###
###                                                                          ###
###    DESCRIPTION                                                           ###
###    ---------------------------------                                     ###
###                                                                          ###
###       This package is a collection of scripts dedicated to help          ###
###    the process reliability team of ams AG. It includes tools to          ###
###    quickly visualize data and extract model parameters in order          ###
###    to predict device lifetimes.                                          ###
###                                                                          ###
###       This module is dedicated to the wrapper function.                  ###
###    It allows starting all other functions by using a text menu.          ###
###                                                                          ###
###                                                                          ###
###    FUNCTIONS                                                             ###
###    ---------------------------------                                     ###
###                                                                          ###
###       ReliabilityAnalysis          Display a menu to select an analysis  ###
###                                                                          ###
################################################################################


#' Reliability data analysis
#'
#' This function displays a menu listing all available analyses.
#' User is guided with textual menus in order to select the analysis.
#'
#' @param None
#'
#' @return None
#'
#' @examples
#' ReliabilityAnalysis()
#' @author Emmanuel Chery, \email{emmanuel.chery@@ams.com}
#' @import ggplot2 MASS scales nlstools tcltk
#' @export
ReliabilityAnalysis <- function()
# Wrapper to call all the available analyses of amsReliability
{
    amsRelHeader()

    cat("Please select the analysis you want to perform:\n-----------------------------------------------\n\n")
    cat(" 1) Electromigration\n 2) TDDB\n 3) Quit\n\n")
    clc()
    userChoice <- readline(prompt="Enter your choice: ")

    if (userChoice == 1){
        amsRelHeader()
        cat("\nElectromigration\n----------------\n\n")
        cat(" 1) Exportfile creation\n 2) Lifetime analysis and parameter extraction\n 3) Quit\n\n")
        clc()
        userChoice <- readline(prompt="Enter your choice: ")

        if (userChoice == 1){
            CreateExportFiles()
        } else if (userChoice == 2){
            BlackAnalysis()
        } else {
            return()
        }

    } else if (userChoice == 2){
        amsRelHeader()
        cat("\nTDDB\n----\n\n")
        cat(" 1) Lifetime analysis and parameter extraction\n 2) Quit\n\n")
        clc()
        userChoice <- readline(prompt="Enter your choice: ")

        if (userChoice == 1){
            OxideTDDB()
        # } else if (userChoice == 2){
            # BlackAnalysis()
        } else {
            return()
        }

    } else if (userChoice == "u") {
            AutoUpdate()

    } else {
        return()
    }

}

clc <- function()
# Clear screen function for terminal
{
    return(cat("\n\n\n\n\n\n\n\n"))
    # return(cat("\014"))
}

amsRelHeader <- function()
# Display title and version number
{
    clc()
    cat("\n \t\t\t//////////////////////////////////////////\n ")
    cat("\t\t\t/                                        /\n ")
    cat("\t\t\t/\tWelcome in amsReliability!\t /\n")
    cat("\t\t\t/                                        /\n ")
    cat("\t\t\t//////////////////////////////////////////\n\n ")
    cat(paste("\n\nYou are currently using amsReliability version ", packageVersion("amsReliability"), ".\n\n\n", sep=''))
}

AutoUpdate <- function()
{
    print("AutoUpdate ongoing")
    library(devtools)
    install_github("KDB2/amsReliability")
    return()
}
