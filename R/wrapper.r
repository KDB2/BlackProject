
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
        userChoice <- readline(prompt="Enter your choice: ")

        if (userChoice == 1){
            OxideTDDB()
        # } else if (userChoice == 2){
            # BlackAnalysis()
        } else {
            return()
        }

    } else {
        return()
    }

}

clc <- function()
# Clear screen function for terminal
{
    return(cat("\n\n\n\n\n\n"))
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
