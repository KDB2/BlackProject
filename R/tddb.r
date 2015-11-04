################################################################################
###                                                                          ###
###    INFORMATIONS                                                          ###
###    ---------------------------------                                     ###
###                                                                          ###
###       PACKAGE NAME        amsReliability                                 ###
###       SECTION NAME        tddb.r                                         ###
###       VERSION             0.7                                            ###
###                                                                          ###
###       AUTHOR              Emmanuel Chery                                 ###
###       MAIL                emmanuel.chery@ams.com                         ###
###       DATE                2015/11/03                                     ###
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
###       This section is dedicated to tddb experiments.                     ###
###    Extraction of model parameters is performed.                          ###
###                                                                          ###
###                                                                          ###
###    FUNCTIONS                                                             ###
###    ---------------------------------                                     ###
###                                                                          ###
###       ReadDataTDDB              Read Exportfile and create data table    ###
###                                                                          ###
################################################################################


ReadDataTDDB <- function(ListFiles)
{

    # ResTable initialisation
    ResTable <- data.frame()

    for (i in seq_along(ListFiles)){

        # Find the line where the data are stored. They start after [DATA].
        StartLine <- grep('DATA]', readLines(ListFiles[i]) )
        # Read the file. Data start two lines after [DATA]
        File <- read.delim(ListFiles[i], skip=StartLine+2, header=FALSE)

        # Store important data
        TTF <- File[,14]
        Temperature <- File[,9]
        Dimension <- File[,17]
        Stress <- File[,12]
        Status <- File[,15]
        # Make status similar to Electromigration: -1 wrong device, not to be considered in statistic; 1 failed; 0 not failed.
        Status[Status != 100 & Status != -1] <- 1
        Status[Status == 100] <- 0
        # Device with Status 0 have wrong voltage and TTF = 0. We will force good voltage and 1E30 for TTF.
        TTF[Status==0] <- 1E30
        Stress[Status==0] <- as.numeric(substr(as.character(File[Status==0,2]), 2, nchar(as.character(File[Status==0,2]))))
        # Condition stickers
        Conditions <- paste(Stress,"V/",Temperature,"C",sep="")
        # Creation of a dataframe to store the data
        TempDataFrame <- data.frame(TTF,Status,Conditions,Stress,Temperature,Dimension)
        # Force the column names
        names(TempDataFrame) <- c("TTF", "Status", "Conditions", "Stress", "Temperature","Dimension")
        # Store the data in the final table
        ResTable <- rbind(ResTable,TempDataFrame)
    }

    # Now that all the files have been opened, let's check if different sizes were used.
    # If yes, we will change the Condition sticker to include the area.
    if (length(levels(factor(Dimension))) > 1 ){
        ResTable$Condition <- paste(ResTable$Condition,"/",ResTable$Dimension /1000, "k" , sep="")
    }

    # Cleaning to remove units where status is not 1 or 0.
    ResTable <- Clean(ResTable)

    # List the conditions present in ResTable
    CondList <- levels(factor(ResTable$Conditions))

    # Probability is missing. Let's add it.
    # Final dataframe is ExpDataTable
    # Initialisation
    ExpDataTable <- data.frame()

    # For each condition found, we calculate the probability of failure. Data are stacked in ExpDataFrame. Weibull scale is used.
    for (i in seq_along(CondList)){

        TempDataTable <- CreateDataFrame(ResTable$TTF[ResTable$Conditions==CondList[i]], ResTable$Status[ResTable$Conditions==CondList[i]],
          ResTable$Condition[ResTable$Conditions==CondList[i]], ResTable$Stress[ResTable$Conditions==CondList[i]], ResTable$Temperature[ResTable$Conditions==CondList[i]], Scale="Weibull",ResTable$Dimension[ResTable$Conditions==CondList[i]])

        ExpDataTable <- rbind(ExpDataTable,TempDataTable)
    }

    # We force the new names here as a security check.
    names(ExpDataTable) <- c("TTF", "Status", "Probability", "Conditions", "Stress", "Temperature","Dimension")
    return(ExpDataTable)

}
