################################################################################
###                                                                          ###
###    INFORMATIONS                                                          ###
###    ---------------------------------                                     ###
###                                                                          ###
###       PACKAGE NAME        amsReliability                                 ###
###       MODULE NAME         electromigration.r                             ###
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
###       This module is dedicated to electromigration experiments.          ###
###    Extraction of Black's parameters is performed.                        ###
###                                                                          ###
###                                                                          ###
###    FUNCTIONS                                                             ###
###    ---------------------------------                                     ###
###                                                                          ###
###       BlackAnalysis             Main function for data analysis          ###
###       BlackModelization         Extraction of Black's parameters         ###
###       ReadDataAce               Read Exportfile and create data table    ###
###                                                                          ###
################################################################################


ReadDataAce <- function(ListFileName, StructureList=c())
# Read the exportfiles listed in ListFileName and store them in a dataframe.
# First read all the files and then calculate the probability scale
# for each condition. This allows to work with conditions splitted in different files.
# Data are cleaned to remove bad units
# Exportfile from Ace and Mira have different headers,
# therefore column numbers are used
{
    # ResTable initialisation
    ResTable <- data.frame()

    for (file in ListFileName){

        # Read the file and store it in a temporary dataframe
        TempTable <- read.delim(file)
        # Creation of the new dataframe
        TTF <- TempTable[,3]
        Status <- TempTable[,2]
        Stress <- TempTable[,5]
        Temperature <- TempTable[,8]
        Condition <- paste(TempTable[,5],"mA/",TempTable[,8],"°C",sep="") #paste(TempTable[,"Istress"],"mA/",TempTable[,"Temp"],"°C",sep="")
        Dimension <- TempTable[,6]

        # Handle case where some unfailed samples have a lower TTF than finished ones.
        TTF[Status==0 & TTF<max(TTF[Status==1])] <- max(TTF[Status==1]) + 1

        # Creation of a dataframe to store the data
        TempDataFrame <- data.frame(TTF,Status,Condition,Stress,Temperature,Dimension)
        # Force the column names
        names(TempDataFrame) <- c("TTF", "Status", "Conditions", "Stress", "Temperature","Dimension")
        # Store the data in the final table
        ResTable <- rbind(ResTable,TempDataFrame)
    }
    # security check, we force again the name on ResTable
    names(ResTable) <- c("TTF", "Status", "Conditions", "Stress", "Temperature", "Dimension")

    # Cleaning to remove units where status is not 1 or 0.
    ResTable <- Clean(ResTable)

    # If Structure is not empty, we select only the structures listed.
    if (length(StructureList) != 0){
        NewResTable <- data.frame()
        for (strucLength in StructureList){
            NewResTable <- rbind(NewResTable,ResTable[ResTable$Dimension==strucLength,])
        }
        ResTable <- NewResTable
    }

    # List the conditions present in ResTable
    CondList <- levels(factor(ResTable$Conditions))

    # Probability is missing. Let's add it.
    # Final dataframe is ExpDataTable
    # Initialisation
    ExpDataTable <- data.frame()

    # For each condition found, we calculate the probability of failure. Data are stacked in ExpDataFrame. Lognormal scale is used.
    for (cond in CondList){

        TempDataTable <- CreateDataFrame(ResTable$TTF[ResTable$Conditions==cond], ResTable$Status[ResTable$Conditions==cond],
          ResTable$Condition[ResTable$Conditions==cond], ResTable$Stress[ResTable$Conditions==cond], ResTable$Temperature[ResTable$Conditions==cond], Scale="Lognormal",ResTable$Dimension[ResTable$Conditions==cond])

        ExpDataTable <- rbind(ExpDataTable,TempDataTable)
    }

    # We force the new names here as a security check.
    names(ExpDataTable) <- c("TTF", "Status", "Probability", "Conditions", "Stress", "Temperature","Dimension")
    # Order the condition in numerical/alphabetical order
    ExpDataTable <- ExpDataTable[OrderConditions(ExpDataTable),]
    # Do the same for conditions levels
    ExpDataTable$Conditions <- factor(ExpDataTable$Conditions, SortConditions(levels(ExpDataTable$Conditions)))
    return(ExpDataTable)
}


BlackModelization <- function(DataTable, DeviceID)
# Modelize the data using Black equation
# Extract the parameters: A, n and Ea
# as well as the lognormal slope
# TTF = A j^(-n) exp(Ea/kT + Scale * Proba)
# Proba in standard deviations
# Data(TTF,Status,Probability,Conditions,Stress,Temperature, Dimension)
{
    # Read the list of device to retrieve the section parameters.
    ListDevice <- try(read.delim("//fsup04/fntquap/Common/Qual/Process_Reliability/Process/amsReliability_R_Package/ListDeviceName.txt"),silent=TRUE)
    #if file is not present, error is returned.
    if (class(ListDevice) == "try-error"){
        print("File //fsup04/fntquap/Common/Qual/Process_Reliability/Process/amsReliability_R_Package/ListDeviceName.txt not found.")
        return(ListDevice)
    } else {

        W <- ListDevice$Width[ListDevice$Device==DeviceID] # micrometers
        H <- ListDevice$Height[ListDevice$Device==DeviceID] # micrometers
        S <- W*H*1E-12 # m^2

        # if S is a positive number different from 0, we can proceed:
        if (is.na(S) || S<=0 ) {
            print(paste("Structure",DeviceID, "is not present in the list. Please fill the list!"))
            # Force an error in the return for BlackAnalysis.
            ModelDataTable <- data.frame()
            as(ModelDataTable,"try-error")
            return(ModelDataTable)
        } else { # we proceed

          # Physical constants
          k <- 1.38E-23 # Boltzmann
          e <- 1.6E-19 # electron charge

          # Remove the units where status is 0
          CleanDataTable <- DataTable[DataTable$Status==1,]

          # Black model / Log scale: use of log10 to avoid giving too much importance to data with a high TTF
          Model <- nls(log10(TTF) ~ log10(exp(A)*(Stress*1E-3/S)^(-n)*exp((Ea*e)/(k*(Temperature+273.15))+Scale*Probability)), CleanDataTable, start=list(A=30,n=1,Ea=0.7,Scale=0.3),control= list(maxiter = 50, tol = 1e-7))#, minFactor = 1E-5, printEval = FALSE, warnOnly = FALSE))#,trace = T)
          #Model <- nls(TTF ~ exp(A)*(Stress*1E-3/S)^(-n)*exp((Ea*e)/(k*(Temperature+273.15))+Scale*Probability), DataTable, start=list(A=30,n=1,Ea=0.7,Scale=0.3))
          # Parameters Extraction
          A <- coef(Model)[1]
          n <- coef(Model)[2]
          Ea <-coef(Model)[3]
          Scale <- coef(Model)[4]
          # Residual Sum of Squares
          RSS <- sum(resid(Model)^2)
          # Total Sum of Squares: TSS <- sum((TTF - mean(TTF))^2))
          TSS <- sum(sapply(split(CleanDataTable[,1],CleanDataTable$Conditions),function(x) sum((x-mean(x))^2)))
          Rsq <- 1-RSS/TSS # R-squared measure
          #print(paste("Size on 150 rows:", format(object.size(Model), unit="Mb")))

          # Using the parameters and the conditions, theoretical distributions are created
          ListConditions <- levels(CleanDataTable$Conditions)

          # Initialisation
          ModelDataTable <- data.frame()
          # y axis points are calculated. (limits 0.01% -- 99.99%) Necessary to have nice confidence bands.
          Proba <- seq(qnorm(0.0001),qnorm(0.9999),0.05)

          for (condition in ListConditions){
              # Experimental conditions:
              I <- CleanDataTable$Stress[CleanDataTable$Conditions==condition][1]
              Temp <- CleanDataTable$Temperature[CleanDataTable$Conditions==condition][1]  # °C

              # TTF calculation
              TTF <- exp(A)*(I*0.001/S)^(-n)*exp((Ea*e)/(k*(273.15+Temp))+ Proba * Scale)

              # Dataframe creation
              ModelDataTable <- rbind(ModelDataTable, data.frame('TTF'=TTF,'Status'=1,'Probability'=Proba,'Conditions'=condition,'Stress'=I,'Temperature'=Temp))
          }

          # Drawing of the residual plots
          plot(nlsResiduals(Model))
          # Display of fit results
          cat(DeviceID,"\n")
          print(summary(Model))
          cat(paste("Residual squared sum: ",RSS,sep=""))
          #print(coef(Model))
          #print(sd(resid(Model)))

          # Save in a file
          capture.output(summary(Model),file="fit.txt")
          cat("Residual Squared sum:\t",file="fit.txt",append=TRUE)
          cat(RSS,file="fit.txt",append=TRUE)
          cat("\n \n",file="fit.txt",append=TRUE)
          cat("Experimental Data:",file="fit.txt",append=TRUE)
          cat("\n",file="fit.txt",append=TRUE)
          capture.output(DataTable,file="fit.txt",append=TRUE)
          return(ModelDataTable)
        }
    }
}


#' Electromigration data analysis
#'
#' Extract Black's parameters from a set of electromigration experiments.
#' The experimental data as well as the resulting model are displayed and
#' can be saved. Extracted parameters are saved in a fit.txt file.
#'
#' @param ErrorBand displays the confidence intervals if set to TRUE.
#' @param ConfidenceValue percentage used in the confidence interval calculation
#' @param Save saves the chart as .png if set to TRUE.
#'
#' @return None
#'
#' @examples
#' BlackAnalysis()
#' BlackAnalysis(ErrorBand=FALSE)
#' @author Emmanuel Chery, \email{emmanuel.chery@@ams.com}
#' @import ggplot2 MASS scales grid nlstools
#' @export
BlackAnalysis <- function(ErrorBand=FALSE, ConfidenceValue=0.95, Save=TRUE)
{
    # Disable warning for this function.
    oldw <- getOption("warn")
    options(warn = -1)
    #rm(list=ls())
    ListFiles <- list.files(pattern="*exportfile.txt")
    #DeviceID <- strsplit(ListFiles[1],split="_")[[1]][2]
    # case 1, there are one or several files available
    if (length(ListFiles) != 0){
          # List of DeviceID available in the selected exportfiles
          DeviceIDList <- levels(sapply(ListFiles,function(x){factor(strsplit(x,split="_")[[1]][2])}))

          for (DeviceID in DeviceIDList){
              SubListFiles <- ListFiles[grep(DeviceID,ListFiles)]
              # Import the file(s) and create the 3 dataframes + display data
              DataTable <- ReadDataAce(SubListFiles)
              # Attempt to modelize. If succes, we plot the chart, otherwise we only plot the data.
              ModelDataTable <- try(BlackModelization(DataTable, DeviceID),silent=TRUE)
              # Check if the modelization is a succes
              if (class(ModelDataTable) != "try-error"){
                    ErrorDataTable <- ErrorEstimation(DataTable, ModelDataTable, ConfidenceValue)
                    CreateGraphProba(DataTable,ModelDataTable,ErrorDataTable,DeviceID,Scale="Lognormal",ErrorBand,Save)
              } else { # if modelization is not a success, we display the data and return parameters of the distribution in the console (scale and loc) in case user need them.
                    ModelDataTable <- FitDistribution(DataTable,Scale="Lognormal")
                    CreateGraphProba(DataTable,ModelDataTable,DataTable,DeviceID,Scale="Lognormal",ErrorBand=FALSE,Save=FALSE)
              }
          }

    } else { # case 2, there are no files available
          print("You need to create the export files first!")
    }
    # return(DataTable)
    # Warning are set on again.
    options(warn = oldw)
}


BlackModelization.me <- function(DataTable, DeviceID)
# Modelize the data using Black equation
# Extract the parameters: A, n and Ea
# as well as the lognormal slope
# TTF = A j^(-n) exp(Ea/kT + Scale * Proba)
# Proba in standard deviations
# Data(TTF,Status,Probability,Conditions,Stress,Temperature, Dimension)
{
    # Read the list of device to retrieve the section parameters.
    ListDevice <- try(read.delim("//fsup04/fntquap/Common/Qual/Process_Reliability/Process/amsReliability_R_Package/ListDeviceName.txt"),silent=TRUE)
    #if file is not present, error is returned.
    if (class(ListDevice) == "try-error"){
        print("File //fsup04/fntquap/Common/Qual/Process_Reliability/Process/amsReliability_R_Package/ListDeviceName.txt not found.")
        return(ListDevice)
    } else {

        W <- ListDevice$Width[ListDevice$Device==DeviceID] # micrometers
        H <- ListDevice$Height[ListDevice$Device==DeviceID] # micrometers
        S <- W*H*1E-12 # m^2

        # if S is a positive number different from 0, we can proceed:
        if (is.na(S) || S<=0 ) {
            print(paste("Structure",DeviceID, "is not present in the list. Please fill the list!"))
            # Force an error in the return for BlackAnalysis.
            ModelDataTable <- data.frame()
            as(ModelDataTable,"try-error")
            return(ModelDataTable)
        } else { # we proceed

          # Physical constants
          k <- 1.38E-23 # Boltzmann
          e <- 1.6E-19 # electron charge

          # Remove the units where status is 0
          CleanDataTable <- DataTable[DataTable$Status==1,]

          B.f <- deriv(~log(exp(A)*(Stress*1E-3/1)^(-n)*exp((Ea*1.6E-19)/(1.38E-23*(Temperature+273.15))+Scale*Probability))/log(10),namevec = c('A','Ea','n','Scale'), function.arg = c('A','Ea','n','Scale','Stress', 'Temperature','Probability'))
          y = log10(DataTable$TTF)
          fit.nlmer <- nlmer(log10(y) ~ B.f(A,Ea,n,Scale,Stress, Temperature,Probability) ~ Temperature | Conditions, start=list(nlpars=c(A=30,Ea=0.7,n=2,Scale=0.3)), data=DataTable)
          summary(fit.nlmer)


          # Parameters Extraction
          A <- coef(Model)[1]
          n <- coef(Model)[2]
          Ea <-coef(Model)[3]
          Scale <- coef(Model)[4]
          # Residual Sum of Squares
          RSS <- sum(resid(Model)^2)
          # Total Sum of Squares: TSS <- sum((TTF - mean(TTF))^2))
          TSS <- sum(sapply(split(CleanDataTable[,1],CleanDataTable$Conditions),function(x) sum((x-mean(x))^2)))
          Rsq <- 1-RSS/TSS # R-squared measure
          #print(paste("Size on 150 rows:", format(object.size(Model), unit="Mb")))

          # Using the parameters and the conditions, theoretical distributions are created
          ListConditions <- levels(CleanDataTable$Conditions)

          # Initialisation
          ModelDataTable <- data.frame()
          # y axis points are calculated. (limits 0.01% -- 99.99%) Necessary to have nice confidence bands.
          Proba <- seq(qnorm(0.0001),qnorm(0.9999),0.05)

          for (i in seq_along(ListConditions)){
              # Experimental conditions:
              Condition <- ListConditions[i]
              I <- CleanDataTable$Stress[CleanDataTable$Conditions==Condition][1]
              Temp <- CleanDataTable$Temperature[CleanDataTable$Conditions==Condition][1]  # °C

              # TTF calculation
              TTF <- exp(A)*(I*0.001/S)^(-n)*exp((Ea*e)/(k*(273.15+Temp))+ Proba * Scale)

              # Dataframe creation
              ModelDataTable <- rbind(ModelDataTable, data.frame('TTF'=TTF,'Status'=1,'Probability'=Proba,'Conditions'=Condition,'Stress'=I,'Temperature'=Temp))
          }

          # Drawing of the residual plots
          plot(nlsResiduals(Model))
          # Display of fit results
          print(DeviceID)
          print(summary(Model))
          print(paste("Residual squared sum: ",RSS,sep=""))
          #print(coef(Model))
          #print(sd(resid(Model)))

          # Save in a file
          capture.output(summary(Model),file="fit.txt")
          cat("Residual Squared sum:\t",file="fit.txt",append=TRUE)
          cat(RSS,file="fit.txt",append=TRUE)
          cat("\n \n",file="fit.txt",append=TRUE)
          cat("Experimental Data:",file="fit.txt",append=TRUE)
          cat("\n",file="fit.txt",append=TRUE)
          capture.output(DataTable,file="fit.txt",append=TRUE)
          return(ModelDataTable)
        }
    }
}
