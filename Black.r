# Script collection allowing the extraction of Black's parameters
# using a serie of electromigration experiments.
# September 2015
# Emmanuel Chery
# Version 0.8

# Required Packages
library('ggplot2')
library('MASS')
library('scales')
library('grid')



Ranking <- function(TTF)
# Fraction estimator calculation
# rk(i)=(i-0.3)/(n+0.4)
# TTF is a vector.
{
    # ties.method="random" handles identical TTFs and provide a unique ID
    rk <- (rank(TTF, ties.method="random")-0.3)/(length(TTF)+0.4)
}


CalculProbability <- function(Probability, Scale="Lognormale")
# Given a vector Probability of probabilities, the function calculates
# the correspondence in standard deviations for the lognormale case.
# Calculation of the Weibit is made for the Weibull case.
{
  if (Scale=="Weibull") {
      Proba <- ln(-ln(1-Probability)) # Weibull
  } else {
      Proba <- qnorm(Probability) # Lognormal
  }
  return(Proba)
}


Clean <- function(TTF,Status)
# TTF and status are vectors and have the same length.
# Cleaning of the data. Only TTF with a status 1 or 0 are kept.
# Finally TTF are sorted from the smallest to the largest.
{
    TTF <- TTF[Status==0 | Status==1]
    Status <- Status[Status==0 | Status==1]
    Res <- data.frame('TTF'=TTF,'Status'=Status)
    Res <- Res[order(Res$"TTF"),] # Sort TTF
    return(Res)
}


CreateDataFrame <- function(TTF, Status, Condition, Current, Temperature, Scale="Lognormale")
# Creation of the dataframe assembling the TTF, the status of the samples,
# the probability, the condition (stickers for charts),
# the current and the temperature used durng the stress.
# The probability is calculated according to lognormale or Weibull distribution.
# Data are cleaned before probability calculation.

# Data(TTF,Status,Probability,Conditions,Current,Temperature)
{
    CleanedData <- Clean(TTF,Status) # Clean & sort
    rk <- Ranking(CleanedData$TTF) # Fraction estimator calculation
    if (Scale=="Weibull") {
        Proba <- CalculProbability(rk,Scale="Weibull") # Probability calculation Weibull
    } else {
        Proba <- CalculProbability(rk,Scale="Lognormale") # Probability calculation Lognormale
    }
    # Generation of the final data frame
    DataTable <- data.frame('TTF'=CleanedData$TTF,'Status'=CleanedData$Status,'Probability'=Proba,'Conditions'=Condition, 'Current'=Current, 'Temperature'=Temperature)
    return(DataTable)
}


StackData <- function(DataTable1, DataTable2)
# Merge 2 DataTable
{
    NewDataTable <- merge(DataTable1, DataTable2, all=TRUE)
    NewDataTable <- NewDataTable[order(NewDataTable$"Conditions"),]
    return(NewDataTable)
}


Modelization <- function(DataTable, Type="Lognormale")
# Using experimental data the theoretical distribution is generated
# Default is Lonormale scale but Weibull is available as an option.
{
    # Condition, Current and Temperature stickers
    ModelCondition <- DataTable[1,"Conditions"]
    ModelCurrent <- DataTable[1,"Current"]
    ModelTemperature <- DataTable[1,"Temperature"]

    # x axis limits are calculated
    lim <- range(DataTable$TTF)
    lim.high <- 10^(ceiling(log(lim[2],10)))
    lim.low <- 10^(floor(log(lim[1],10)))
    # Generation of a vector for the calculation of the model. 100pts/decades
    x <- 10^seq(log(lim.low,10),log(lim.high,10),0.01)
    # Model calculation with the experimental TTF
    if (Type=="Weibull") { # Weibull
          fit <- fitdistr(DataTable$TTF[DataTable$Status==1],"weibull")
          Shape <- fit$estimate[1]  # Beta
          Scale <- fit$estimate[2]  # Characteristic time (t_63%)
          y <- CalculProbability(pweibull(x, Shape, Scale),"Weibull")
    } else { # Lognormale
          fit <- fitdistr(DataTable$TTF[DataTable$Status==1],"lognormal")
          Scale <- fit$estimate[1]  # meanlog
          Shape <- fit$estimate[2]  # sdlog
          y <- CalculProbability(plnorm(x, Scale, Shape),"Lognormale")
    }

    ModelDataTable <- data.frame('TTF'=x,'Status'=1,'Probability'=y,'Conditions'=ModelCondition,'Current'=ModelCurrent,'Temperature'=ModelTemperature)
    return(ModelDataTable)
}

ErrorEstimation <- function(ExpDataTable, ModelDataTable, ConfidenceValue=0.95)
# Genration of confidence intervals
{
    mZP_Value <- qnorm((1 - ConfidenceValue) / 2)
    CDF <- pnorm(ModelDataTable$Probability)
    sef <- sqrt(CDF * (1 - CDF)/length(ExpDataTable$TTF))
    LowerLimit <- qnorm(CDF - sef * mZP_Value)
    HigherLimit <- qnorm(CDF + sef * mZP_Value)

    ConfidenceDataTable <- data.frame('TTF'=ModelDataTable$TTF,'LowerLimit'=LowerLimit,'HigherLimit'=HigherLimit,'Conditions'=ModelDataTable$Conditions)
    return(ConfidenceDataTable)
}


CreateGraph <- function(ExpDataTable, ModelDataTable, ConfidenceDataTable, Scale="Lognormale")
# Use the table prepared with CreateDataFrame and create the probability plot.
# Default is Lonormale scale but Weibull is available as an option.
{
    # x scale limits calculation based on the data.
    lim <- range(ExpDataTable[,"TTF"]) # Min of the values is stored in [1] and max in  [2]
    lim.high <- 10^(ceiling(log(lim[2],10)))
    lim.low <- 10^(floor(log(lim[1],10)))
    # Now that we have the limits, we create the graph labels for x axis.
    GraphLabels <- 10^(seq(floor(log(lim[1],10)),ceiling(log(lim[2],10))))

    # Label for y axis
    # Dynamique labels as a function of the minimal probability observed.
    # Minimal proba is 0.01 %

    #  Weibull
    if (Scale == "Weibull") {
        if (ExpDataTable[1,"Probability"]<= Weibit(0.1/100)){ # Case 1: lower than 0.1%
            ListeProba <- c(0.01,0.1,1,5,10,20,30,40,50,63,70,80,90,95,99,99.9,99.99)
        }
        if (ExpDataTable[1,"Probability"]<= Weibit(1/100) && ExpDataTable[1,"Probability"]>= Weibit(0.1/100)){ # Case 2: lower than 1% but higher than 0.1%
            ListeProba <- c(0.1,1,5,10,20,30,40,50,63,70,80,90,95,99,99.9)
        }
        if (ExpDataTable[1,"Probability"] >= Weibit(1/100)) { # Case 3: higher than 1%
            ListeProba <- c(1,5,10,20,30,40,50,63,70,80,90,95,99)
        }
    ProbaNorm <- Weibit(ListeProba/100)

    } else { # Lognormale
        if (ExpDataTable[1,"Probability"]<= qnorm(0.1/100)){ # Case 1: lower than 0.1%
            ListeProba <- c(0.01,0.1,1,5,10,20,30,40,50,60,70,80,90,95,99,99.9,99.99)
        }
        if (ExpDataTable[1,"Probability"]<= qnorm(1/100) && ExpDataTable[1,"Probability"]>= qnorm(0.1/100)){ # Case 2: lower than 1% but higher than 0.1%
            ListeProba <- c(0.1,1,5,10,20,30,40,50,60,70,80,90,95,99,99.9)
        }
        if (ExpDataTable[1,"Probability"] >= qnorm(1/100)) { # Case 3: higher than 1%
            ListeProba <- c(1,5,10,20,30,40,50,60,70,80,90,95,99)
        }
    ProbaNorm <- qnorm(ListeProba/100)
    }

    # We are only going to plot samples where status is '1' (experiment is finished).
    # Table is sorted & conditions stay togeteher.
    CleanExpTable <- ExpDataTable[ExpDataTable$Status==1,]
    CleanExpTable <- CleanExpTable[order(CleanExpTable$"Conditions"),]

    # Graph creation with CleanTable
    Graph <- ggplot(data=CleanExpTable, aes(x=TTF, y=Probability, colour=Conditions, shape=Conditions))
    # box around chart + background
    Graph <- Graph + theme_linedraw() + theme(panel.background = element_rect(fill="gray90", color="black"))
    # Grid definitions
    Graph <- Graph + theme(panel.grid.major = element_line(colour="white", size=0.25, linetype=1))
    Graph <- Graph + theme(panel.grid.minor = element_line(linetype=0, colour="white", size = 0.25))
    # Definition of scales
    Graph <- Graph + scale_x_log10(limits = c(lim.low,lim.high),breaks = GraphLabels,labels = trans_format("log10", math_format(10^.x)))
    Graph <- Graph + scale_y_continuous(limits=range(ProbaNorm), breaks=ProbaNorm, labels=ListeProba )
    # Controled symbol list -- Max is 20 conditions on the chart.
    Graph <- Graph + scale_shape_manual(values=c(19,15,17,16,19,15,17,16,19,15,17,16,19,15,17,16,19,15,17,16))
    Graph <- Graph + geom_point(size=4)+annotation_logticks(sides='tb')
    # Add the theoretical model
    Graph <- Graph + geom_line(data=ModelDataTable, aes(color=Conditions), size=0.8)
    # Add the confidence intervals
    Graph <- Graph + geom_line(data=ConfidenceDataTable, aes(x=TTF, y=LowerLimit, color=Conditions), linetype="dashed", size=0.8)
    Graph <- Graph + geom_line(data=ConfidenceDataTable, aes(x=TTF, y=HigherLimit, color=Conditions), linetype="dashed",size=0.8)
    # Font size & x/y titles...
    Graph <- Graph + xlab("Time to Failure (s)") + ylab("Probability (%)")
    Graph <- Graph + theme(axis.title.x = element_text(face="bold", size=16))
    Graph <- Graph + theme(axis.title.y = element_text(face="bold", size=16))
    # legend size
    Graph <- Graph + theme(legend.title = element_text(size=14, face="bold"))
    Graph <- Graph + theme(legend.text = element_text(size = 12))
    # Box around legend
    Graph <- Graph + theme(legend.background = element_rect())
    Graph <- Graph + theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    #Box around the conditions in legend
    Graph <- Graph + theme(legend.key = element_rect(fill="gray90", colour = "black", linetype=0))
    # Label/ticks size
    Graph <- Graph + theme(axis.text.x = element_text(face="bold", size=16))
    Graph <- Graph + theme(axis.text.y = element_text(size=16))
    Graph <- Graph + theme(axis.ticks.length = unit(-.25, "cm"), axis.ticks.margin=unit(0.4, "cm"))

    print(Graph)
}


ReadData <- function(FileName, Scale="Lognormale")
# Read the file exportfile and store it in a dataframe
{
    ResTable <- read.delim(FileName)
    TTF <- ResTable["Lifetime.s."]
    Status <- ResTable["Failed"]
    Current <- ResTable["Istress"]
    Temperature <- ResTable["Temp"]
    Condition <- paste(ResTable[,"Istress"],"mA/",ResTable[,"Temp"],"C",sep="") #paste(ResTable[,5],"mA/",ResTable[,8],"C",sep="")

    if (Scale=="Weibull") {
        ExpDataTable <- CreateDataFrame(TTF, Status, Condition, Current, Temperature, Scale="Weibull")
    } else {
        ExpDataTable <- CreateDataFrame(TTF, Status, Condition, Current, Temperature, Scale="Lognormale")
    }
    # Istress and Temp are dataframe and keep their names. We force the new names here.
    names(ExpDataTable) <- c("TTF", "Status", "Probability", "Conditions", "Current", "Temperature")
    return(ExpDataTable)
}

BlackAnalysis <- function(Scale="Lognormale")
# Open all the exportfiles from the workfolder
{
    ListFiles = list.files(pattern="*exportfile.txt")
    # case 1, there are one or several files available
    if (length(ListFiles) != 0){
          # Import the first file to create the 3 dataframes
          DataTable <- ReadData(ListFiles[1])
          ModelDataTable <- Modelization(DataTable)
          ErrorDataTable <- ErrorEstimation(DataTable,ModelDataTable)

          # Let's now check if other files are available
          if (length(ListFiles) > 1){
                # loop to open all the files and stack them in the dataframe
                for (i in 2:length(ListFiles)){
                    NewDataTable <- ReadData(ListFiles[i])
                    NewModelDataTable <- Modelization(NewDataTable)
                    NewErrorDataTable <- ErrorEstimation(NewDataTable,NewModelDataTable)

                    # Merging the tables
                    DataTable <- StackData(DataTable,NewDataTable)
                    ModelDataTable <- StackData(ModelDataTable,NewModelDataTable)
                    ErrorDataTable <- StackData(ErrorDataTable,NewErrorDataTable)
                }
          }
    } else { # case 2, there are no files available
          print("You need to create the export files first!")
    }
    CreateGraph(DataTable,ModelDataTable,ErrorDataTable)
}
