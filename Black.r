# Script collection allowing the extraction of Black's parameters
# using a serie of electromigration experiments.
# September 2015
# Emmanuel Chery

# Required Packages
library('ggplot2')
library('MASS')
library('scales')


Ranking <- function(TTF)
# Fraction estimator calculation
# rk(i)=(i-0.3)/(n+0.4)
# TTF is a vector.
{
    rk <- (rank(TTF)-0.3)/(length(TTF)+0.4)
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


CreateGraph <- function(DataTable, Scale="Lognormale")
# Use the table prepared with CreateDataFrame and create the probability plot.
# Default is Lonormale scale but Weibull is available as an option.
{
    # x scale limits calculation based on the data.
    lim <- range(DataTable[,"TTF"]) # Min of the values is stored in [1] and max in  [2]
    lim.high <- 10^(ceiling(log(lim[2],10)))
    lim.low <- 10^(floor(log(lim[1],10)))
    # Now that we have the limits, we create the graph labels for x axis.
    GraphLabels <- 10^(seq(floor(log(lim[1],10)),ceiling(log(lim[2],10))))

    # Label for y axis
    # Dynamique labels as a function of the minimal probability observed.
    # Minimal proba is 0.01 %

    #  Weibull
    if (Scale == "Weibull") {
        if (DataTable[1,"Probability"]<= Weibit(0.1/100)){ # Case 1: lower than 0.1%
            ListeProba <- c(0.01,0.1,1,5,10,20,30,40,50,63,70,80,90,95,99,99.9,99.99)
        }
        if (DataTable[1,"Probability"]<= Weibit(1/100) && DataTable[1,"Probability"]>= Weibit(0.1/100)){ # Case 2: lower than 1% but higher than 0.1%
            ListeProba <- c(0.1,1,5,10,20,30,40,50,63,70,80,90,95,99,99.9)
        }
        if (DataTable[1,"Probability"] >= Weibit(1/100)) { # Case 3: higher than 1%
            ListeProba <- c(1,5,10,20,30,40,50,63,70,80,90,95,99)
        }
    ProbaNorm <- Weibit(ListeProba/100)

    } else { # Lognormale
        if (DataTable[1,"Probability"]<= qnorm(0.1/100)){ # Case 1: lower than 0.1%
            ListeProba <- c(0.01,0.1,1,5,10,20,30,40,50,60,70,80,90,95,99,99.9,99.99)
        }
        if (DataTable[1,"Probability"]<= qnorm(1/100) && DataTable[1,"Probability"]>= qnorm(0.1/100)){ # Case 2: lower than 1% but higher than 0.1%
            ListeProba <- c(0.1,1,5,10,20,30,40,50,60,70,80,90,95,99,99.9)
        }
        if (DataTable[1,"Probability"] >= qnorm(1/100)) { # Case 3: higher than 1%
            ListeProba <- c(1,5,10,20,30,40,50,60,70,80,90,95,99)
        }
    ProbaNorm <- qnorm(ListeProba/100)
    }

    # We are only going to plot samples where status is '1' (experiment is finished).
    # Table is sorted & conditions stay togeteher.
    CleanTable <- DataTable[DataTable$Status==1,]
    CleanTable <- CleanTable[order(CleanTable$"Conditions"),]

    # Graph creation with CleanTable
    Graph <- ggplot(data=CleanTable, aes(x=TTF, y=Probability, colour=Conditions, shape=Conditions))
    Graph <- Graph + scale_x_log10(limits = c(lim.low,lim.high),breaks = GraphLabels,labels = trans_format("log10", math_format(10^.x)))
    Graph <- Graph + scale_y_continuous(limits=range(ProbaNorm), breaks=ProbaNorm, labels=ListeProba )
    Graph <- Graph + geom_point(size=4)+annotation_logticks(sides='tb')
    print(Graph)
}


StackData <- function(DataTable1, DataTable2)
# Merge 2 DataTable
{
    NewDataTable <- merge(DataTable1, DataTable2, all=TRUE)
    NewDataTable <- NewDataTable[order(NewDataTable$"Conditions"),]
    return(NewDataTable)
}


Modelization <- function(DataTable, Type="Lognormale")
# Using experimental data the theoretical distribution is genrated
# 95% confidence intervals are also generated.
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
    # Generation of a vector for the calculation of the model. 20pts/decades
    x <- 10^seq(log(lim.low,10),log(lim.high,10),0.05)
    # Model calculation with the experimental TTF
    if (Type=="Weibull") { # Weibull
          fit <- fitdistr(DataTable$TTF,"weibull")
          Shape <- fit$estimate[1]  # Beta
          Scale <- fit$estimate[2]  # Characteristic time (t_63%)
          y <- pweibull(x, Shape, Scale)
    } else { # Lognormale
          fit <- fitdistr(DataTable$TTF,"lognormal")
          Scale <- fit$estimate[1]  #   meanlog
          Shape <- fit$estimate[2]  # sdlog
          y <- plnorm(x, Scale, Shape)
    }

ModelDataTable <- data.frame('TTF'=x,'Status'=1,'Probability'=y,'Conditions'=ModelCondition,'Current'=ModelCurrent,'Temperature'=ModelTemperature)
return(ModelDataTable)
}
