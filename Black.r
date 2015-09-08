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

# Data(TTF,Status,Probability,Condition,Current,Temperature)
{
    CleanedData <- Clean(TTF,Status) # Clean & sort
    rk <- Ranking(CleanedData$TTF) # Fraction estimator calculation
    if (Scale=="Weibull") {
      Proba <- CalculProbability(rk,Scale="Weibull") # Probability calculation Weibull
    } else {
      Proba <- CalculProbability(rk,Scale="Lognormale") # Probability calculation Lognormale
    }
    # Generation of the final data frame
    Data <- data.frame('TTF'=CleanedData$TTF,'Status'=CleanedData$Status,'Probability'=Proba,'Conditions'=Condition, 'Current'=Current, 'Temperature'=Temperature)
    return(Data)
}


Plot <- function(Table, Scale="Lognormale")
# Prend la table de données issue de ArrangeXxxxx et trace le graphe correspondant
# Le paramètre facultatif Scale permet de tracer une échelle Weibull.
{
    # Calcul des limites pour le graphe
    lim <- range(Table[,1])
    lim.high <- 10^(ceiling(log(lim[2],10)))
    lim.low <- 10^(floor(log(lim[1],10)))
    # Basé sur les limites, le calculs des etiquettes de l'axe x.
    GraphLabels <- 10^(seq(floor(log(lim[1],10)),ceiling(log(lim[2],10))))

    # Calcul des etiquettes de l'axe y
    # Etiquettes dynamique en fonction de la probabilité minimale

    # Cas Weibull
    if (Scale == "Weibull") {
        if (Table[1,3]<= Weibit(0.1/100)){ # Cas 1: inférieur à 0.1%
            ListeProba <- c(0.01,0.1,1,5,10,20,30,40,50,63,70,80,90,95,99,99.9,99.99)
        }
        if (Table[1,3]<= Weibit(1/100) && Table[1,3]>= Weibit(0.1/100)){ # Cas 2: inférieur à 1% mais supérieur à 0.1%
            ListeProba <- c(0.1,1,5,10,20,30,40,50,63,70,80,90,95,99,99.9)
        }
        if (Table[1,3] >= Weibit(1/100)) { # Cas 3: Supérieur à 1%
            ListeProba <- c(1,5,10,20,30,40,50,63,70,80,90,95,99)
        }
    ProbaNorm <- Weibit(ListeProba/100)

    } else { # Cas distribution lognormale
        if (Table[1,3]<= qnorm(0.1/100)){ # Cas 1: inférieur à 0.1%
            ListeProba <- c(0.01,0.1,1,5,10,20,30,40,50,60,70,80,90,95,99,99.9,99.99)
        }
        if (Table[1,3]<= qnorm(1/100) && Table[1,3]>= qnorm(0.1/100)){ # Cas 2: inférieur à 1% mais supérieur à 0.1%
            ListeProba <- c(0.1,1,5,10,20,30,40,50,60,70,80,90,95,99,99.9)
        }
        if (Table[1,3] >= qnorm(1/100)) { # Cas 3: Supérieur à 1%
            ListeProba <- c(1,5,10,20,30,40,50,60,70,80,90,95,99)
        }
    ProbaNorm <- qnorm(ListeProba/100)
    }

    # Création du sous ensemble avec les temps où le status est à 1.
    # La table est triée pour que les conditions soient continues.
    Table <- Table[Table$Status==1,]
    Table <- Table[order(Table$"Conditions"),]

    # Création du graph
    # Seul les temps ou le status est à 1 sont affichés.
    Plot <- ggplot(data=Table, aes(x=TTF, y=Probability, colour=Conditions, shape=Conditions))
    Plot <- Plot + scale_x_log10(limits = c(lim.low,lim.high),breaks = GraphLabels,labels = trans_format("log10", math_format(10^.x)))
    Plot <- Plot + scale_y_continuous(limits=range(ProbaNorm), breaks=ProbaNorm, labels=ListeProba )
    Plot <- Plot + geom_point(size=4)+annotation_logticks(sides='tb')
    print(Plot)
}


StackData <- function(Table1, Table2)
# Associe 2 dataframe à la suite
{
    Data <- merge(Table1, Table2, all=TRUE)
    Data <- Data[order(Data$"Conditions"),]
    return(Data)
}


Modelization <- function(Data, Condition, Type="Lognormale")
# Genration de la distribution theorique et les intervalles de confience
# à partir de données expérimentales.

# Calcul des limites pour le graphe
lim <- range(Data)
lim.high <- 10^(ceiling(log(lim[2],10)))
lim.low <- 10^(floor(log(lim[1],10)))
x <- 10^seq(log(lim.low,10),log(lim.high,10),0.05) # serie de points pour le modèle

{
    if (Type=="Weibull") {
        fit <- fitdistr(Data,"weibull")
        Shape <- fit$estimate[1]  # Beta
        Scale <- fit$estimate[2]  # Charac time
        y <- pweibull(x, Shape, Scale)

    } else { # All other case are considered as Lognormale
        fit <- fitdistr(Data,"lognormal")
        Scale <- fit$estimate[1]  #   meanlog
        Shape <- fit$estimate[2]  # sdlog
        y <- plnorm(x, Scale, Shape)
    }

Model <- data.frame('TTF'=x,'Status'=1,'Probability'=y,'Conditions'=Condition)
}
