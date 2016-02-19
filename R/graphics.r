################################################################################
###                                                                          ###
###    INFORMATIONS                                                          ###
###    ---------------------------------                                     ###
###                                                                          ###
###       PACKAGE NAME        amsReliability                                 ###
###       MODULE NAME         graphics.r                                     ###
###       VERSION             0.9                                            ###
###                                                                          ###
###       AUTHOR              Emmanuel Chery                                 ###
###       MAIL                emmanuel.chery@ams.com                         ###
###       DATE                2016/02/08                                     ###
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
###       This module includes the graphics functions.                       ###
###                                                                          ###
###                                                                          ###
###    FUNCTIONS                                                             ###
###    ---------------------------------                                     ###
###                                                                          ###
###       CreateGraph               In charge of data representation         ###
###                                                                          ###
###                                                                          ###
################################################################################


CreateGraph <- function(ExpDataTable, ModelDataTable = NULL , ConfidenceDataTable = NULL, Title="", Scalex = "Log", Scaley="Lognormal", ErrorBands=TRUE, Save=TRUE)
# Use the table prepared with CreateDataFrame and create the probability plot.
# Default y scale is Lonormale scale but Weibull and degradation (%) are available as an option.
# Default x scale is log but linear (lin) is available in option.
{
    # x scale limits calculation based on the data.
    lim <- ExtractLimits(ExpDataTable$TTF[ExpDataTable$Status==1], minDecades=1)
    probaMin <- min(ExpDataTable$Probability)
    # Label for y axis
    # Dynamique labels as a function of the minimal probability observed.
    # Minimal proba is 0.01 %

    # # Case 0: Proba min is above 1%
    # if (Scaley == "Weibull"){ # Weibull requires 63% and details in low %
    #     ListeProba <- c(1,2,3,5,10,20,30,40,50,63,70,80,90,95,99)
    # } else { # Lognormal scale is symetric.
    #     ListeProba <- c(1,5,10,20,30,40,50,60,70,80,90,95,99)
    # }
    #
    # MinProba <- min(ExpDataTable$Probability)
    #
    # if (MinProba <= CalculProbability(1/100,Scaley)){ # Case 1: lower than 1%
    #     ListeProba <- c(0.1,ListeProba, 99.9)
    # }
    # if (MinProba <= CalculProbability(0.1/100,Scaley)){ # Case 2: lower than 0.1%
    #     ListeProba <- c(0.01,ListeProba, 99.99)
    # }
    #
    # # Probability vector used to draw y axis.
    # ProbaNorm <- CalculProbability(ListeProba/100,Scaley)

    # We are only going to plot samples where status is '1' (experiment is finished).
    # Table is sorted & conditions stay togeteher.
    CleanExpTable <- ExpDataTable[ExpDataTable$Status==1,]
    CleanExpTable <- CleanExpTable[order(CleanExpTable$"Conditions"),]

    # Graph creation with CleanTable
    Graph <- ggplot(data=CleanExpTable, aes(x=TTF, y=Probability, colour=Conditions, shape=Conditions))
    # Add default options
    Graph <- GraphBase(Graph, Title)

    # Definition of scales
    #Graph <- CreateScale.x(Graph, CleanExpTable$TTF, Scale = "Log")
    #Graph <- CreateAxisLog(Graph, scaleLimits = lim, axis = "x")
    Graph <- CreateAxisLin(Graph, scaleLimits = lim, axis = "x")
    # Graph <- Graph + scale_x_log10(limits = c(lim.low,lim.high),breaks = GraphLabels,labels = trans_format("log10", math_format(10^.x)), minor_breaks=trans_breaks(faceplant1, faceplant2, n=length(MinorTicks)))
    # Graph <- Graph + scale_y_continuous(limits=range(ProbaNorm), breaks=ProbaNorm, labels=ListeProba)
    Graph <- CreateAxisLognormal(Graph, probaMin)
    # Grid definitions
    Graph <- Graph + theme(panel.grid.major = element_line(colour="white", size=0.25, linetype=1))
    Graph <- Graph + theme(panel.grid.minor = element_line(linetype=2, colour="white", size = 0.25))
    Graph <- Graph + theme(panel.grid.minor.y = element_line(linetype=0, colour="white", size = 0.25))

    Graph <- Graph + geom_point(size=4)#+annotation_logticks(sides='tb')

    # Add the theoretical model
    if (!is.null(ModelDataTable)){
        Graph <- Graph + geom_line(data=ModelDataTable, aes(color=Conditions), size=0.8)
    }
    # Add the confidence intervals
    if (!is.null(ConfidenceDataTable) & ErrorBands==TRUE) {
        Graph <- Graph + geom_line(data=ConfidenceDataTable, aes(x=TTF, y=LowerLimit, color=Conditions), linetype="dashed", size=0.8)
        Graph <- Graph + geom_line(data=ConfidenceDataTable, aes(x=TTF, y=HigherLimit, color=Conditions), linetype="dashed",size=0.8)
    }

    # Font size & x/y titles...
    Graph <- Graph + xlab("Time to Failure (s)") + ylab("Probability (%)")

    print(Graph)

    # Save as png or pdf
    if (Save == TRUE){
        GraphSave(Title, Extension ="png")
    }
}


GraphBase <- function(Graph, Title)
# Add default parameters to a graph
# background, legend, shape and color of points
{
    # box around chart + background
    Graph <- Graph + theme_linedraw() + theme(panel.background = element_rect(fill="gray90", color="black"))
    # Controled symbol list -- Max is 20 conditions on the chart.
    Graph <- Graph + scale_shape_manual(values=c(19,15,17,16,19,15,17,16,19,15,17,16,19,15,17,16,19,15,17,16))
    Graph <- Graph + scale_colour_manual(values = c("#d53e4f","#3288bd","#66a61e","#f46d43","#e6ab02","#8073ac","#a6761d","#666666","#bc80bd","#d53e4f","#3288bd","#66a61e","#f46d43","#e6ab02","#8073ac","#a6761d","#666666","#bc80bd","#d53e4f","#3288bd")) # "#5e4fa2" ,"#66c2a5", "#fec44f",
    # legend size
    Graph <- Graph + theme(legend.title = element_text(size=14, face="bold"))
    Graph <- Graph + theme(legend.text = element_text(size = 12))
    # Box around legend
    Graph <- Graph + theme(legend.background = element_rect())
    Graph <- Graph + theme(legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))
    #Box around the conditions in legend
    Graph <- Graph + theme(legend.key = element_rect(fill="gray90", colour = "black", linetype=0))
    # Label/ticks size
    Graph <- Graph + theme(axis.text.x = element_text(face="bold", size=16, margin=margin(0.4,0,0,0, "cm")))
    Graph <- Graph + theme(axis.text.y = element_text(size=16, margin=margin(0,0.4,0,0.2, "cm")))
    Graph <- Graph + theme(axis.ticks.length = unit(-0.25, "cm"))#, axis.ticks.margin = unit(0.4, "cm")) #Depreciated see margin above.
    # Add a title
    Graph <- Graph + ggtitle(Title)
    Graph <- Graph + theme(plot.title = element_text(face="bold", size=18))
    # Font size & x/y titles...
    Graph <- Graph + theme(axis.title.x = element_text(face="bold", size=16))
    Graph <- Graph + theme(axis.title.y = element_text(face="bold", size=16))

    return(Graph)
}


GraphSave <- function(Title, Extension="png")
# Save a graph
{
    if (Title != ""){
        ggsave(filename=paste(Title, Extension ,sep="."),dpi=300)
        #ggsave(filename=paste(Title,"pdf",sep="."))
    } else {
        ggsave(filename=paste("Chart", Extension, sep="."),dpi=300)
        #ggsave(filename="Chart.pdf")
    }
}


GraphTargetLines <- function(Graph, x=NULL, y=NULL, Colorx="red", Typex = 2, Colory="red", Typey = 2)
# Add target lines to a graph
{
    if (!is.null(x) & is.numeric(x)){
        Graph <- Graph + geom_vline(xintercept = x, color = Colorx, linetype = Typex)
    }

    if (!is.null(y) & is.numeric(x)){
        Graph <- Graph + geom_hline(xintercept = y, color = Colory, linetype = Typey)
    }

    return(Graph)
}


ExtractLimits <- function(Data, minDecades=3)
# Return the limits of the Data
#  Add necessary decades to follow the minimal number of decades requested.
{
    lim <- range(Data) # Min of the values is stored in [1] and max in  [2]
    lim.high <- 10^(ceiling(log(lim[2],10)))
    lim.low <- 10^(floor(log(lim[1],10)))

    nbDecades <- log10(lim.high) - log10(lim.low)
    diff <- minDecades - nbDecades

    # In case the number of minimal decades is not reached, we add decades at both ends
    if ( nbDecades < minDecades ) {
        # We estimate where the data are closer to the edge in order to add the aditional decade on this side.
        # (Odd case)
        if ((log10(lim[1]) - log10(lim.low)) < (log10(lim.high) - log10(lim[2]))){ # additional decade on the left.
            lim.low <- lim.low / 10^(diff %/% 2 + diff %% 2)
            lim.high <- lim.high * 10^(diff %/% 2)
        } else { # aditional decade on the right
            lim.low <- lim.low / 10^(diff %/% 2 )
            lim.high <- lim.high * 10^(diff %/% 2 + diff %% 2)
        }
    }
    return(c(lim.low, lim.high))
}


CreateScale.x <- function(Graph, Data, Scale = "Log")
{
    if (Scale == "Lin")
    {
        scaleLimits <- ExtractLimits(Data, minDecades=1)
        lim.low <- scaleLimits[1]
        lim.high <- scaleLimits[2]
        Graph <- Graph + scale_x_continuous(limits = c(lim.low,lim.high),breaks = GraphLabels,labels = trans_format("log10", math_format(10^.x)), minor_breaks=trans_breaks(faceplant1, faceplant2, n=length(MinorTicks)))
    } else { # log scale
        scaleLimits <- ExtractLimits(Data, minDecades=3)
        lim.low <- scaleLimits[1]
        lim.high <- scaleLimits[2]
        # Now that we have the limits, we create the graph labels for x axis.
        GraphLabels <- 10^(seq(log10(lim.low),log10(lim.high)))

        # Now we create the minor ticks
        ind.lim.high <- log10(lim.high)
        ind.lim.low <- log10(lim.low)
        MinorTicks <- rep(seq(1,9), ind.lim.high - ind.lim.low ) * rep(10^seq(ind.lim.low, ind.lim.high-1), each=9)

        # Function used to calculate the distance between ticks for logscale. See line 166:
        # minor_breaks=trans_breaks(faceplant1, faceplant2, n=length(MinorTicks)))
        faceplant1 <- function(x) {
            return (c(x[1]*10^.25, x[2]/10^.25))
        }

        faceplant2 <- function(x) {
            return (MinorTicks)
        }

        Graph <- Graph + scale_x_log10(limits = c(lim.low,lim.high), breaks = GraphLabels, labels = trans_format("log10", math_format(10^.x)), minor_breaks=trans_breaks(faceplant1, faceplant2, n=length(MinorTicks)))
    }
    return(Graph)
}


CreateAxisLog <- function(Graph, scaleLimits, axis = "x")
# Create a log axis.
# Limits <- c(lim.low, lim.high)
# axis: x or y
{
    lim.low <- scaleLimits[1]
    lim.high <- scaleLimits[2]
    graphLabels <- 10^(seq(log10(lim.low),log10(lim.high)))

    # Now we create the minor ticks
    minorTicks <- rep(seq(1,9), log10(lim.high) - log10(lim.low) ) * rep(10^seq(log10(lim.low), log10(lim.high)-1), each=9)

    # Function used to calculate the distance between ticks for logscale. See line 166:
    # minor_breaks=trans_breaks(faceplant1, faceplant2, n=length(MinorTicks)))
    faceplant1 <- function(x) {
        return (c(x[1]*10^.25, x[2]/10^.25))
    }

    faceplant2 <- function(x) {
        return (minorTicks)
    }

    if (axis == "x"){
        Graph <- Graph + scale_x_log10(limits = c(lim.low,lim.high), breaks = graphLabels, labels = trans_format("log10", math_format(10^.x)), minor_breaks=trans_breaks(faceplant1, faceplant2, n=length(minorTicks)))
    } else if (axis == "y"){
        Graph <- Graph + scale_y_log10(limits = c(lim.low,lim.high), breaks = graphLabels, labels = trans_format("log10", math_format(10^.x)), minor_breaks=trans_breaks(faceplant1, faceplant2, n=length(minorTicks)))
    }
    return(Graph)
}

CreateAxisLin <- function(Graph, scaleLimits, axis = "x")
# Create a lin axis.
# Limits <- c(lim.low, lim.high) # Not used in the current implementation
# axis: x or y
{
    lim.low <- scaleLimits[1]
    lim.high <- scaleLimits[2]

    if (axis == "x"){
        Graph <- Graph + scale_x_continuous(limits = NULL, breaks = waiver(), labels = waiver(), minor_breaks= waiver())

    } else if (axis == "y"){
        Graph <- Graph + scale_y_continuous(limits = NULL, breaks = waiver(), labels = waiver(), minor_breaks= waiver())
    }
    return(Graph)
}

CreateAxisWeibull <- function(Graph, minProba)
# Create a Weibull scale on axis y
# minProba defines the minimal probability being displayed.
# minProba is given in weibit
{
    minProba.ind <- min(0, floor(log10( (1-exp(-exp( minProba)))  *100)))
    ListeProba <- c( 10^seq(minProba.ind,0),2,3,5,10,20,30,40,50,63,70,80,90,95, (100 - 10^seq(0 , minProba.ind)) )
    ProbaNorm <- CalculProbability(ListeProba/100,"Weibull")

    Graph <- Graph + scale_y_continuous(limits=range(ProbaNorm), breaks=ProbaNorm, labels=ListeProba)
    return(Graph)
}


CreateAxisLognormal <- function(Graph, minProba)
# Create a lognormal scale on axis y
# minProba defines the minimal probability being displayed.
# minProba is given in standard deviations.
{
    minProba.ind <- min(0, floor(log10(pnorm(minProba)*100)))
    ListeProba <- c( 10^seq(minProba.ind,0),5,10,20,30,40,50,60,70,80,90,95, (100 - 10^seq(0 , minProba.ind)) )
    ProbaNorm <- CalculProbability(ListeProba/100,"Lognormal")

    Graph <- Graph + scale_y_continuous(limits=range(ProbaNorm), breaks=ProbaNorm, labels=ListeProba)
    return(Graph)
}
