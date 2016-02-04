################################################################################
###                                                                          ###
###    INFORMATIONS                                                          ###
###    ---------------------------------                                     ###
###                                                                          ###
###       MODULE NAME         resistor.r                                     ###
###       VERSION             0.1                                            ###
###                                                                          ###
###       AUTHOR              Emmanuel Chery                                 ###
###       MAIL                emmanuel.chery@ams.com                         ###
###       DATE                2016/01/18                                     ###
###       PLATFORM            Windows 7 & Gnu/Linux 3.16                     ###
###       R VERSION           R 3.2.3                                        ###
###                                                                          ###
###    DESCRIPTION                                                           ###
###    ---------------------------------                                     ###
###                                                                          ###
###       This module includes functions used to automatize export           ###
###    files creation for the resistor electromigration experiments.         ###
###                                                                          ###
###                                                                          ###
################################################################################


ResistorExportFiles <- function(lot = "C18051", wafer = "W5")
# Exportfile creation using the DEG2.0XX files and the TCR file.
# Experimental conditions are given in a Setup.txt file.
{

    listFile2Read <- list.files(pattern="DEG2")

    # Length of the files to read. All the same length as the XP are run on the same tool.
    lengthFile2Read <- length(read.delim("DEG2.001",sep=" ", header=FALSE,na.strings="NA")[,1])

    #Read the setup condition from file Setup.txt
    setupConditions <- read.delim("Setup.txt")

    deviceList <- setupConditions$Device
    temp <- setupConditions$Temp[1]
    stressList <- setupConditions$Stress
    lengthList <- setupConditions$Length
    widthList <- setupConditions$Width
    lowIndiceList <- setupConditions$IndLow
    highIndiceList <- setupConditions$IndHigh

    # Read the TCR file where the initial resistor at stress temperature is stored.
    tcrFile <- read.delim("TCR.EM1", skip = 2)
    # Cleaning to keep only device 4 (calculated value, current indep)
    tcrFile <- tcrFile[tcrFile$Device == 4, ]


    for (i in seq_along(deviceList)){
        device <- deviceList[i]
        stress <- stressList[i]
        length <- lengthList[i]
        width <- widthList[i]
        indLow <-  lowIndiceList[i]
        indHigh <- highIndiceList[i]

        # Name of the file where the results are stored
        fileName <- paste(paste(lot, wafer, device, temp, paste(stress,"mA",sep=""), sep="_"),"txt",sep=".")
        # Experiment name with principal information
        ExpName <- paste(device,"EM",paste(temp,"C",sep=""),paste(stress,"mA",sep=""),sep="_")

        # Initialisation Main data
        resultTable <- data.frame()
        # Initialisation table used in mean degradation calculation
        DeltaRTable <- data.frame(row.names=1:lengthFile2Read)
        # Initialisation table used in mean R calculation
        RTable <- data.frame(row.names=1:lengthFile2Read)

        deviceRef <- 1

        for (j in indLow: indHigh){
            # Read the device dedicated file from MIRA
            tempData <- read.delim(listFile2Read[j],sep=" ", header=FALSE,na.strings=c("NA","1.0000000E+12",1.0000000E+12))
            # Retrive the initial resistance (before stress)
            refResistance <- tcrFile[tcrFile$Pkg == j ,5]

            # Check if unit was good. if yes, results are available
            if (length(refResistance) != 0 && refResistance != 0 && !is.na(tempData[,4])){
                # DeltaR calculation
                deltaR <- (tempData[,4] - refResistance)/refResistance
                # Creation of the global result data.frame
                tempTable <- data.frame("XPName"=ExpName,"GroupName"=device,"DevName"= deviceRef,"Version"=1,"Cycle"=seq(1:length(tempData[,1])) ,
                                        "Time"=tempData[,2],"R"=tempData[,4], "DR"=deltaR, "Stress"=stress, "Temp"=temp, "Length"=length, "Width"= width)
                # same for DR table and R table.
                DeltaRTable <- cbind(DeltaRTable,deltaR)
                RTable <- cbind(RTable,tempData[,4])

                # Cleaning of Error code on main table
                #tempTable <- tempTable[tempTable$R < 1E8, ]
                tempTable <- tempTable[!is.na(tempTable$R),]
                resultTable <- rbind(resultTable, tempTable)
            }

            deviceRef <- deviceRef + 1
        }

        # Search of the first device failing. Statistic calculation will
        # not be performed further, as this would not have sense.
        minLength <- min(apply(!is.na(RTable),2,sum))
        # Resize tables
        DeltaRTable <- DeltaRTable[1:minLength,]
        RTable <- RTable[1:minLength,]
        # Median calcultation
        medianR <- apply(RTable,1,median,na.rm=TRUE)
        medianDeg <- apply(DeltaRTable,1,median,na.rm=TRUE)

        # add the median degradation as a device at the end of the main table
        resultTable <- rbind(resultTable, data.frame("XPName"=ExpName,"GroupName"=device,"DevName"= "Median","Version"=1,"Cycle"=seq(1:minLength),
                    "Time"=tempData[1:minLength,2],"R"=medianR, "DR"=medianDeg, "Stress"=stress, "Temp"=temp, "Length"=length, "Width"= width))

        # Save in a file
        cat("[DATA]\n", file=fileName)
        cat("ExpName\tGroupName\tDevName\tVersion\tCycle\tTGES[S]\tR(Ohm)\tDeltaR(%)\tStress\tTemperature (C)\tLength (um)\tWidth (um)\n", file=fileName, append=TRUE)
        cat("Char_50\tChar_50\tChar_50\tBigInt\tBigInt\tDouble\tDouble\tDouble\tDouble\tDouble\tDouble\tDouble\n", file=fileName, append=TRUE)
        write.table(resultTable,file=fileName, sep="\t", append=TRUE, quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}




ReadDataResistor <- function(listFiles)
{

    dataTable <- data.frame()

    for (file in listFiles){
        openFile <- read.delim(file, skip=3, header=FALSE)
        names(openFile) <- c("ExpName","GroupName","DevName","Version","Cycle","Time","R","DeltaR","Stress","Temperature","Length","Width")

        Time <- openFile$Time
        DeviceNum <- openFile$DevName
        Res <- openFile$R
        DeltaRes <- openFile$DeltaR
        Stress <- openFile$Stress
        Temp <- openFile$Temperature
        Length <- openFile$Length
        Width <- openFile$Width
        Conditions <- paste(Stress,"mA/",Temp,"°C",sep="")
        DeviceName <- openFile$GroupName

        tempDataFrame <- data.frame(Time, Res, DeltaRes, DeviceName, DeviceNum, Conditions,
                                    Stress, Temp, Length, Width)


        dataTable <- rbind(dataTable, tempDataFrame)
    }
    return(dataTable)
}


CreateGraphDeg <- function(ExpDataTable,  Title="",  ErrorBands=TRUE, Save=TRUE)
# Use the table prepared with ReadData function familly and create the degradation plot.
# Default is Loglog scale.
{



    # x scale limits calculation based on the data.
    # lim <- range(ExpDataTable$TTF[ExpDataTable$Status==1]) # Min of the values is stored in [1] and max in  [2]
    # lim.high <- 10^(ceiling(log(lim[2],10)))
    # lim.low <- 10^(floor(log(lim[1],10)))
    # 3 decades minimum are needed for a good looking chart.
    # In case the distribution is only in 1 decade, we add a decade at both ends
    # if ((log10(lim.high) - log10(lim.low)) == 1 ) {
    #     lim.high <- lim.high * 10
    #     lim.low <- lim.low / 10
    # # if we have already tw0 decades, we add one decade in the area where the data are closer to the edge
    # } else if ((log10(lim.high) - log10(lim.low)) == 2) {
    #     if ((log10(lim[1]) - log10(lim.low)) < (log10(lim.high) - log10(lim[2]))){
    #         lim.low <- lim.low / 10
    #     } else {
    #         lim.high <- lim.high * 10
    #     }
    # }

    # Now that we have the limits, we create the graph labels for x axis.
    # GraphLabels <- 10^(seq(log10(lim.low),log10(lim.high)))
    # Now we create the minor ticks
    # ind.lim.high <- log10(lim.high)
    # ind.lim.low <- log10(lim.low)
    # MinorTicks <- rep(seq(1,9), ind.lim.high - ind.lim.low ) * rep(10^seq(ind.lim.low, ind.lim.high-1), each=9)
    GraphLabelsx <- 10^(seq(log10(10),log10(1E9)))
    GraphLabelsy <- 10^(seq(log10(1E-2),log10(1E2)))
    MinorTicksx <- rep(seq(1,9), 9 - 1 ) * rep(10^seq(1, 9-1), each=9)
    MinorTicksy <- rep(seq(1,9), 2 + 2 ) * rep(10^seq(-2, 2-1), each=9)

    # Function used to calculate the distance between ticks for logscale. See line 166:
    # minor_breaks=trans_breaks(faceplant1, faceplant2, n=length(MinorTicks)))
    faceplant1 <- function(x) {
        return (c(x[1]*10^.25, x[2]/10^.25))
    }

    faceplant2 <- function(x) {
        return (MinorTicksx)
    }

    faceplant3 <- function(x) {
        return (MinorTicksy)
    }
    #############################

    # Graph creation with CleanTable
    Graph <- ggplot(data=ExpDataTable, aes(x=Time, y=DeltaRes*100, colour=Conditions, shape=Conditions))
    # box around chart + background
    Graph <- Graph + theme_linedraw() + theme(panel.background = element_rect(fill="gray90", color="black"))
    # Definition of scales
    # Graph <- Graph + scale_x_log10(limits = c(lim.low,lim.high),breaks = GraphLabels,labels = trans_format("log10", math_format(10^.x)), minor_breaks=trans_breaks(faceplant1, faceplant2, n=length(MinorTicks)))
    Graph <- Graph + scale_x_log10(limits = c(10,1E9),breaks = GraphLabelsx,labels = trans_format("log10", math_format(10^.x)), minor_breaks=trans_breaks(faceplant1, faceplant2, n=length(MinorTicksx)))
    # Graph <- Graph + scale_y_continuous(limits=range(ProbaNorm), breaks=ProbaNorm, labels=ListeProba)
    # Graph <- Graph + scale_y_continuous(limits=c(1E-3,100), breaks=c, labels=ListeProba)
     Graph <- Graph + scale_y_log10(limits=c(1E-2,100), breaks=GraphLabelsy, labels=trans_format("log10", math_format(10^.x)), minor_breaks=trans_breaks(faceplant1, faceplant3, n=length(MinorTicksy)))
    # Grid definitions
    Graph <- Graph + theme(panel.grid.major = element_line(colour="white", size=0.25, linetype=1))
    Graph <- Graph + theme(panel.grid.minor = element_line(linetype=2, colour="white", size = 0.25))
    Graph <- Graph + theme(panel.grid.minor.y = element_line(linetype=2, colour="white", size = 0.25))
    # Controled symbol list -- Max is 20 conditions on the chart.
    Graph <- Graph + scale_shape_manual(values=c(19,15,17,16,19,15,17,16,19,15,17,16,19,15,17,16,19,15,17,16))
    Graph <- Graph + scale_colour_manual(values = c("#d53e4f","#3288bd","#66a61e","#f46d43","#e6ab02","#8073ac","#a6761d","#666666","#bc80bd","#d53e4f","#3288bd","#66a61e","#f46d43","#e6ab02","#8073ac","#a6761d","#666666","#bc80bd","#d53e4f","#3288bd")) # "#5e4fa2" ,"#66c2a5", "#fec44f",
    Graph <- Graph + geom_point(size=4)+annotation_logticks(sides='tblr')
    # Add the theoretical model
    # Graph <- Graph + geom_line(data=ModelDataTable, aes(color=Conditions), size=0.8)
    # Add the confidence intervals
    # if (ErrorBands==TRUE) {
        # Graph <- Graph + geom_line(data=ConfidenceDataTable, aes(x=TTF, y=LowerLimit, color=Conditions), linetype="dashed", size=0.8)
        # Graph <- Graph + geom_line(data=ConfidenceDataTable, aes(x=TTF, y=HigherLimit, color=Conditions), linetype="dashed",size=0.8)
    # }
    # Font size & x/y titles...
    Graph <- Graph + xlab("Stress time (s)") + ylab("Degradation (%)")
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
    Graph <- Graph + theme(axis.text.x = element_text(face="bold", size=16, margin=margin(0.4,0,0,0, "cm")))
    Graph <- Graph + theme(axis.text.y = element_text(size=16, margin=margin(0,0.4,0,0.2, "cm")))
    Graph <- Graph + theme(axis.ticks.length = unit(-0.25, "cm"))#, axis.ticks.margin = unit(0.4, "cm")) #Depreciated see margin above.
    # Add a title
    Graph <- Graph + ggtitle(Title)
    Graph <- Graph + theme(plot.title = element_text(face="bold", size=18))
    Graph <- Graph + geom_vline(xintercept = 5E8, color = "red", linetype=2 )
    Graph <- Graph + geom_hline(yintercept = 5, color = "red", linetype=2 )
    print(Graph)

    # Save as png & pdf
    # if (Save == TRUE){
    #     if (Title != ""){
    #         ggsave(filename=paste(Title,"png",sep="."),dpi=300)
    #         #ggsave(filename=paste(Title,"pdf",sep="."))
    #     } else {
    #         ggsave(filename="Chart.png",dpi=300)
    #         #ggsave(filename="Chart.pdf")
    #     }
    # }
}


AnalyzeRes <- function(){

    ListFiles <- list.files(pattern="mA.txt$")
    ExpDataTable <- ReadDataResistor(ListFiles)

    title <- paste(ExpDataTable$DeviceName[1], "(L =", ExpDataTable$Length[1], "µm", "W =", ExpDataTable$Width[1], "µm)", sep=" ")

    MedianDegTable <- ExpDataTable[ExpDataTable$DeviceNum == "Median",]
    # Drop the first measure for each serie in case DeltaR = 0 (Should not be the case)
    MedianDegTable <- MedianDegTable[!(MedianDegTable$DeltaRes == 0),]
    CreateGraphDeg(MedianDegTable,Title=title)

}


StackResistorData <- function()
{


    cat("Please select the result files of the first experiment\n")
    firstExp <- SelectFiles()
    cat("Please select the result files of the second experiment\n")
    secondExp <- SelectFilesAdvanced()

    # Resulting Stacked files will be available in a subdirectory of the second selection
    dir.create("results")

    # Read the device dedicated file from MIRA for XP1 and XP2
    for (i in seq_along(firstExp)){
        fileXP1 <- read.delim(firstExp[i],sep="", header=FALSE)
        fileXP2 <- read.delim(secondExp[i],sep="", header=FALSE)

        # add final time XP1 to XP2
        maxTempsXP1 <- fileXP1[length(fileXP1[,1]),1]
        fileXP2[,1] <- fileXP2[,1] + maxTempsXP1

        # Save the stacked files to a new file
        fileName <- paste("results/", secondExp[i], sep="")
        write.table(rbind(fileXP1,fileXP2), file = fileName, row.names=FALSE, col.names = FALSE)
    }



}
