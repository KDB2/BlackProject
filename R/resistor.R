################################################################################
###                                                                          ###
###    INFORMATIONS                                                          ###
###    ---------------------------------                                     ###
###                                                                          ###
###       MODULE NAME         exportFiles.r                                  ###
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


ResistorExportFiles <- function(lot = "C18051", wafer = "W5", deviceList = c("OPPPCRES","OPPPCRES","OPNPCRES","OPNPCRES","OPNDRES","OPNDRES"),
indiceList = list(c(1,10),c(31,40),c(11,20),c(41,50),c(21,30),c(51,60)), temp = 150, stressList = c(15,1.6,30,4,30,4),
lengthList = c(10,3,10,3,10,3), widthList = c(6, 0.64, 12, 1.6, 6, 0.8)){

    ################################################################################
    #                      Experimental Conditions                                 #
    # lot = "C18051", wafer = "W5", deviceList = c("OPPPCRES","OPPPCRES","OPNPCRES","OPNPCRES","OPNDRES","OPNDRES"), indiceList = list(c(1,10),c(31,40),c(11,20),c(41,50),c(21,30),c(51,60)), temp = 150, stressList = c(6,0.64,12,1.6,36,4.8)
    # lengthList = c(10,3,10,3,10,3), widthList = c(6, 0.64, 12, 1.6, 6, 0.8)
    ################################################################################


    listFile2Read <- list.files(pattern="DEG2")
    # Length of the files to read. All the same length as the XP are run on the same tool.
    lengthFile2Read <- length(read.delim("DEG2.001",sep=" ", header=FALSE,na.strings="NA")[,1])

    for (i in seq_along(deviceList)){
        device <- deviceList[i]
        stress <- stressList[i]
        length <- lengthList[i]
        width <- widthList[i]

        fileName <- paste(paste(lot, wafer, device, temp, paste(stress,"mA",sep=""), sep="_"),"txt",sep=".")
        ExpName <- paste("device","EM",paste(temp,"C",sep=""),paste(stress,"mA",sep=""),sep="_")

        # Main data
        resultTable <- data.frame()
        # USed in mean degradation calculation
        meanDegTable <- data.frame(row.names=1:lengthFile2Read)
        # Used in mean R calculation
        meanRTable <- data.frame(row.names=1:lengthFile2Read)

        deviceRef <- 1

        for (j in indiceList[[i]][1]:indiceList[[i]][2]){

            tempData <- read.delim(listFile2Read[j],sep=" ", header=FALSE,na.strings=c("NA","1.0000000E+12",1.0000000E+12))
            tempData[,4]
            deltaR <- (tempData[,4] - tempData[1,4])/tempData[1,4]

            tempTable <- data.frame("XPName"=ExpName,"GroupName"=device,"DevName"= deviceRef,"Version"=1,"Cycle"=seq(1:length(tempData[,1])) ,"Time"=tempData[,2],"R"=tempData[,4], "DR"=deltaR, "Stress"=stress, "Temp"=temp, "Length"=length, "Width"= width)

            meanDegTable <- cbind(meanDegTable,deltaR)
            meanRTable <- cbind(meanRTable,tempData[,4])

            # Cleaning of Error code on main table
            #tempTable <- tempTable[tempTable$R < 1E8, ]
            tempTable <- tempTable[!is.na(tempTable$R),]
            resultTable <- rbind(resultTable, tempTable)

            deviceRef <- deviceRef + 1
        }

    #meanDeg <- rowMeans(meanDegTable, na.rm = TRUE, dims = 1)
    #meanR <- rowMeans(meanRTable, na.rm = TRUE, dims = 1)
    medianR <- apply(meanRTable,1,median,na.rm=TRUE)
    medianDeg <- apply(meanDegTable,1,median,na.rm=TRUE)
    resultTable <- rbind(resultTable, data.frame("XPName"=ExpName,"GroupName"=device,"DevName"= "Median","Version"=1,"Cycle"=seq(1:length(tempData[,1])),
                "Time"=tempData[,2],"R"=medianR, "DR"=medianDeg, "Stress"=stress, "Temp"=temp, "Length"=length, "Width"= width))

    # Save in a file
    cat("[DATA]\n", file=fileName)
    cat("ExpName\tGroupName\tDevName\tVersion\tCycle\tTGES[S]\tR(Ohm)\tDeltaR(%)\tStress\tTemperature (C)\tLength (um)\tWidth (um)\n", file=fileName, append=TRUE)
    cat("Char_50\tChar_50\tChar_50\tBigInt\tBigInt\tDouble\tDouble\tDouble\tDouble\tDouble\tDouble\tDouble\n", file=fileName, append=TRUE)
    write.table(resultTable,file=fileName, sep="\t", append=TRUE, quote=FALSE,row.names=FALSE,col.names=FALSE)
    # cat("\n \n",file="fit.txt",append=TRUE)
    # cat("\n",file="fit.txt",append=TRUE)
    # cat("Experimental Data:",file="fit.txt",append=TRUE)

    }
}




ReadDataResistor <- function(listFiles){

    dataTable <- data.frame()

    for (file in listFiles){
        openFile <- read.delim(file, skip=3, header=FALSE)
        names(openFile) <- c("ExpName","GroupName","DevName","Version","Cycle","Time","R","DeltaR","Stress","Temperature","Length","Width")

        dataTable <- rbind(dataTable, openFile[openFile$DevName=="Median",])
    }
    return(dataTable)
}



ListFiles <- list.files(pattern="mA.txt$")
