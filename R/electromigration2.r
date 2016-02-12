CompareEMLifeTime <- function()
# Perform a lifetime analysis given the name of the related structure
{
    Filters <- matrix(c("All files", "*", "Text", ".txt", "Export Files", "*exportfile.txt"),3, 2, byrow = TRUE)
    # Child Data
    cat("Please select the data you want to analyze\n")
    ListFilesChild <- SelectFilesAdvanced(Filters)
    DeviceIDChild <- sapply(ListFilesChild,function(x){strsplit(x,split="_")[[1]][2]})
    DataTableChild <- ReadDataAce(ListFilesChild)
    DataTableChild <- AddArea(DataTableChild, DeviceIDChild)
    # Child Path
    childWdPath <- getwd()

    # Child Data
    cat("Please select the reference data\n")
    ListFilesMother <- SelectFilesAdvanced(Filters)
    DataTableMother <- ReadDataAce(ListFilesMother)
    RefDeviceID <- sapply(ListFilesMother[1],function(x){strsplit(x,split="_")[[1]][2]})
    DataTableMother <- AddArea(DataTableMother, RefDeviceID)
    ModelMother <- ModelFit(DataTableMother, Law="BlackLaw")
    # Parent Path
    ParentPath <- getwd()

    # DataFrame creation for plot
    cond <- as.character(DataTableChild$Conditions[1])
    DataTableChild$Conditions <- as.factor(DeviceIDChild)

    SubDataTableMother <- DataTableMother[DataTableMother$Conditions == cond,]
    SubDataTableMother$Conditions <- as.factor(RefDeviceID)

    DataTable <- rbind(SubDataTableMother, DataTableChild)

    # Model DataFrame
    # Child
    ModelTableChild <- FitDistribution(DataTableChild, Scale="Lognormal")
    ModelTableChild <- AddArea(ModelTableChild, DeviceIDChild)
    ModelTableChild$Conditions <- DeviceIDChild
    #  Mother
    ModelTableMother <- CreateModelDataTable(ModelMother, ListConditions=cond, Area=DataTableMother$Area[1], Law="BlackLaw", Scale="Lognormal")
    ModelTableMother$Conditions <- RefDeviceID

    ModelDataTable <- rbind(ModelTableChild,ModelTableMother)

    # Error Data frame
    ErrorTable <- ErrorEstimation(SubDataTableMother, ModelTableMother, ConfidenceValue=0.95, Scale="Lognormal")
    ErrorTable$Conditions <- RefDeviceID

    # Graph
    CreateGraph(DataTable, ModelDataTable, ErrorTable, Title="", Scale="Lognormal", ErrorBands=TRUE, Save=FALSE)
    # Lifetime ratio
    lifetimeRatio <- MeanLifetimeRatio(ModelTableMother$TTF, DataTableChild$TTF[DataTableChild$Status==1], Scale="lognormal")
    print(paste("Mean lifetime ratio:", lifetimeRatio, sep=' '))

    # back to Child WD
    setwd(childWdPath)
}


MeanLifetimeRatio <- function(DataRef, DataComparison, Scale="lognormal")
# Provide the ratio for the caracteristic TTF (50% or 63%) for 2 populations
{
    if (Scale == "Weibull"){
        fitRef <- fitdistr(DataRef,"weibull")
        fitComp <- fitdistr(DataComparison,"weibull")
        ratio <- as.numeric( exp(fitComp$estimate[2])/exp(fitRef$estimate[2]) )
    } else {
        fitRef <- fitdistr(DataRef,"lognormal")
        fitComp <- fitdistr(DataComparison,"lognormal")
        ratio <- as.numeric( exp(fitComp$estimate[1])/exp(fitRef$estimate[1]) )
    }
    return(ratio)
}
