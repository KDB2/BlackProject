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
    cat("Please select the data of the reference structure\n")
    ListFilesMother <- SelectFilesAdvanced(Filters)
    DataTableMother <- ReadDataAce(ListFilesMother)
    RefDeviceID <- sapply(ListFilesMother[1],function(x){strsplit(x,split="_")[[1]][2]})
    DataTableMother <- AddArea(DataTableMother, RefDeviceID)
    ModelMother <- ModelFit(DataTableMother, Law="BlackLaw")
    # Parent Path
    ParentPath <- getwd()

    # # Read the child data, before goinf to the mother wd
    # ListFilesChild <- list.files(pattern="*exportfile.txt")
    # DeviceIDChild <- sapply(ListFilesChild,function(x){strsplit(x,split="_")[[1]][2]})
    # DataTableChild <- ReadDataAce(ListFilesChild)
    # DataTableChild <- AddArea(DataTableChild, DeviceIDChild)


    # Extract mean lifetime for Mother Structure and for child structure.
    MTTFmother <- CalculLifeTime(Model=ModelMother, Area=DataTableMother$Area[1], Stress=DataTableChild$Stress[1], Temperature=DataTableChild$Temperature[1], Probability=0,  Law="BlackLaw")

    fit <- fitdistr(DataTableChild$TTF[DataTableChild$Status==1],"lognormal")
    MTTFchild <- as.numeric(exp(fit$estimate[1]))  # meanlog
    ScaleChild <- as.numeric(fit$estimate[2])  # sdlog

    lifetimeRatio <- MTTFchild / MTTFmother
    print(paste("Lifetime ratio:", lifetimeRatio, sep=' '))

    # DataFrame creation for plot
    cond <- as.character(DataTableChild$Conditions[1])
    DataTableChild$Conditions <- as.factor(DeviceIDChild)

    SubDataTableMother <- DataTableMother[DataTableMother$Conditions == cond,]
    SubDataTableMother$Conditions <- as.factor(RefDeviceID)
    # SubDataTableMother <- droplevels(SubDataTableMother)

    DataTable <- rbind(DataTableChild, SubDataTableMother)

    # Model DataFrame
    # Model Child
    ModelTableChild <- FitDistribution(DataTableChild, Scale="Lognormal")
    ModelTableChild <- AddArea(ModelTableChild, DeviceIDChild)
    ModelTableChild$Conditions <- DeviceIDChild
    # Model Mother
    ModelTableMother <- CreateModelDataTable(ModelMother, ListConditions=cond, Area=DataTableMother$Area[1], Law="BlackLaw", Scale="Lognormal")
    ModelTableMother$Conditions <- RefDeviceID

    ModelDataTable <- rbind(ModelTableChild,ModelTableMother)

    # Error Data frame
    ErrorTable <- ErrorEstimation(SubDataTableMother, ModelTableMother, ConfidenceValue=0.95, Scale="Lognormal")
    ErrorTable$Conditions <- RefDeviceID

    CreateGraph(DataTable, ModelDataTable, ErrorTable, Title="", Scale="Lognormal", ErrorBands=TRUE, Save=FALSE)

    # back to Child WD
    setwd(childWdPath)

}
