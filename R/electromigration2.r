CompareEMLifeTime <- function(StrucName)
# Perform a lifetime analysis given the name of the related structure
{
    # Child Path
    childStrWdPath <- getwd()

    # Parent Path
    explosedPath <- strsplit(childWdPath, "/")[[1]]

    parentWdPath <- explosedPath[1]
    for (i in 2:length(explosedPath)-1){
        parentWdPath <- paste(parentWdPath, explosedPath[i], sep="/")
    }
    parentWdPath <- paste(parentWdPath, StrucName, sep="/")

}
