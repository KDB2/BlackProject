amsReliability <- function()
{
    # Main windows
    tt <- tktoplevel()
    tkwm.minsize(tt, 500, 300)
    tkwm.resizable(tt, TRUE, TRUE)
    tkwm.title(tt, paste("amsReliability ",packageVersion("amsReliability")))

    done <- tclVar(0)

    tkconfigure(tt, borderwidth= 20, width="70")

    # Main title
    titleFont <- tkfont.create(size=10,weight ="bold")
    titleWindow <- tklabel(tt, text= "amsReliability")
    tkconfigure(titleWindow, font=titleFont)

    # Menu
    topMenu <- tkmenu(tt)
    tkconfigure(tt, menu=topMenu)
    # File
    fileMenu <- tkmenu(topMenu, tearoff=FALSE)
    tkadd(fileMenu,"command",label="Quit",command=function() tkdestroy(tt))
    tkadd(topMenu,"cascade",label="File",menu=fileMenu)
    # About & Update Menu
    about <- tkmenu(topMenu, tearoff=FALSE)
    tkadd(about,"command",label="Update",
          command=function() AutoUpdate())
    tkadd(about,"command",label="About",
          command=function()
                tkmessageBox(title="About",
                            message=paste("amsReliability\nVersion ", packageVersion("amsReliability"),"\n\nEmmanuel Chery\n2015--2016",sep=""),
                            icon="info"))
    tkadd(topMenu,"cascade",label="Help",menu=about)

    tkfocus(tt)


    # Main windows:
    ## Text
    titlePolice <- tkfont.create(size=14, weight="bold")
    subtitlePolice <- tkfont.create(size=10, weight="bold")
    welcomeText <- tklabel(tt, text="Welcome in amsReliability!")
    versionText <- tklabel(tt, text=paste("\n\t\t\t\tVersion ",packageVersion("amsReliability")," is currently loaded." ))
    tkconfigure(welcomeText,font=titlePolice)
    selectText <- tklabel(tt, text="\n\nPlease select the analysis you want to perform:\n")
    tkconfigure(selectText,font=subtitlePolice)
    # tkpack(welcomeText, versionText, selectText)
    tkgrid(welcomeText, columnspan=12)
    tkgrid(versionText, columnspan=12)
    tkgrid(selectText, columnspan=12)

    ## List of possibilities
    listAnalyses <- c("TDDB modelization and model parameter extraction","EM modelization and model parameter extraction", "EM exportfiles creation")
    maxChar <- max(sapply(listAnalyses, nchar))
    myList<-tklistbox(tt, height= length(listAnalyses)+1, width= maxChar, selectmode="single",background="white")

    tkgrid(myList,columnspan=12)
    for (analysis in listAnalyses){
        tkinsert(myList,"end",analysis)
    }
    # default selection
    tkselection.set(myList,0)

    ## Options
    ### text
    optionPolice <- tkfont.create(size=8, weight="bold")
    optionText <- tklabel(tt, text="\nOptions:")
    tkconfigure(optionText,font=optionPolice)
    tkgrid(optionText,row=6, column=1)
    ### Buttons
    error <- tkcheckbutton(tt)
    errorValue <- tclVar("0")
    tkconfigure(error,variable=errorValue)
    save <- tkcheckbutton(tt)
    saveValue <- tclVar("1")
    tkconfigure(save,variable=saveValue)
    tkgrid(tklabel(tt,text="\tDisplay error bands:"),error, columnspan=2)
    tkgrid(tklabel(tt,text="\tSave chart:"),save, columnspan=2)


    # OnOK function
    OnOK <- function()
    {
        # Option values:
        errorband <- as.character(tclvalue(errorValue))
        save <- as.character(tclvalue(saveValue))
        if (error == "1"){
            errorband <- TRUE
        } else {
            errorband <- FALSE
        }

        if (save == "1"){
            save <- TRUE
        } else {
            save <- FALSE
        }

        # List value:
        userChoice <- as.numeric(tkcurselection(myList))+1 # Index starts at 1 now
        tkdestroy(tt)

        if (userChoice == 1){
            OxideTDDB(ErrorBand = errorband, Save = save)
        } else if (userChoice == 2){
            BlackAnalysis(ErrorBand = errorband, Save = save)
        } else if (userChoice == 3){
            CreateExportFiles()
        }
    }


    ## Button OK & Quit
    OK.but <- tkbutton(tt,text="OK",command=OnOK)
    Quit.but <- tkbutton(tt,text="Quit",command=function() tkdestroy(tt))
    tkgrid(OK.but,row=9, column=11)
    tkgrid(Quit.but,row=9, column=12)

    # Watch for windows closure.
    tkbind(tt, "<Destroy>", function()tclvalue(done)<-2)
    tkwait.variable(done)

    if(tclvalue(done)=="2") stop("Quit -- The light at the end of the tunnel will be switched off.")

    tkdestroy(tt)
}
