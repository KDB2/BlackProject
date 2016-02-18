tt <- tktoplevel()
tkwm.minsize(tt, 500, 300)
tkwm.resizable(tt, TRUE, TRUE)
tkwm.title(tt, "amsReliability")

done <- tclVar(0)

tkconfigure(tt, borderwidth= 20, width="70")

titleFont <- tkfont.create(size=10,weight ="bold")
titleWindow <- tklabel(tt, text="amsReliability")
tkconfigure(titleWindow, font=titleFont)


tkbind(tt, "<Destroy>", function()tclvalue(done)<-2)
tkwait.variable(done)

if(tclvalue(done)=="2") stop("aborted")

tkdestroy(tt)