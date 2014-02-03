#' @title Easily inserting and transforming a dataset in fSRM
#' @description
#' This GUI can be used for inserting csv, txt and sav-files in R by means of a pop-up window.
#' Next, it it allows the user to easily transform a dataset into the required
#' format (long format), since SRM data are typically organized in a wide format.
#' Finally, the user can specify the desired labels that will be used in the output.
#'
#' @export
#' @details
#' The upper section of the main window allows to insert csv, txt and sav-files in R. It is required that the first row in the datafile contains the variable names.
#' A new pop-up window appears after clicking on 'transform my data' in the middle section. This new window allows to transform a dataset from wide format (i.e. each column representing one measured relationship, and one row for each family) to the necessary long format.
#' Specify - if present - in this new window first the variable names which identifies the different families and groups, respectively.
#' Next, specify the columns in the original dataset which contain the dyadic measurements. Enumerated columns need to be separated with a comma,consecutive columns are defined by means of colon (e.g. 1 : 9 defines the first until the second column in the original dataset).
#' Finally, define the position of the characters of the rater, the person being rated and the indicator (if present) in the previously selected dyadic measurements.
#' At the bottom section of the main window the user can agree with the default labels of the SRM components by clicking on 'Confirm output format' or specify alternative labels.

#' @examples
#' # The result is saved in a variable called \code{SRMData}, this is the dataset in long format.
#' # The original dataset is saved as \code{MyData}, which is in wide format.
#' import()

import <- function() {
  
#### Read in the dataset ####
  require(tcltk)
  getSPSS <- function() {
    name <- tclvalue(tkgetOpenFile(
      filetypes = "{{SPSS Files} {.sav}} {{All files} *}"))
    if (name == "") return;
    require(foreign)
    style$MyData <- read.spss(name, use.value.labels = TRUE, to.data.frame = TRUE)
    #assign("MyData", MyData, envir = .GlobalEnv)
    # tkdestroy(tt) # zodat automatisch het venster weggaat
    cat("Datafile is loaded\n")
    tkconfigure(button.SPSS, text = "Data inserted")
  }
  
  getCSV <- function() {
    name <- tclvalue(tkgetOpenFile(
      filetypes = "{{CSV Files} {.csv}} {{All files} *}"))
    if (name == "") return;
    style$MyData <- read.csv(name)
    #assign("MyData", MyData, envir = .GlobalEnv)
    # tkdestroy(tt)
    cat("Datafile is loaded\n")
    tkconfigure(button.CSV, text = "Data inserted")
  }
  
  getTXT <- function() {
    name <- tclvalue(tkgetOpenFile(
      filetypes = "{{TXT Files} {.txt}} {{All files} *}"))
    if (name == "") return;
    style$MyData <- read.table(name, header = T)
    #assign("MyData", MyData, envir = .GlobalEnv)
    # tkdestroy(tt)
    cat("Datafile is loaded\n")
    tkconfigure(button.TXT, text = "Data inserted")
  }
  
  
  
#### Transformation of the dataset ####
  launchDialog <- function() {
    onOK <- function() {
      #Define the variables where the position of the characters will be inserted (actor, partner, indicator, group)
      var <- as.character(tclvalue(Entry0))
      group <- as.character(tclvalue(Entrygr))
      fam <- as.character(tclvalue(EntryID))
      act_from <- as.character(tclvalue(Entry1))
      act_to <- as.character(tclvalue(Entry2))
      part_from <- as.character(tclvalue(Entry3))
      part_to <- as.character(tclvalue(Entry4))
      ind_from <- as.character(tclvalue(Entry5))
      ind_to <- as.character(tclvalue(Entry6))
           
      
      # Transform from wide to long format
      # Select only the relevant variables
      if (var == "") {
        tkmessageBox(title="Warning", message = "Please insert the column numbers containing the dyadic measurements of interest (i.e. the measured variables)!") }
      else {
        b <- strsplit(var,",")[[1]]
        ind <- grep(":",b)
        DVs <- c()
        cnt <- 1
        for(i in 1:length(b)){
          if(i %in% ind){
            sequ <- as.numeric(strsplit(b[i],":")[[1]])
            # bij A:B de : van verwijderen
            lengthsequ <-diff(c(sequ[1],sequ[2]))
            # Hoogste - laagste waarde van element dat oorspronkelijk : bevatte
            DVs[cnt:(cnt+lengthsequ)] <- sequ[1]:sequ[2]
            # DVs heeft het aantal elementen dat er tussen A en B zit (A:B), en deze worden opgevuld door de getallen die er echt tussenzitten
            cnt <- cnt+lengthsequ+1
          } else {
            DVs[cnt] <- as.numeric(b[i])
            # voeg aan deze vector gewoon dat getal toe
            cnt <- cnt+1
              }
		  }
        }
      
          # Add an family ID to the basic dataset,
      if (group != "") {
		  if (fam != "") {
		  style$SRMData <- melt(style$MyData, id.vars=c(fam, group), measure.vars=c(DVs)) 
		  } else {
			  style$MyData$family.id <- 1:nrow(style$MyData)
			  style$SRMData <- melt(style$MyData, id.vars=c("family.id",group), measure.vars=c(DVs))
		  }
		} else {
        	if (fam != "") {
				style$SRMData <- melt(style$MyData, id.vars=fam, measure.vars=c(DVs)) 
			} else {
			  style$MyData$family.id <- 1:nrow(style$MyData)
			  style$SRMData <- melt(style$MyData, id.vars="family.id", measure.vars=c(DVs))
            }
        }
      
          
      # Substract the characters of interest from the variable names + create useful pop-up warnings when something is not inserted
      # Actor ID's:
      if (act_from == "From" | act_from == "" | act_to == "To" | act_to == "") {
		  tkmessageBox(title="Warning", message = "The characters of the rater are not correctly inserted")
	  } else {
		  style$SRMData$actor.id <- substr(style$SRMData$variable, act_from, act_to)
	  }
	  
      # Partner ID's:
      if (part_from == "From" | part_from == "" | part_to == "To" | part_to == "") {
		  tkmessageBox(title="Warning", message = "The characters of the person being rated are not correctly inserted")
	  } else {
		  style$SRMData$partner.id <- substr(style$SRMData$variable, part_from, part_to)
	  }
	  
      # Indicator:
      if (ind_from == "From" | ind_from == "") {
		  value <- tkmessageBox(title="No Indicators", message = "No indicators were defined. Is this correct?", icon = "info", type="yesno", default="yes")
		  value <- tclvalue(value)
		  if (value == "no") return() 
	  } else {
		  style$SRMData$ind <- substr(style$SRMData$variable, ind_from, ind_to) 
	  }
               
      
      # Only return if all fields are completed (except for the indicator and group)
      if (act_from != "From" & act_from != "" & act_to != "To" & act_to != "" & part_from != "From" & part_from != "" & part_to != "To" & part_to != "") { 
		  cat("A new datafile called 'SRMData' is created\n")
		  tkdestroy(popup)
		  tkfocus(tt)
	  }
      
    }
    
	# Basic lay-out of the new pop-up window
    popup <- tktoplevel()
    tktitle(popup) <- "Transformation of the dataset"
    tkgrid(tklabel(popup, text = " "))
        # Ask for family ID
    heading <- tklabel(popup, text="If present, enter the variable name that contains the identifications of ... ")
    ID <- tklabel(popup, text=" the different families. ")
    gr <- tklabel(popup, text=" the different groups. ")
    tkgrid(tklabel(popup, text = " "))
    EntryID <- tclVar("")
    Entrygr <- tclVar("")
    e.fam <- tkentry(popup,width="25", textvariable=EntryID)
    e.gr <- tkentry(popup,width="25", textvariable=Entrygr)
    tkgrid(heading, columnspan=3)
    tkgrid(ID, e.fam)
    tkgrid(gr, e.gr)
    tkgrid(tklabel(popup, text = " "))
    tkgrid(tklabel(popup, text = " "))
    tkgrid.configure(ID, gr, sticky = "e")
    tkgrid.configure(heading, e.fam, e.gr, sticky = "w")
      # Ask the position of the dyadic measurements
    font1 <- tkfont.create(family="times",size=9,slant="italic")

    Entry0 <- tclVar("")
    e.dyad <- tkentry(popup,width="25", textvariable=Entry0)
    blanco <- tklabel(popup, text="")
    heading0 <- tklabel(popup, text="Specify the column numbers of the dyadic measurements in your original dataset ")
    ex <- tklabel(popup, text="E.g. variables 1 until 9, 11, 13 and 15 are entered as",font=font1)
    ex2 <- tklabel(popup, text="1:9, 11, 13, 15",font=font1, background="white")
    tkgrid(heading0, columnspan=3)
    tkgrid(blanco, e.dyad)
    tkgrid(ex, ex2)
    tkgrid.configure(ex, blanco, sticky="e")
    tkgrid.configure(ex2, e.dyad, sticky="w")
    tkgrid(tklabel(popup, text = " "))
    
      # Start subtracting part from variable names
    heading2 <- tklabel(popup, text="Based on the variable names of the dyadic measurements, define the position of the characters of ... ")
    tkgrid(heading2, columnspan=3)
    tkgrid(tklabel(popup, text = " "))
    
    #Everything for inserting the characters of the actor ID:
    l.actor <- tklabel(popup, text=" the rater in the dyadic measurement?")
    Entry1 <- tclVar("From")
    Entry2 <- tclVar("To")
    e.actor1 <- tkentry(popup,width="6", textvariable=Entry1)
    e.actor2 <- tkentry(popup,width="6", textvariable=Entry2)
    tkgrid(l.actor, e.actor1, e.actor2)
    tkgrid(tklabel(popup, text = " "))
     
    #Everything for inserting the characters of the partner ID:
    l.partner <- tklabel(popup, text=" the person being rated in the dyadic measurement?")
    Entry3 <- tclVar("From")
    Entry4 <- tclVar("To")
    e.partner1 <- tkentry(popup,width="6", textvariable=Entry3)
    e.partner2 <- tkentry(popup,width="6", textvariable=Entry4)
    tkgrid(l.partner, e.partner1, e.partner2)
    tkgrid(tklabel(popup, text = " "))
    
    #Everything for inserting the characters of the indicator:
    l.ind <- tklabel(popup, text=" the indicators (if present)")
    Entry5 <- tclVar("From")
    Entry6 <- tclVar("To")
    e.ind1 <- tkentry(popup,width="6", textvariable=Entry5)
    e.ind2 <- tkentry(popup,width="6", textvariable=Entry6)
    tkgrid(l.ind, e.ind1, e.ind2)
    tkgrid(tklabel(popup, text = " "))
    
  
    tkgrid.configure(heading2, l.actor, l.partner, l.ind, sticky="w")
    
    OK.but <- tkbutton(popup, text = " OK ", command = onOK)
    Cancel.but <- tkbutton(popup, text = " Cancel ", command = function() {tkdestroy(popup)})
    tkgrid(OK.but, Cancel.but)
    tkgrid(tklabel(popup, text = " "))
    tkfocus(popup)
  }
  
	Change <- function() {
		outfam <- as.character(tclvalue(Entry9))
		outact <- as.character(tclvalue(Entry10))
		outpar <- as.character(tclvalue(Entry11))
		outrel <- as.character(tclvalue(Entry12))
		#style <- new.env(parent=globalenv())
		style$familyeffect <- outfam
		style$actor <- outact
		style$partner <- outpar
		style$relationship <- outrel
		tkconfigure(change.but, text = "Outputformat confirmed")
	}



  tt <- tktoplevel()
  tktitle(tt) <- "Inserting and transforming your data"
  label.read <- tklabel(tt,text="Read in your datafile")
  button.CSV <- tkbutton(tt, text = "Select CSV file", command = getCSV)
  button.TXT <- tkbutton(tt, text = "Select TXT file", command = getTXT)
  button.SPSS <- tkbutton(tt, text = "Select SPSS File", command = getSPSS)
  tkgrid(tklabel(tt, text="
"))
  tkgrid(label.read)
  tkgrid(button.CSV)
  tkgrid(button.TXT)
  tkgrid(button.SPSS)
  tkfocus(tt)
  space1 <- tklabel(tt,text="
")
  label.data <- tklabel(tt,text="Do you want to transform your data from wide to long format? ")
  launchpopup.button <- tkbutton(tt, text = "Transform my data", command = launchDialog)
  space2 <- tklabel(tt,text=" ")
  tkgrid(space1)
  tkgrid(label.data)
  tkgrid(launchpopup.button)
  tkgrid(space2)
  
    ### Everything for the output format:
    l.output <- tklabel(tt,text="Which SRM labels do you prefer in the output? ")
    tkgrid( tklabel(tt,text=" "))
    tkgrid(l.output, columnspan=3)
    tkgrid(tklabel(tt,text=" "))
    o.fam <- tklabel(tt, text="Family effect?")
    o.actor <- tklabel(tt, text="Actor/Perceiver effect?")
    o.partner <- tklabel(tt, text="Partner/Target effect?")
    o.rel <- tklabel(tt, text="Relationship effect?")
    Entry9 <- tclVar("FE")
    Entry10 <- tclVar("A")
    Entry11 <- tclVar("P")
    Entry12 <- tclVar("R")
    o.famE <- tkentry(tt,width="15", textvariable=Entry9)
    o.actE <- tkentry(tt,width="15", textvariable=Entry10)
    o.parE <- tkentry(tt,width="15", textvariable=Entry11)
    o.relE <- tkentry(tt,width="15", textvariable=Entry12)
    tkgrid(o.fam, o.famE)
    tkgrid(o.actor, o.actE)
    tkgrid(o.partner, o.parE)
    tkgrid(o.rel, o.relE)
    tkgrid.configure(o.fam, o.actor, o.partner, o.rel, sticky="w")
    change.but <- tkbutton(tt, text = " Confirm output format ", command=Change)
    tkgrid(change.but)
    tkfocus(tt)
    tkgrid.configure(label.read, label.data, l.output, sticky="w")
    tkgrid( tklabel(tt,text="
"))
    tkgrid(tkbutton(tt, text = " OK ", command= function() {
		# this is the final OK - after that: return the data object
		tkdestroy(tt)
		return(style$SRMData)
	}))
    tkgrid( tklabel(tt,text="
"))

	# Print a warning about missing values ...
	font2 <- tkfont.create(family="times",size=8,slant="italic")
	heading13 <- tklabel(tt, text="Please deal with missing data in an appropriate way before using these functions.", font = font2)
	tkgrid(heading13)
	tkgrid.configure(heading13, sticky="e")
}

import2 <- function() {
	res <- tclvalue(import())
	return(res)
}