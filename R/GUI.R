#' A graphical user interface for the package mdir.logrank
#'
#' This function provides a graphical user interface for calculating
#' multiple-direction logrank test and the corresponding p-values
#' (based on \eqn{\chi^2}-approximation as well as on permutation).
#'
#'
#' @aliases GUI
#'
#' @export

calculateGUI <- function(){

  #require("RGtk2", quietly = TRUE)
  requireNamespace("RGtk2", quietly = TRUE)
  if(!("package:RGtk2" %in% search())){attachNamespace("RGtk2")}
  # Create window
  window <- RGtk2::gtkWindow()
  # Add title
  window["title"] <- "Multiple-direction logrank test"
  # Add a frame
  frame <- RGtk2::gtkFrameNew("Specify the settings")
  window$add(frame)
  # Create vertical container for file name entry
  vbox <- RGtk2::gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)
  # Add a sub-frame to vbox
  frame <- RGtk2::gtkFrameNew("Specify the data location")
  vbox$add(frame)
  # Add a vertical box to this sub-frame
  vbox1 <- RGtk2::gtkVBoxNew(FALSE, 8)
  vbox1$setBorderWidth(24)
  frame$add(vbox1)
  # Add Filename
  hbox <- RGtk2::gtkHBoxNew(FALSE, 8)
  vbox1$packStart(hbox, FALSE, FALSE, 0)
  label <- RGtk2::gtkLabelNewWithMnemonic("_File name")
  hbox$packStart(label, FALSE, FALSE, 0)
  # Add entry named "filename"
  filename <- RGtk2::gtkEntryNew()
  filename$setWidthChars(50)
  label$setMnemonicWidget(filename)
  hbox$packStart(filename, FALSE, FALSE, 0)
  #Add the Text "Headers?"
  hbox <- RGtk2::gtkHBoxNew(FALSE, 8)
  vbox1$packStart(hbox, FALSE, FALSE, 0)
  label <- RGtk2::gtkLabelNewWithMnemonic("_Headers?")
  hbox$packStart(label, FALSE, FALSE, 0)
  #Add the checkbox for the question "Headers?"
  headersEntry <- RGtk2::gtkCheckButton()
  headersEntry$active <- TRUE
  hbox$packStart(headersEntry, FALSE, FALSE, 0)
  label$setMnemonicWidget(headersEntry)
  # what separator is used?
  label <- RGtk2::gtkLabelNewWithMnemonic("Col. _Separator?")
  hbox$packStart(label, FALSE, FALSE, 0)
  sepEntry <- RGtk2::gtkEntryNew()
  sepEntry$setWidthChars(1)
  sepEntry$setText("")
  hbox$packStart(sepEntry, FALSE, FALSE, 0)
  label$setMnemonicWidget(sepEntry)
  # what's the character used for decimal points?
  label <- RGtk2::gtkLabelNewWithMnemonic("_Dec. character?")
  hbox$packStart(label, FALSE, FALSE, 0)
  decEntry <- RGtk2::gtkEntryNew()
  decEntry$setWidthChars(1)
  decEntry$setText(".")
  hbox$packStart(decEntry, FALSE, FALSE, 0)
  label$setMnemonicWidget(decEntry)
  ######DIRECTION-FRAME----------------------------
  # Add a sub-frame to vbox
  frame <- RGtk2::gtkFrameNew("Specify the directions")
  vbox$add(frame)
  # Add a vertical box to this sub-frame
  vbox1 <- RGtk2::gtkVBoxNew(FALSE, 8)
  vbox1$setBorderWidth(24)
  frame$add(vbox1)
  #Add the horizontal box
  hbox <- RGtk2::gtkHBoxNew(FALSE, 8)
  vbox1$packStart(hbox, FALSE, FALSE, 0)
  #Add Proportional? + check box
  label <- RGtk2::gtkLabelNewWithMnemonic("Proportional?")
  hbox$packStart(label, FALSE, FALSE, 0)
  propEntry <- RGtk2::gtkCheckButton()
  propEntry$active <- TRUE
  hbox$packStart(propEntry, FALSE, FALSE, 0)
  label$setMnemonicWidget(propEntry)
  #Add Crossing? + check box
  label <- RGtk2::gtkLabelNewWithMnemonic("Crossing?")
  hbox$packStart(label, FALSE, FALSE, 0)
  crossingEntry <- RGtk2::gtkCheckButton()
  crossingEntry$active <- TRUE
  hbox$packStart(crossingEntry, FALSE, FALSE, 0)
  label$setMnemonicWidget(crossingEntry)
  #Add Early? + check box
  label <- RGtk2::gtkLabelNewWithMnemonic("Early?")
  hbox$packStart(label, FALSE, FALSE, 0)
  earlyEntry <- RGtk2::gtkCheckButton()
  hbox$packStart(earlyEntry, FALSE, FALSE, 0)
  label$setMnemonicWidget(earlyEntry)
  #Add Late? + check box
  label <- RGtk2::gtkLabelNewWithMnemonic("Late?")
  hbox$packStart(label, FALSE, FALSE, 0)
  lateEntry <- RGtk2::gtkCheckButton()
  hbox$packStart(lateEntry, FALSE, FALSE, 0)
  label$setMnemonicWidget(lateEntry)
  #Add Late? + check box
  label <- RGtk2::gtkLabelNewWithMnemonic("Central?")
  hbox$packStart(label, FALSE, FALSE, 0)
  centralEntry <- RGtk2::gtkCheckButton()
  hbox$packStart(centralEntry, FALSE, FALSE, 0)
  label$setMnemonicWidget(centralEntry)
  #Add horizontal box
  hbox <- RGtk2::gtkHBoxNew(FALSE, 8)
  vbox1$packStart(hbox, FALSE, FALSE, 0)
  #Add question
  label <- RGtk2::gtkLabelNewWithMnemonic("_User specified directions of the form w(x) = x^r * (1-x)^g:")
  hbox$packStart(label, FALSE, FALSE, 0)
  ####COMBOBOXES----------------------------------------------
  #Add horizontal box
  hbox <- RGtk2::gtkHBoxNew(FALSE, 8)
  vbox1$packStart(hbox, FALSE, FALSE, 0)
  # Add "r":
  label <- RGtk2::gtkLabelNewWithMnemonic("_r: ")
  hbox$packStart(label, FALSE, FALSE, 0)
  #Add 1.combobox for the choice of r
  choices.vect <- 0:20 # How many different r,g can be chosen
  choices <- RGtk2::rGtkDataFrame(c("", as.character(choices.vect)))
  comboboxr1 <- RGtk2::gtkComboBox(choices)
  crt <- RGtk2::gtkCellRendererText()
  comboboxr1$packStart(crt)
  comboboxr1$addAttribute(crt, "text", 0)
  RGtk2::gtkComboBoxSetActive(comboboxr1,0)
  hbox$packStart(comboboxr1)
  #Add 2.combobox for the choice of r
  comboboxr2 <- RGtk2::gtkComboBox(choices)
  crt <- RGtk2::gtkCellRendererText()
  comboboxr2$packStart(crt)
  comboboxr2$addAttribute(crt, "text", 0)
  RGtk2::gtkComboBoxSetActive(comboboxr2,0)
  hbox$packStart(comboboxr2)
  #Add 3.combobox for the choice of r
  comboboxr3 <- RGtk2::gtkComboBox(choices)
  crt <- RGtk2::gtkCellRendererText()
  comboboxr3$packStart(crt)
  comboboxr3$addAttribute(crt, "text", 0)
  RGtk2::gtkComboBoxSetActive(comboboxr3,0)
  hbox$packStart(comboboxr3)
  #Add 4.combobox for the choice of r
  comboboxr4 <- RGtk2::gtkComboBox(choices)
  crt <- RGtk2::gtkCellRendererText()
  comboboxr4$packStart(crt)
  comboboxr4$addAttribute(crt, "text", 0)
  RGtk2::gtkComboBoxSetActive(comboboxr4,0)
  hbox$packStart(comboboxr4)
  #Add 5.combobox for the choice of r
  comboboxr5 <- RGtk2::gtkComboBox(choices)
  crt <- RGtk2::gtkCellRendererText()
  comboboxr5$packStart(crt)
  comboboxr5$addAttribute(crt, "text", 0)
  RGtk2::gtkComboBoxSetActive(comboboxr5,0)
  hbox$packStart(comboboxr5)
  #Add 6.combobox for the choice of r
  comboboxr6 <- RGtk2::gtkComboBox(choices)
  crt <- RGtk2::gtkCellRendererText()
  comboboxr6$packStart(crt)
  comboboxr6$addAttribute(crt, "text", 0)
  RGtk2::gtkComboBoxSetActive(comboboxr6,0)
  hbox$packStart(comboboxr6)
  #Add 7.combobox for the choice of r
  comboboxr7 <- RGtk2::gtkComboBox(choices)
  crt <- RGtk2::gtkCellRendererText()
  comboboxr7$packStart(crt)
  comboboxr7$addAttribute(crt, "text", 0)
  RGtk2::gtkComboBoxSetActive(comboboxr7,0)
  hbox$packStart(comboboxr7)
  #Add horizontal box
  hbox <- RGtk2::gtkHBoxNew(FALSE, 8)
  vbox1$packStart(hbox, FALSE, FALSE, 0)
  # Add "g":
  label <- RGtk2::gtkLabelNewWithMnemonic("_g:")
  hbox$packStart(label, FALSE, FALSE, 0)
  #Add 1.combobox for the choice of g
  comboboxg1 <- RGtk2::gtkComboBox(choices)
  crt <- RGtk2::gtkCellRendererText()
  comboboxg1$packStart(crt)
  comboboxg1$addAttribute(crt, "text", 0)
  RGtk2::gtkComboBoxSetActive(comboboxg1,0)
  hbox$packStart(comboboxg1)
  #Add 2.combobox for the choice of g
  comboboxg2 <- RGtk2::gtkComboBox(choices)
  crt <- RGtk2::gtkCellRendererText()
  comboboxg2$packStart(crt)
  comboboxg2$addAttribute(crt, "text", 0)
  RGtk2::gtkComboBoxSetActive(comboboxg2,0)
  hbox$packStart(comboboxg2)
  #Add 3.combobox for the choice of g
  comboboxg3 <- RGtk2::gtkComboBox(choices)
  crt <- RGtk2::gtkCellRendererText()
  comboboxg3$packStart(crt)
  comboboxg3$addAttribute(crt, "text", 0)
  RGtk2::gtkComboBoxSetActive(comboboxg3,0)
  hbox$packStart(comboboxg3)
  #Add 4.combobox for the choice of g
  comboboxg4 <- RGtk2::gtkComboBox(choices)
  crt <- RGtk2::gtkCellRendererText()
  comboboxg4$packStart(crt)
  comboboxg4$addAttribute(crt, "text", 0)
  RGtk2::gtkComboBoxSetActive(comboboxg4,0)
  hbox$packStart(comboboxg4)
  #Add 5.combobox for the choice of g
  comboboxg5 <- RGtk2::gtkComboBox(choices)
  crt <- RGtk2::gtkCellRendererText()
  comboboxg5$packStart(crt)
  comboboxg5$addAttribute(crt, "text", 0)
  RGtk2::gtkComboBoxSetActive(comboboxg5,0)
  hbox$packStart(comboboxg5)
  #Add 6.combobox for the choice of g
  comboboxg6 <- RGtk2::gtkComboBox(choices)
  crt <- RGtk2::gtkCellRendererText()
  comboboxg6$packStart(crt)
  comboboxg6$addAttribute(crt, "text", 0)
  RGtk2::gtkComboBoxSetActive(comboboxg6,0)
  hbox$packStart(comboboxg6)
  #Add 7.combobox for the choice of g
  comboboxg7 <- RGtk2::gtkComboBox(choices)
  crt <- RGtk2::gtkCellRendererText()
  comboboxg7$packStart(crt)
  comboboxg7$addAttribute(crt, "text", 0)
  RGtk2::gtkComboBoxSetActive(comboboxg7,0)
  hbox$packStart(comboboxg7)
  ## Simulation setting---------------------------------------------------------------
  frame <- RGtk2::gtkFrameNew("Specify the simulation settings")
  vbox$add(frame)
  # Add a vertical box to this sub-frame
  vbox1 <- RGtk2::gtkVBoxNew(FALSE, 8)
  vbox1$setBorderWidth(24)
  frame$add(vbox1)
  # Add a horizontal box
  hbox <- RGtk2::gtkHBoxNew(FALSE, 8)
  vbox1$packStart(hbox, FALSE, FALSE, 0)
  # Add nperm name
  label <- RGtk2::gtkLabelNewWithMnemonic("_permutation iterations")
  hbox$packStart(label, FALSE, FALSE, 0)
  # Add entry for nperm
  nperm <- RGtk2::gtkEntryNew()
  nperm$setWidthChars(7)
  nperm$setText(10000)
  label$setMnemonicWidget(nperm)
  hbox$packStart(nperm, FALSE, FALSE, 0)
  # Add digit for p-values perm name
  label <- RGtk2::gtkLabelNewWithMnemonic("digits for p-values")
  hbox$packStart(label, FALSE, FALSE, 0)
  # Add entry for digpval
  digpval <- RGtk2::gtkEntryNew()
  digpval$setWidthChars(2)
  digpval$setText(3)
  label$setMnemonicWidget(digpval)
  hbox$packStart(digpval, FALSE, FALSE, 0)
  # Add digitstat name
  label <- RGtk2::gtkLabelNewWithMnemonic("digits for statistic")
  hbox$packStart(label, FALSE, FALSE, 0)
  # Add entry for digstat
  digstat <- RGtk2::gtkEntryNew()
  digstat$setWidthChars(2)
  digstat$setText(3)
  label$setMnemonicWidget(digstat)
  hbox$packStart(digstat, FALSE, FALSE, 0)
  ## RESULTS---------------------------------------------------------------------
  frame <- RGtk2::gtkFrameNew("Results")
  vbox$add(frame)
  # Add a vertical box to this sub-frame
  vbox1 <- RGtk2::gtkVBoxNew(FALSE, 8)
  vbox1$setBorderWidth(24)
  frame$add(vbox1)
  # Add a horizontal box
  hbox <- RGtk2::gtkHBoxNew(FALSE, 8)
  vbox1$packStart(hbox, FALSE, FALSE, 0)
  # Add wait name
  label <- RGtk2::gtkLabelNewWithMnemonic("Notifications: ")
  hbox$packStart(label, FALSE, FALSE, 0)
  # Add wait field
  wait <- RGtk2::gtkEntryNew()
  wait$setWidthChars(40)
  label$setMnemonicWidget(wait)
  hbox$packStart(wait, FALSE, FALSE, 0)
  # Add a horizontal box
  hbox <- RGtk2::gtkHBoxNew(FALSE, 8)
  vbox1$packStart(hbox, FALSE, FALSE, 0)
  # Add pval Approx name
  label <- RGtk2::gtkLabelNewWithMnemonic("p-value (Approx.)")
  hbox$packStart(label, FALSE, FALSE, 0)
  # Add entry for p-value (Approx)
  pvalappr <- RGtk2::gtkEntryNew()
  pvalappr$setWidthChars(7)
  label$setMnemonicWidget(pvalappr)
  hbox$packStart(pvalappr, FALSE, FALSE, 0)
  # Add entry for p-value (Perm)
  label <- RGtk2::gtkLabelNewWithMnemonic("p-value (Perm.)")
  hbox$packStart(label, FALSE, FALSE, 0)
  # Add entry for p-value (Perm.)
  pvalper <- RGtk2::gtkEntryNew()
  pvalper$setWidthChars(7)
  label$setMnemonicWidget(pvalper)
  hbox$packStart(pvalper, FALSE, FALSE, 0)
  # Add test statistic
  label <- RGtk2::gtkLabelNewWithMnemonic("test statistic")
  hbox$packStart(label, FALSE, FALSE, 0)
  # Add entry for test statistic
  tstat <- RGtk2::gtkEntryNew()
  tstat$setWidthChars(7)
  label$setMnemonicWidget(tstat)
  hbox$packStart(tstat, FALSE, FALSE, 0)
  # The buttons--------------------------------------------------------------------------------
  the.buttons <- RGtk2::gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)
  buttonOK <- RGtk2::gtkButtonNewFromStock("gtk-ok")
  buttonLoad <- RGtk2::gtkButtonNewFromStock("load data")
  the.buttons$packStart(buttonOK, fill=F)
  the.buttons$packStart(buttonLoad, fill=F)
  buttonCancel <- RGtk2::gtkButtonNewFromStock("gtk-close")
  the.buttons$packStart(buttonCancel, fill=F)
  ### Function for button load
  getDirectory <- function(button, user.data){
    directory <- file.choose()
    RGtk2::gtkEntrySetText(filename, directory)
  }
  ## Function for button Start calculation
  performStatistics <- function(button, user.data) {
    extra.fun <- function(){
      # Alle the code is in an separate function since we want to
      # apply tryCatch latter.
    wait$setText("Calculating, wait please...")
    res <- NULL
    d <- NULL
    # Get the information about data and the file
    the.file <- filename$getText()
    if (the.file == ""){
      wait$setText("ERROR: Specifiy filename or load data!")
      return(0)
    }
    the.perm <- as.numeric(nperm$getText())
    the.digp <- as.numeric(digpval$getText())
    the.digs <- as.numeric(digstat$getText())
    the.sep <- sepEntry$getText()
    the.headers <- headersEntry$active
    the.dec <- decEntry$getText()
    tryCatch(
      d <- read.table(the.file, sep = the.sep, header = the.headers,
                    dec = the.dec),
      error = function(e){
        wait$setText("ERROR: Problems with reading the data")
        return(0)
      }
    )
    the.prop     <- propEntry$active
    the.central  <- centralEntry$active
    the.early    <- earlyEntry$active
    the.late     <- lateEntry$active
    the.crossing <- crossingEntry$active


    # Produce list rg
    comboboxr <- list( comboboxr1, comboboxr2, comboboxr3,
                       comboboxr4, comboboxr5, comboboxr6,
                       comboboxr7)
    comboboxg <- list( comboboxg1, comboboxg2, comboboxg3,
                       comboboxg4, comboboxg5, comboboxg6,
                       comboboxg7)
    rg <- list()
    for (i in 1:7){
      r <- RGtk2::gtkComboBoxGetActive(comboboxr[[i]])
      g <- RGtk2::gtkComboBoxGetActive(comboboxg[[i]])
      if (r*g == 0 && r+g > 0){
        # In this case only r or g are specificed for the corresponding direction
        # ->error
        # Note that also "" can be choosen. Hence, the value 0
        # represent that nothing is chosen.
        wait$setText("ERROR: Check the user specified directions!")
        return()
      }
      if( r>0 && g>0){ # Both exponents are specified
        rg <- c( rg, list(c(r-1, g-1)) )
      }
    }
    if ( the.prop == TRUE){    rg <- c(rg, list( c(0, 0) )) }
    if ( the.early == TRUE){   rg <- c(rg, list( c(1, 5) ))}
    if ( the.late == TRUE){    rg <- c(rg, list( c(5, 1) ))}
    if ( the.central == TRUE){ rg <- c(rg, list( c(1, 1) ))}
    if ( length(rg) == 0){ rg <- NULL}


    res <- mdir.logrank(data = d, cross = the.crossing, rg = rg, nperm =
                          the.perm, dig_p = the.digp, dig_stat = the.digs)
    wait.test <- substring( wait$getText(),1, 5) # Check if there is already an error
    if( wait.test != "ERROR"){
      wait$setText("Done")
    }
    pvalappr$setText( res$p_value$Approx)
    pvalper$setText(  res$p_value$Perm)
    tstat$setText(    res$stat)
    } # END of extra.fun
    match.fun(extra.fun)
    tryCatch( extra.fun()
        , error = function(e){
          # If there is some ERROR in extra.fun() then the
          # error function becomes active
          wait.test <- substring( wait$getText(),1, 5) # Check if there is already an error
          if( wait.test != "ERROR"){
            wait$setText("ERROR. Please check the settings!")
          }
        }
    ) # End of tryCatch
  }
  ## Activate buttons
  RGtk2::gSignalConnect(buttonOK, "clicked", performStatistics)
  RGtk2::gSignalConnect(buttonLoad, "clicked", getDirectory)
  RGtk2::gSignalConnect(buttonCancel, "clicked", window$destroy)
}

