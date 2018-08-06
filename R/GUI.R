#' A graphical user interface for the package mdir.logrank
#'
#' This function provides a graphical user interface for calculating
#' multiple-direction logrank test for the two-sided and the one-sided
#' testing problem.
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
  frame <- RGtk2::gtkFrameNew("Choose between the one-sided and the two-sided testing problem")
  window$add(frame)
  # Create vertical container for file name entry
  vbox <- RGtk2::gtkVBoxNew(FALSE, 8)
  vbox$setBorderWidth(24)
  frame$add(vbox)
  #Add horizontal box
  hbox <- RGtk2::gtkHBoxNew(FALSE, 5)
  vbox$packStart(hbox, FALSE, FALSE, 0)
  # Add "Testing problem: ":
  label <- RGtk2::gtkLabelNewWithMnemonic("Testing problem: ")
  hbox$packStart(label, FALSE, FALSE, 0)
  #Add combobox
  choices <- RGtk2::rGtkDataFrame(c(" One-side  ", " Two-sided  "))
  hbox$packStart
  combobox <- RGtk2::gtkComboBox(choices)
  crt <- RGtk2::gtkCellRendererText()
  combobox$packStart(crt)
  combobox$addAttribute(crt, "text", 0)
  RGtk2::gtkComboBoxSetActive(combobox,0)
  hbox$packStart(combobox, expand = FALSE) # Expend = FALSE ensures that the size for the combobox is exactly the size of the choices.
   ######
  the.buttons <- RGtk2::gtkHButtonBoxNew()
  the.buttons$setBorderWidth(5)
  vbox$add(the.buttons)
  the.buttons$setLayout("spread")
  the.buttons$setSpacing(40)
  buttonOK <- RGtk2::gtkButtonNewFromStock("gtk-ok")
  the.buttons$packStart(buttonOK, fill=F)
  buttonCancel <- RGtk2::gtkButtonNewFromStock("gtk-close")
  the.buttons$packStart(buttonCancel, fill=F)

  # Function for ok-button
  functionOK <- function(button, user.data){
    out_combo <- RGtk2::gtkComboBoxGetActive(combobox)
    if (out_combo == 1){
      calculateGUI_twosided()
    }else{
      calculateGUI_onesided()
    }
  }

  #By clicking the ok-button the window is closed and the functionOK is activated
  RGtk2::gSignalConnect(buttonOK, "clicked", functionOK )
  RGtk2::gSignalConnect(buttonOK, "clicked", window$destroy)
  RGtk2::gSignalConnect(buttonCancel, "clicked", window$destroy)
}
#################################################################
###################################################################
############# GUI for two-sided testing problem ##################
###################################################################
###################################################################
calculateGUI_twosided <- function(){
  #require("RGtk2", quietly = TRUE)
  requireNamespace("RGtk2", quietly = TRUE)
  if(!("package:RGtk2" %in% search())){attachNamespace("RGtk2")}
  # Create window
  window <- RGtk2::gtkWindow()
  # Add title
  window["title"] <- "Two-sided permutation test"
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
  #The data always need a header, otherwise, your function does not work.
  #Thus, the question "Headers?" is not needed.
  ##Add the Text "Headers?"
  hbox <- RGtk2::gtkHBoxNew(FALSE, 8)
  vbox1$packStart(hbox, FALSE, FALSE, 0)
  #label <- RGtk2::gtkLabelNewWithMnemonic("_Headers?")
  #hbox$packStart(label, FALSE, FALSE, 0)
  #Add the checkbox for the question "Headers?"
  #headersEntry <- RGtk2::gtkCheckButton()
  #headersEntry$active <- TRUE
  #hbox$packStart(headersEntry, FALSE, FALSE, 0)
  #label$setMnemonicWidget(headersEntry)

  # what separator is used?
  label <- RGtk2::gtkLabelNewWithMnemonic("Col. _Separator?")
  hbox$packStart(label, FALSE, FALSE, 0)
  sepEntry <- RGtk2::gtkEntryNew()
  sepEntry$setWidthChars(1)
  sepEntry$setText(",")
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
  wait$setWidthChars(50)
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
    #the.headers <- headersEntry$active # not needed for our situation
    the.dec <- decEntry$getText()
    tryCatch(
      d <- read.table(the.file, sep = the.sep, header = TRUE,
                    dec = the.dec),
      error = function(e){
        wait$setText("ERROR: Problems with reading the data")
        return(0)
      }
    )
    if( sum(c("time","group","event") %in% names(d)) != 3){
      # Note that a misspecification of sep or dec may also lead here to
      # an error, that is why we prefer this error text instead of a more
      # specific one
      wait$setText("ERROR: Problems with reading the data!")
      return()
    }
    lev_group <- levels( as.factor(d$group))
    if ( length(lev_group) != 2){
      wait$setText("ERROR: There are either more or less than 2 groups!")
      return()
    }
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
###################################################################
###################################################################
############# GUI for the one-sided testing problem ###############
###################################################################
###################################################################
calculateGUI_onesided <- function(){
  #require("RGtk2", quietly = TRUE)
  requireNamespace("RGtk2", quietly = TRUE)
  if(!("package:RGtk2" %in% search())){attachNamespace("RGtk2")}
  # Create window
  window <- RGtk2::gtkWindow()
  # Add title
  window["title"] <- "One-sided wild bootstrap test"
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
  #Add button to load the data
  buttonLoad <- RGtk2::gtkButtonNewFromStock("load data")
  hbox$packStart(buttonLoad, FALSE, FALSE, 0)
  getDirectory <- function(button, user.data){
    directory <- file.choose()
    RGtk2::gtkEntrySetText(filename, directory)
  }
  RGtk2::gSignalConnect(buttonLoad, "clicked", getDirectory)
  #The data always need a header, otherwise, your function does not work.
  #Thus, the question "Headers?" is not needed.
  ##Add the Text "Headers?"
  hbox <- RGtk2::gtkHBoxNew(FALSE, 8)
  vbox1$packStart(hbox, FALSE, FALSE, 0)
  #label <- RGtk2::gtkLabelNewWithMnemonic("_Headers?")
  #hbox$packStart(label, FALSE, FALSE, 0)
  #Add the checkbox for the question "Headers?"
  #headersEntry <- RGtk2::gtkCheckButton()
  #headersEntry$active <- TRUE
  #hbox$packStart(headersEntry, FALSE, FALSE, 0)
  #label$setMnemonicWidget(headersEntry)

  # what separator is used?
  label <- RGtk2::gtkLabelNewWithMnemonic("Col. _Separator?")
  hbox$packStart(label, FALSE, FALSE, 0)
  sepEntry <- RGtk2::gtkEntryNew()
  sepEntry$setWidthChars(1)
  sepEntry$setText(",")
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
  #new horizontal
  hbox <- RGtk2::gtkHBoxNew(FALSE, 8)
  vbox1$packStart(hbox, FALSE, FALSE, 0)
  #Add Group 1 name text
  label <- RGtk2::gtkLabelNewWithMnemonic("Specify the first group:")
  hbox$packStart(label, FALSE, FALSE, 0)
  #Add an empty combobox which can be manipuled later by the commands
  # gtkComboBoxAppendText, gtkComboBoxRemoveText, gtkComboBoxPrependText (see below)
  combogroup1 <- gtkComboBoxNewText()
  hbox$packStart(combogroup1)
  #new horizontal
  hbox <- RGtk2::gtkHBoxNew(FALSE, 8)
  vbox1$packStart(hbox, FALSE, FALSE, 0)
  # Add Text instruction for group1
  label <- RGtk2::gtkLabelNewWithMnemonic("Extract the group names from the data: ")
  hbox$packStart(label, FALSE, FALSE, 0)
  buttonExtract <- RGtk2::gtkButtonNewFromStock("Extract")
  hbox$packStart(buttonExtract, fill=F)
  # function for buttonExtract
  functionExtract <- function(button, user.data){
    the.file <- filename$getText()
    if (the.file == ""){
      wait$setText("ERROR: Specifiy filename or load data!")
      return(0)
    }
    the.sep <- sepEntry$getText()
    #the.headers <- headersEntry$active # not needed for our situation
    the.dec <- decEntry$getText()
    tryCatch(
      d <- read.table(the.file, sep = the.sep, header = TRUE,
                      dec = the.dec),
      error = function(e){
        wait$setText("ERROR: Problems with reading the data")
        return(0)
      }
    )
    if( sum(c("time","group","event") %in% names(d)) != 3){
      # Note that a misspecification of sep or dec may also lead here to
      # an error, that is why we prefer this error text instead of a more
      # specific one
      wait$setText("ERROR: Problems with reading the data!")
      return()
    }
    lev_group <- levels( as.factor(d$group))
    if ( length(lev_group) != 2){
      wait$setText("ERROR: There are either more or less than 2 groups!")
      return()
    }
    #Remove the previous entries
    RGtk2::gtkComboBoxRemoveText(combogroup1, 0)
    RGtk2::gtkComboBoxRemoveText(combogroup1, 0)
    #add the new entries
    RGtk2::gtkComboBoxAppendText(combogroup1, as.character(lev_group[1]))
    RGtk2::gtkComboBoxAppendText(combogroup1, as.character(lev_group[2]))
    RGtk2::gtkComboBoxSetActive(combogroup1,0)
   }
  #Extract button clicked
  RGtk2::gSignalConnect(buttonExtract, "clicked", functionExtract)

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
  #Add Early? + check box
  label <- RGtk2::gtkLabelNewWithMnemonic("Early?")
  hbox$packStart(label, FALSE, FALSE, 0)
  earlyEntry <- RGtk2::gtkCheckButton()
  earlyEntry$active <- TRUE
  hbox$packStart(earlyEntry, FALSE, FALSE, 0)
  label$setMnemonicWidget(earlyEntry)
  #Add Late? + check box
  label <- RGtk2::gtkLabelNewWithMnemonic("Late?")
  hbox$packStart(label, FALSE, FALSE, 0)
  lateEntry <- RGtk2::gtkCheckButton()
  lateEntry$active <- TRUE
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
  # Add iter name
  label <- RGtk2::gtkLabelNewWithMnemonic("_wild bootstrap iterations")
  hbox$packStart(label, FALSE, FALSE, 0)
  # Add entry for iter
  iter <- RGtk2::gtkEntryNew()
  iter$setWidthChars(7)
  iter$setText(10000)
  label$setMnemonicWidget(iter)
  hbox$packStart(iter, FALSE, FALSE, 0)
  # Add digit for p-values perm name
  label <- RGtk2::gtkLabelNewWithMnemonic("digits for p-value")
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
  # Add a horizontal box
  hbox1 <- RGtk2::gtkHBoxNew(FALSE, 8)
  vbox1$packStart(hbox1, FALSE, FALSE, 0)
  # Add wild name
  label <- RGtk2::gtkLabelNewWithMnemonic("wild bootstrap type")
  hbox1$packStart(label, FALSE, FALSE, 0)
  #Combobox for wild
  choices_wild <- RGtk2::rGtkDataFrame(c(" Rademacher ", " Normal ", " Poisson "))
  comboboxwild <- RGtk2::gtkComboBox(choices_wild)
  crt <- RGtk2::gtkCellRendererText()
  comboboxwild$packStart(crt)
  comboboxwild$addAttribute(crt, "text", 0)
  RGtk2::gtkComboBoxSetActive(comboboxwild,0)
  hbox1$packStart(comboboxwild, expand = FALSE)
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
  wait$setWidthChars(60)
  label$setMnemonicWidget(wait)
  hbox$packStart(wait, FALSE, FALSE, 0)
  # Add a horizontal box
  hbox <- RGtk2::gtkHBoxNew(FALSE, 8)
  vbox1$packStart(hbox, FALSE, FALSE, 0)
  # Add name for p-value
  label <- RGtk2::gtkLabelNewWithMnemonic("p-value")
  hbox$packStart(label, FALSE, FALSE, 0)
  # Add empty box for p-value
  pval <- RGtk2::gtkEntryNew()
  pval$setWidthChars(10)
  label$setMnemonicWidget(pval)
  hbox$packStart(pval, FALSE, FALSE, 0)
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
  the.buttons$packStart(buttonOK, fill=F)
  buttonCancel <- RGtk2::gtkButtonNewFromStock("gtk-close")
  the.buttons$packStart(buttonCancel, fill=F)
  ## Function for button Start calculation
  performStatistics <- function(button, user.data) {
    extra.fun <- function(){
      # Al the code is in an separate function since we want to
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
      the.iter <- as.numeric(iter$getText())
      the.digp <- as.numeric(digpval$getText())
      the.digs <- as.numeric(digstat$getText())
      the.sep <- sepEntry$getText()
      #the.headers <- headersEntry$active # not needed for our situation
      the.dec <- decEntry$getText()
      wild_index <- RGtk2::gtkComboBoxGetActive(comboboxwild) + 1
      wild <- c("rade", "norm", "pois")
      the.wild <- wild[ wild_index ]
      the.group1 <- RGtk2::gtkComboBoxGetActiveText(combogroup1)
      if ( is.null(the.group1) == TRUE){
        wait$setText("ERROR: Extract the groups from the data and specify the first group")
        return(0)
      }
      tryCatch(
        d <- read.table(the.file, sep = the.sep, header = TRUE,
                        dec = the.dec),
        error = function(e){
          wait$setText("ERROR: Problems with reading the data")
          return(0)
        }
      )
      if( sum(c("time","group","event") %in% names(d)) != 3){
        # Note that a misspecification of sep or dec may also lead here to
        # an error, that is why we prefer this error text instead of a more
        # specific one
        wait$setText("ERROR: Problems with reading the data!")
        return()
      }
      lev_group <- levels( as.factor(d$group))
      if ( length(lev_group) != 2){
        wait$setText("ERROR: There are either more or less than 2 groups!")
        return()
      }
      the.prop     <- propEntry$active
      the.central  <- centralEntry$active
      the.early    <- earlyEntry$active
      the.late     <- lateEntry$active

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

        }
        if( r>0 && g>0){ # Both exponents are specified
          rg <- c( rg, list(c(r-1, g-1)) )
        }
      }
      if ( the.prop == TRUE){    rg <- c(rg, list( c(0, 0) )) }
      if ( the.early == TRUE){   rg <- c(rg, list( c(0, 4) ))}
      if ( the.late == TRUE){    rg <- c(rg, list( c(4, 0) ))}
      if ( the.central == TRUE){ rg <- c(rg, list( c(1, 1) ))}


      res <- mdir.onesided(d, rg = rg, group1 = the.group1, wild = the.wild, iter = the.iter, dig_p = the.digp, dig_stat = the.digs)
      wait.test <- substring( wait$getText(),1, 5) # Check if there is already an error
      if( wait.test != "ERROR"){
        wait$setText("Done")
      }
      pval$setText(  res$p_value)
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
  RGtk2::gSignalConnect(buttonCancel, "clicked", window$destroy)
}

