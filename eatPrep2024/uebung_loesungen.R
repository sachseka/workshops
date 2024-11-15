###############################################################################
##################### eatPrep Workshop 19.11.2024 #############################
###############################################################################

### ggf. installieren
# install.packages("remotes")
remotes::install_github("sachseka/eatPrep")


### ...und laden des Pakets

library(eatPrep)




###############################################################################
###############################################################################	

### Übung 1: Hauptfunktionen

###############################################################################
###############################################################################

###############################################################################
###############################################################################

### Einlesen der im Paket enthaltenen .xslx-Datei "inputListe" mit readDaemonXlsx()
filename <- system.file("extdata", "inputList.xlsx", package = "eatPrep")
inpustList <- readDaemonXlsx(filename)

### Ergänzen der InputListe um lokale Pfade zu den .sav-Dateien (im realen Anwendungsfall i.d.R. nicht nötig, da die Pfade im ZKDaemon gesetzt werden)
inputList$savFiles$fullname[1] <- system.file("extdata", "booklet1.sav", package = "eatPrep")
inputList$savFiles$fullname[2] <- system.file("extdata", "booklet2.sav", package = "eatPrep")
inputList$savFiles$fullname[3] <- system.file("extdata", "booklet3.sav", package = "eatPrep")

### Überprüfen der inputListe mit checkInputList()
checkInputList(inputList)

###############################################################################
###############################################################################

### alle Aufbereitungsschritte mit der Wrapper-Funktion automateDataPreparation()

prepDat1 <- automateDataPreparation(inputList = inputList,
    readSpss = TRUE, checkData = TRUE,  mergeData = TRUE,
    recodeData = TRUE, aggregateData = TRUE, scoreData = TRUE,
    writeSpss = FALSE, verbose = TRUE)
	
###############################################################################
###############################################################################	

### ODER alle Aufbereitungsschritte einzeln:

###############################################################################
###############################################################################

### Einlesen der Datensätze booklet1.sav, booklet2.sav und booklet3.sav mit readSpss()
dat_b1 <- readSpss(inputList$savFiles$fullname[1])
dat_b2 <- readSpss(inputList$savFiles$fullname[2])
dat_b3 <- readSpss(inputList$savFiles$fullname[3])

### Checken der drei Datensätze
checkData(dat = dat_b1, datnam = "b1", 
			values = inputList$values, subunits = inputList$subunits, 
			units = inputList$units, ID = "ID", verbose = TRUE)
			
checkData(dat = dat_b2, datnam = "b2", 
			values = inputList$values, subunits = inputList$subunits, 
			units = inputList$units, ID = "ID", verbose = TRUE)
		
checkData(dat = dat_b3, datnam = "b3", 
			values = inputList$values, subunits = inputList$subunits, 
			units = inputList$units, ID = "ID", verbose = TRUE)


### Zusammenführen der drei Datensätze

dat <- mergeData("ID", list(dat_b1, dat_b2, dat_b3))


### Rekodieren des zusammengeführten Datensatzes

datRec <- recodeData(dat, values = inputList$values, subunits = inputList$subunits, verbose = TRUE)

### Aggregieren der rekodierten Daten

datAgg <- aggregateData(datRec, subunits = inputList$subunits, units = inputList$units, 
						rename = TRUE, recodedData = TRUE, verbose = TRUE)

### Scoren der aggregieren Daten

prepDat2 <- scoreData(datAgg, inputList$unitRecodings, inputList$subunits, verbose = TRUE)

###############################################################################
###############################################################################

# Vergleichen der Ergebnisse:

identical(prepDat1, prepDat2) #TRUE

###############################################################################
###############################################################################

# Überprüfen, ob Daten zum Testdesign passen:
datRec2 <- recodeData(dat, values = inputList$values, verbose = TRUE)

checkDesign(dat = datRec2, booklets = inputList$booklets, blocks = inputList$blocks, 
    rotation = inputList$rotation, sysMis = "NA", id="ID", verbose = TRUE)
