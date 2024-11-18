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

# 1.1 Aufbereitung

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

datRaw <- mergeData("ID", list(dat_b1, dat_b2, dat_b3))


### Rekodieren des zusammengeführten Datensatzes

datRec <- recodeData(datRaw, values = inputList$values, subunits = inputList$subunits, verbose = TRUE)

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

# 1.2 Überprüfen, ob Daten zum Testdesign passen

###############################################################################
###############################################################################

# (entweder das datRec von oben nehmend, bei dem die Items gemäß subunits$subunitRecoded benannt sind)
checkDesign(dat = datRec, booklets = inputList$booklets, blocks = inputList$blocks, 
            rotation = inputList$rotation, subunits = inputList$subunits, 
            sysMis = "NA", id="ID", verbose = TRUE)

# oder die Daten so rekodieren, dass Items nicht umbenannt werden und dann muss bei checkDesign
# inputList$subunits nicht mehr mitübergeben werden
datRec2 <- recodeData(dat, values = inputList$values, verbose = TRUE)

checkDesign(dat = datRec2, booklets = inputList$booklets, blocks = inputList$blocks, 
    rotation = inputList$rotation, sysMis = "NA", id="ID", verbose = TRUE)

# Nein, sehr viele Abweichungen!

###############################################################################
###############################################################################

# 1.3 Export als GADSdat-Objekt

###############################################################################
###############################################################################

# raw
prepGADSraw <- prep2GADS(dat = dat, inputList = inputList, trafoType="raw")

# scored
prepGADSsco <- prep2GADS(dat = prepDat1, inputList = inputList, trafoType="scored")





###############################################################################
###############################################################################	

### Übung 2: Zusatzfunktionen

###############################################################################
###############################################################################

# 2.1 Kategorientrennschärfen

###############################################################################
###############################################################################

pbcs   <- catPbc(datRaw, datRec, idRaw = "ID", idRec = "ID",
                 context.vars = "hisei", values = inputList$values,
                 subunits = inputList$subunits)
evalPbc(pbcs)


###############################################################################
###############################################################################

# 2.2 Testsitzungsprotokolle - visualSubsetRecode()

###############################################################################
###############################################################################

subsetInfoMin <- data.frame(ID=c("person100", "person101", "person102", 
                                 "person103", "person101", "person100", 
                                 "person101", "person102", "person103", 
                                 "person101", "person101"),
                            datCols=c("I01", "I02", "I03", "I01", "I02", "I03",
                                      "I04", NA, "I02", "I03", "I04"))


datVisRec <- visualSubsetRecode(dat=prepDat1, subsetInfo=subsetInfoMin, ID="ID",
                                           toRecodeVal="mci", useGroups=NULL)

###############################################################################
###############################################################################

# 2.3 Beurteilerübereinstimmung - "Rater-Funktionen"

###############################################################################
###############################################################################

data(rater)

dat <- reshape2::dcast(subset(rater, variable == "V02"), id~rater, value.var = "value")
meanAgree(dat[,-1])
meanKappa(dat[,-1])

dat <- reshape2::dcast(subset(rater, variable == "V03"), id~rater, value.var = "value")
meanAgree(dat[,-1])
meanKappa(dat[,-1])

### ODER Schleife über alle Variablen:

library(tidyr)
library(dplyr)

results <- rater %>%
  pivot_wider(names_from = rater, values_from = value) %>%
  group_by(variable) %>%
  group_modify(~ {
    dat <- as.data.frame(select(.x, -id))
    tibble(
      MeanAgree = meanAgree(dat)$meanagree,
      MeanKappa = meanKappa(dat)$meankappa
    )
  }) %>%
  ungroup() %>%
  rename(Variable = variable)



