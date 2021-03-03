# URL zu den Daten des Prognosewettbewerbs

**Trainingsdaten**:

https://raw.githubusercontent.com/sebastiansauer/Statistiklehre/main/2021-SoSe/Prognose-Wettbewerb/Daten/Trainingsdaten.csv


**Anwendungsdaten**

https://raw.githubusercontent.com/sebastiansauer/Statistiklehre/main/2021-SoSe/Prognose-Wettbewerb/Daten/Anwendungsdaten.csv


# R-Syntax, um die Daten einzulesen




```{r}
train <- read_csv("https://raw.githubusercontent.com/sebastiansauer/Statistiklehre/main/2021-SoSe/Prognose-Wettbewerb/Daten/Trainingsdaten.csv")

test <- read_csv("https://raw.githubusercontent.com/sebastiansauer/Statistiklehre/main/2021-SoSe/Prognose-Wettbewerb/Daten/Anwendungsdaten.csv")
```


