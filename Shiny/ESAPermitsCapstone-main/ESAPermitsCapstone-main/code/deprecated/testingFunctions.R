devtools::install_github("rory-spurr/ESAPermitsCapstone")
library(ESAPermitsCapstone)
MakeLeafletApp(DF = WestCoastPermitData, spatialData = WestCoastHUC8, esuBound = esuBound)