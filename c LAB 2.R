install.packages("haven")
setwd("Desktop/econ 4850")
library(haven)
VTdata <- read_sas("psam_p50.sas7bdat")
WYdata <- read_sas("psam_p56.sas7bdat")
# data sets named and uploaded
Total <- rbind(WYdata, VTdata)
# Merged both datasets vertically 
Workers2 <- subset(Total, Total$WAGP > 0,
                  select = c(SERIALNO, ST, SPORDER, WAGP, AGEP, SCHL, JWMNP, SEX))
saveRDS(Workers2, file="Workers.Rda")

#Created a subset of data that contains the varibles in the HW 1 guidelines
