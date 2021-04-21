# install.packages("readxl")
# install.packages("purrrlyr")
# Load the readxl package
library(readxl)
library(dplyr)
library(purrrlyr)
# Read the sheets, one by one
data <- read_excel("Account.xlsx", sheet = 1)

va = c("Sector", "Country", "Category1")

dscribeInfo = function(df, name, variab){
  for (k in 1:length(variab)){    
    columns = names(data[, variab])
    col = columns[1:k]
    InfoByVariable <- df %>% 
    group_by_at(vars(one_of(col)))%>%
      summarise(s = sum(loc_quantity)/sum(data$loc_quantity))
    write.csv(InfoByVariable, 
              paste("Describe", name, "By", 
                    paste(variab[1:k], collapse = ""),".csv", sep = "_"))}
  }
  
dscribeInfo(data, "portfolio", va)
