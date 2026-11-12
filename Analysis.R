#This is the Analyisis for the Statistics 2 project by Daniel Puglisi 3355544, Giulio Bondi .... and Matteo D'Anna.

#For simplicity we rename the Hotels file to "hotels"
hotels <- read.csv("30457_hotel _2026_2027.csv")

#Starting with Descriptive analysis for each category

#Continent (Nominal Variable)
counts <- table(hotels$continent)

continent_summary <-data.frame(
  Category = names(counts),
  Frequency = as.vector (counts),
  Relative_Frequency = round(as.vector(prop.table(counts)),4),
  percentage = round(100*as.vector(prop.table(counts)),2)
)

barplot(counts,
        names.arg = names(counts),
        main = "frequency plot for continent",
        ylab = "Frequency",
        las = 2
)