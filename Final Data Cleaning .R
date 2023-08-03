###Variable Notes 
  #All Information gathered from UN Website 
  #All information between is between 1990 and 2018, some variables will not have data for all of the values
  #Emissions are in kilotonne CO2 Equivalents (refers to greenhouse gas emissions)
  #GDP is all in constant 2011 US Dollars 
  #All per Capita measures are created within the set by dividing totals by the population
  #The goal of this is not to have each country's year by year changes documented, but rather each year and each country can be treated as individual data points 
  #Treating each country and year as specific data point in our analysis will give us more samples of how the variables influence GHG Emissions 
  #It is also important to treat countries with specific years as their own entities because of socioeconomic and political changes from year to year 


#Opening the individual datasets
  library(readr)
  GHG_Emissions_Countries <- read_csv("Downloads/GHG_Emissions_Countries.csv")
  Top_10_Income_Share <- read_csv("Downloads/Top_10_Income_Share.csv")
  Population <- read_csv("Downloads/Population.csv")
  GDP_Total <- read_csv("Downloads/GDP_Total.csv") #Note that GDP is in constant 2011 US Dollars 
  Top_20_Income_Share <- read_csv("Downloads/Top_20_Income.csv")
  Bottom_10_Income_Share <- read_csv("Downloads/Bottom_10_Income.csv")
  Bottom_20_Income_Share <- read_csv("Downloads/Bottom_20_Income.csv")
  Life_Expectancy <- read_csv("Downloads/Life_Expectancy.csv")
  Female_Legislators_and_Managers <- read_csv("Downloads/Female_Legislators_and_Managers.csv")
  Female_Uni_Teachers <- read_csv("Downloads/Female_Uni_Teachers.csv")
  Electricity_Usage <- read_csv("Downloads/Electricity_Usage.csv")  #In kilowatt hours, millions 


#cleaning and merging the datasets 
  Top_10_Income_Share=Top_10_Income_Share[,-c(4)]
  Population=Population[,-c(3)]
  GDP_Total=GDP_Total[,-c(4)]
  Top_20_Income_Share=Top_20_Income_Share[,-c(4)]
  Bottom_10_Income_Share=Bottom_10_Income_Share[,-c(4)]
  Bottom_20_Income_Share=Bottom_20_Income_Share[,-c(4)]
  Life_Expectancy=Life_Expectancy[,-c(3,5:22)]
  Female_Legislators_and_Managers=Female_Legislators_and_Managers[,-c(2,4,5,7)]
  Female_Uni_Teachers=Female_Uni_Teachers[,-c(4)]
  Electricity_Usage=Electricity_Usage[,-c(2,4,6)]


  colnames(Top_10_Income_Share)[which(names(Top_10_Income_Share) == "Value")] <- "Top.10.Income.Share"
  colnames(Top_10_Income_Share)[which(names(Top_10_Income_Share) == "Country or Area")] <- "Country"
  colnames(Top_20_Income_Share)[which(names(Top_20_Income_Share) =="Value")] <- "Top.20.Income.Share"
  colnames(Top_20_Income_Share)[which(names(Top_20_Income_Share) =="Country or Area")] <- "Country"
  colnames(Bottom_10_Income_Share)[which(names(Bottom_10_Income_Share) =="Value")] <- "Bottom.10.Income.Share"
  colnames(Bottom_10_Income_Share)[which(names(Bottom_10_Income_Share) =="Country or Area")] <- "Country"
  colnames(Bottom_20_Income_Share)[which(names(Bottom_20_Income_Share) =="Value")] <- "Bottom.20.Income.Share"
  colnames(Bottom_20_Income_Share)[which(names(Bottom_20_Income_Share) =="Country or Area")] <- "Country"
  colnames(GHG_Emissions_Countries)[which(names(GHG_Emissions_Countries) == "Country or Area")] <- "Country"
  colnames(GHG_Emissions_Countries)[which(names(GHG_Emissions_Countries) == "Value")] <- "Emissions"
  colnames(Population)[which(names(Population) == "Value")] <- "Population"
  colnames(Population)[which(names(Population) == "Country or Area")] <- "Country"
  colnames(Population)[which(names(Population) == "Year(s)")] <- "Year"
  colnames(GDP_Total)[which(names(GDP_Total) =="Value")] <- "GDP.Total"
  colnames(GDP_Total)[which(names(GDP_Total) == "Country or Area")] <- "Country"
  colnames(Life_Expectancy)[which(names(Life_Expectancy) == "Life expectancy")] <- "Life.Expectancy"
  colnames(Female_Legislators_and_Managers)[which(names(Female_Legislators_and_Managers) =="Value")] <- "Percent.Legislators.and.Managers.Female"
  colnames(Female_Legislators_and_Managers)[which(names(Female_Legislators_and_Managers) =="Country or Area")] <- "Country"
  colnames(Female_Uni_Teachers)[which(names(Female_Uni_Teachers) =="Country or Area")] <- "Country"
  colnames(Female_Uni_Teachers)[which(names(Female_Uni_Teachers) =="Value")] <- "Percent.Uni.Teachers.Female"
  colnames(Electricity_Usage)[which(names(Electricity_Usage) =="Quantity")] <- "Electricity.Usage"
  colnames(Electricity_Usage)[which(names(Electricity_Usage) =="Country or Area")] <- "Country"

#Making the United States a Unified Country (Different Titles in Different Datasets)

  GHG_Emissions_Countries$Country = ifelse(GHG_Emissions_Countries$Country=="United States", "United States of America", GHG_Emissions_Countries$Country)
  GHG_Emissions_Countries$Country = ifelse(GHG_Emissions_Countries$Country=="Russian Federation", "Russia", GHG_Emissions_Countries$Country)
  Top_10_Income_Share$Country = ifelse(Top_10_Income_Share$Country=="United States", "United States of America", Top_10_Income_Share$Country)
  Population$Country = ifelse(Population$Country=="United States", "United States of America", Population$Country)
  Population$Country = ifelse(Population$Country=="Russian Federation", "Russia", Population$Country)
  GDP_Total$Country = ifelse(GDP_Total$Country=="United States", "United States of America", GDP_Total$Country)
  Top_20_Income_Share$Country = ifelse(Top_20_Income_Share$Country=="United States", "United States of America", Top_20_Income_Share$Country)
  Bottom_10_Income_Share$Country = ifelse(Bottom_10_Income_Share$Country=="United States", "United States of America", Bottom_10_Income_Share$Country)
  Bottom_20_Income_Share$Country = ifelse(Bottom_20_Income_Share$Country=="United States", "United States of America", Bottom_20_Income_Share$Country)
  Life_Expectancy$Country = ifelse(Life_Expectancy$Country=="United States", "United States of America", Life_Expectancy$Country)
  Female_Legislators_and_Managers$Country = ifelse(Female_Legislators_and_Managers$Country=="United States", "United States of America", Female_Legislators_and_Managers$Country)
  Female_Uni_Teachers$Country = ifelse(Female_Uni_Teachers$Country=="United States", "United States of America", Female_Uni_Teachers$Country)
  Electricity_Usage$Country = ifelse(Electricity_Usage$Country=="United States", "United States of America", Electricity_Usage$Country)
  Electricity_Usage$Country = ifelse(Electricity_Usage$Country=="Russian Federation", "Russia", Electricity_Usage$Country)
  Life_Expectancy$Country = ifelse(Life_Expectancy$Country=="United Kingdom of Great Britain and Northern Ireland","United Kingdom", Life_Expectancy$Country)
  Life_Expectancy$Country = ifelse(Life_Expectancy$Country=="Russian Federation","Russia", Life_Expectancy$Country)
  
#merging the sets
  
  Emissions_Merged<-merge(GHG_Emissions_Countries,Top_10_Income_Share, by=c("Country","Year"), all=TRUE)
  Emissions_Merged<-merge(Emissions_Merged,Top_20_Income_Share, by=c("Country","Year"), all=TRUE)
  Emissions_Merged<-merge(Emissions_Merged,Bottom_10_Income_Share, by=c("Country","Year"), all=TRUE)
  Emissions_Merged<-merge(Emissions_Merged,Bottom_20_Income_Share, by=c("Country","Year"), all=TRUE)
  Emissions_Merged<-merge(Emissions_Merged,Population, by=c("Country","Year"), all=TRUE)
  Emissions_Merged<-merge(Emissions_Merged,GDP_Total, by=c("Country","Year"), all=TRUE)
  Emissions_Merged$Population=Emissions_Merged$Population*1000 #Normalizing the units that are in thousands
  Emissions_Merged$Emissions.Per.Capita=Emissions_Merged$Emissions/Emissions_Merged$Population
  Emissions_Merged$GDP.Per.Capita=Emissions_Merged$GDP.Total/Emissions_Merged$Population
  Emissions_Merged<-merge(Emissions_Merged,Life_Expectancy, by=c("Country","Year"), all=TRUE)
  Emissions_Merged<-merge(Emissions_Merged,Female_Legislators_and_Managers, by=c("Country","Year"), all=TRUE)
  Emissions_Merged<-merge(Emissions_Merged,Female_Uni_Teachers, by=c("Country","Year"), all=TRUE)
  Emissions_Merged$Country = ifelse(Emissions_Merged$Country=="United States", "United States of America", Emissions_Merged$Country)
  table(Emissions_Merged_Full$Country)


#Making Second Emissions_Merged with data that has all of its columns full

  Emissions_Merged_Full<-merge(GHG_Emissions_Countries,Top_10_Income_Share, by=c("Country","Year"))
  View(Emissions_Merged_Full)
  Emissions_Merged_Full<-merge(Emissions_Merged_Full,Top_20_Income_Share, by=c("Country","Year"))
  Emissions_Merged_Full<-merge(Emissions_Merged_Full,Bottom_10_Income_Share, by=c("Country","Year"))
  Emissions_Merged_Full<-merge(Emissions_Merged_Full,Bottom_20_Income_Share, by=c("Country","Year"))
  Emissions_Merged_Full<-merge(Emissions_Merged_Full,Population, by=c("Country","Year"))
  Emissions_Merged_Full<-merge(Emissions_Merged_Full,GDP_Total, by=c("Country","Year"))
  Emissions_Merged_Full$Population=Emissions_Merged_Full$Population*1000 #Normalizing the units that are in thousands
  Emissions_Merged_Full$Emissions.Per.Capita=Emissions_Merged_Full$Emissions/Emissions_Merged_Full$Population
  Emissions_Merged_Full$GDP.Per.Capita=Emissions_Merged_Full$GDP.Total/Emissions_Merged_Full$Population
  Emissions_Merged_Full<-merge(Emissions_Merged_Full,Life_Expectancy, by=c("Country","Year"))
  Emissions_Merged_Full<-merge(Emissions_Merged_Full, Electricity_Usage, by=c("Country", "Year"))
  Emissions_Merged_Full$Electricity.Usage.per.Capita=Emissions_Merged_Full$Electricity.Usage/Emissions_Merged_Full$Population
  
#remove duplicates 
 
   Emissions_Merged_Full = Emissions_Merged_Full[!duplicated(Emissions_Merged_Full$Emissions),]
   View(Emissions_Merged_Full)

#Make new dataframe for the countries with data on female leaders 
  
  Emissions_Merged_Full_Female<-merge(Emissions_Merged_Full,Female_Legislators_and_Managers, by=c("Country","Year"))
  Emissions_Merged_Full_Female<-merge(Emissions_Merged_Full_Female,Female_Uni_Teachers, by=c("Country","Year"))

