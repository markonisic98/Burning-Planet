#data visualization

library("ggplot2")
library("maps")
library("sf")
require(viridis)
library(dplyr)
library("scales")
library("ggfortify")
library("dendextend")
library(psych)
library(stargazer)

#Individual Variable Statistics 
#once you put this in Latex it should print nicer
stargazer(Emissions_Merged_Full[,-c(15,16,1,2,3,9,13)], type = "html", digits = 4,column.sep.width = "10pt", covariate.labels = c("Top 10% Income Share", "Top 20% Income Share", "Bottom 10% Income Share", "Bottom 20% Income Share", "Population", "GHG Emissions per Capita", "GDP per Capita", "Life Expectancy", "Electricity Usage per Capita"),
          dep.var.labels = c("N", "Mean", "Standard Dev.", "Minimum", "25th Percentile", "75th Percentile", "Maximum"))

#Individual Variable Distribution

par(mfrow=c(3,3))
hist(Emissions_Merged_Full$Emissions.Per.Capita, col = "blue", main = "GHG Emissions per Capita", xlab = "", breaks = "FD")
hist(Emissions_Merged_Full$Population, main = "Population", xlab = "", ylab = "", breaks = "FD")
hist(Emissions_Merged_Full$Top.10.Income.Share, col = "blue", main = "Top 10% Income Share", xlab = "", ylab = "", breaks = "FD")
hist(Emissions_Merged_Full$Top.20.Income.Share, main = "Top 20% Income Share", xlab = "", breaks = "FD")
hist(Emissions_Merged_Full$Bottom.10.Income.Share, col = "blue", main = "Bottom 10% Income Share", xlab = "", ylab = "", breaks = "FD")
hist(Emissions_Merged_Full$Bottom.20.Income.Share, main = "Bottom 20% Income Share", xlab = "", ylab = "", breaks = "FD")
hist(Emissions_Merged_Full$Electricity.Usage.per.Capita, col = "blue", main = "Electricity Usage per Capita", xlab = "", breaks = "FD")
hist(Emissions_Merged_Full$Life.Expectancy, main = "Life Expectancy", xlab = "", ylab = "", breaks = "FD")
hist(Emissions_Merged_Full$GDP.Per.Capita, col = "blue", main = "GDP per Capita", xlab = "", ylab = "", breaks = "FD")


world <- map_data("world")

#making a random forest to predict GHG emissions per Capita 

library(randomForest)
attach(Emissions_Merged_Full)
EmissionsForest = randomForest(Emissions.Per.Capita~GDP.Per.Capita+Life.Expectancy+Top.10.Income.Share+Top.20.Income.Share+
                                 Bottom.10.Income.Share+Bottom.20.Income.Share+Electricity.Usage.per.Capita, ntree=500, importance=TRUE, data = Emissions_Merged_Full)
EmissionsForest
stargazer(importance(EmissionsForest), type="html") 
varImpPlot(EmissionsForest, labels = c("Top 10% Income Share", "Top 20% Income Share", "Bottom 20% Income Share","Bottom 10% Income Share", "GDP per Capita", "Life Expectancy", "Electricity Usage per Capita"))
#top 10 and 20 Income Share did not seem to matter much, but they increased the percentage of Variance explained by 4-5% So I will include them 

#next analyze correlation with each variable and emissions to find the effect of each of these variables 
library(corrplot)
library("PerformanceAnalytics")
quantvars = Emissions_Merged_Full[,c(4:8,10:12,14)]
chart.Correlation(quantvars,histogram = TRUE, pch = 19, title = "Correlation Matrix")

#mapping the predictions of the first random forest to compare to 

Emissions_Merged_Full$FirstPred = predict(EmissionsForest, Emissions_Merged_Full)
Emissions_Merged_Full$Mean_Error_FirstPred_Percentage = (Emissions_Merged_Full$FirstPred-Emissions_Merged_Full$Emissions.Per.Capita)/Emissions_Merged_Full$Emissions.Per.Capita
sqrt(mean(Emissions_Merged_Full$Mean_Error_FirstPred_Percentage^2)) #Mean prediction error as a percentage

#Making a dataframe with only the average of variables for each country, so I can create a map with the data
#And making one for each random forest prediction 
EMF_Average <- aggregate(Emissions_Merged_Full$Emissions.Per.Capita, by = list(Emissions_Merged_Full$Country), mean)
EMF_Average$CountryCode = c("au","at","by","be","bg","ca","hr","cy","dk","ee","fi","fr","de","gr","hu","is","ie","it","jp","lv","lt","lu","mt","nl","no","pl","pt","ro","ru","si","es","se","ch","tr","ua","gb","us")
colnames(EMF_Average)=c("region","Emissions","CountryCode")
holderSet <- with(Emissions_Merged_Full, tapply(Emissions_Merged_Full$GDP.Per.Capita, Emissions_Merged_Full$Country, mean))
EMF_Average$GDP <- holderSet[match(EMF_Average$region,names(holderSet))]
holderSet2 <- with(Emissions_Merged_Full, tapply(Emissions_Merged_Full$Life.Expectancy, Emissions_Merged_Full$Country, mean))
EMF_Average$Life.E.Average <- holderSet2[match(EMF_Average$region,names(holderSet2))]
holderSet3 <- with(Emissions_Merged_Full, tapply(Emissions_Merged_Full$Population, Emissions_Merged_Full$Country, mean))
EMF_Average$Population <- holderSet3[match(EMF_Average$region,names(holderSet3))]
holderSet4 <- with(Emissions_Merged_Full, tapply(Emissions_Merged_Full$Mean_Error_FirstPred_Percentage, Emissions_Merged_Full$Country, mean))
EMF_Average$ME_FirstPred <- holderSet4[match(EMF_Average$region, names(holderSet4))]
holderSet5 <- with(Emissions_Merged_Full, tapply(Emissions_Merged_Full$Top.10.Income.Share, Emissions_Merged_Full$Country, mean))
EMF_Average$Top.10.Income.Share <- holderSet5[match(EMF_Average$region, names(holderSet5))]
holderSet6 <- with(Emissions_Merged_Full, tapply(Emissions_Merged_Full$Top.20.Income.Share, Emissions_Merged_Full$Country, mean))
EMF_Average$Top.20.Income.Share <- holderSet6[match(EMF_Average$region, names(holderSet6))]
holderSet7 <- with(Emissions_Merged_Full, tapply(Emissions_Merged_Full$Bottom.10.Income.Share, Emissions_Merged_Full$Country, mean))
EMF_Average$Bottom.10.Income.Share <- holderSet7[match(EMF_Average$region, names(holderSet7))]
holderSet8 <- with(Emissions_Merged_Full, tapply(Emissions_Merged_Full$Bottom.20.Income.Share, Emissions_Merged_Full$Country, mean))
EMF_Average$Bottom.20.Income.Share <- holderSet8[match(EMF_Average$region, names(holderSet8))]
holderSet9 <- with(Emissions_Merged_Full, tapply(Emissions_Merged_Full$Electricity.Usage.per.Capita, Emissions_Merged_Full$Country, mean))
EMF_Average$Electricity.Usage.Per.Capita <- holderSet9[match(EMF_Average$region, names(holderSet9))]

#check for disagreements between the datasets
diff <- setdiff(world$region, EMF_Average$region)
diff
EMF_Average$region = ifelse(EMF_Average$region=="United States of America", "USA", EMF_Average$region)
EMF_Average$region = ifelse(EMF_Average$region=="United Kingdom", "UK", EMF_Average$region)
Emissions_map <- inner_join(world,EMF_Average, by = "region")

#Make dataset to used for second map (Emissions Predictions Map)
EMF_Average_FirstPred <- aggregate(Emissions_Merged_Full$FirstPred, by = list(Emissions_Merged_Full$Country), mean)
colnames(EMF_Average_FirstPred)=c("region","Predicted.Emissions")

#making world map with emissions per capita 
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "black"),
  plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
)

ggplot(data=Emissions_map, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Emissions)) +
  scale_fill_distiller(palette ="RdYlGn", direction = -1, name="GHG Emissions per Capita") + # or direction=1
  ggtitle("GHG Emissions per Capita") +
  plain

#Same map with a focus on Europe 

Emissions_map_Europe <- inner_join(world,EMF_Average[-c(1,6,19,29,37),], by = "region") 

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "black"),
  plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
)

ggplot(data=Emissions_map_Europe, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Emissions)) +
  scale_fill_distiller(palette ="RdYlGn", direction = -1, name="GHG Emissions per Capita") + # or direction=1
  ggtitle("GHG Emissions per Capita: Europe") +
  plain


#mapping the predictions of the first random forest to compare to 
EMF_Average_FirstPred$region = ifelse(EMF_Average_FirstPred$region=="United States of America", "USA", EMF_Average_FirstPred$region)
EMF_Average_FirstPred$region = ifelse(EMF_Average_FirstPred$region=="United Kingdom", "UK", EMF_Average_FirstPred$region)
Pred1_map <- inner_join(world,EMF_Average_FirstPred, by = "region") 

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "Black"),
  plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
)

ggplot(data=Pred1_map, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Predicted.Emissions)) +
  scale_fill_distiller(palette ="RdYlGn", direction = -1, name="GHG Emissions per Capita") + # or direction=1
  ggtitle("Predicted GHG Emissions per Capita") +
  plain

#Focusing predictions on Europe
Pred1_map_Europe <- inner_join(world,EMF_Average_FirstPred[-c(1,6,19,29,37),], by = "region") 

#prediction map but only for Europe
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "Black"),
  plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
)

ggplot(data=Pred1_map_Europe, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = Predicted.Emissions)) +
  scale_fill_distiller(palette ="RdYlGn", direction = -1, name="GHG Emissions per Capita") + # or direction=1
  ggtitle("Predicted GHG Emissions per Capita: Europe") +
  plain

#flag plot visualizations
if (!require("ggflags")) {
  devtools::install_github("rensa/ggflags")
  library(ggflags)
}

#creating a bar chart with the difference in predictions to better visualize individual differences
EMF_Average %>%
  mutate(code = tolower(CountryCode)) %>%
  ggplot(aes(x = reorder(CountryCode, ME_FirstPred), y = ME_FirstPred)) +
  geom_bar(stat = "identity") +
  geom_flag(y = -0.2, aes(country = CountryCode), size = 5) +
  scale_y_continuous(limits = c(-0.2,0.2), labels = percent) +
  xlab("Country") +
  ylab("Prediction Mean Error (%)") +
  theme(legend.title = element_blank(), legend.position = "none", plot.title = element_text(hjust = 0.5, face = "bold", size = 14)) +
  coord_flip() + 
  geom_col(aes(fill=ME_FirstPred)) +
  scale_fill_gradient2(low="red", mid = "black" , high="green") + 
  ggtitle("Random Forest Prediction Error")

#Per Capita GDP and GHG Emissions 
#Size is based on Population
ggplot(EMF_Average, aes(x=Emissions, y=GDP, country=CountryCode, size=Population)) + 
  geom_flag() + 
  scale_country() +
  scale_radius(range = c(10, 23)) + 
  labs(x = "GHG Emissions Per Capita (Kilotonnes CO2 Equivalents)", y = "GDP Per Capita", country = "Country Code") +
  ggtitle("Per Capita GDP and GHG Emissions") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

#Life Expectancy and Emissions, with GDP per capita as size 
ggplot(EMF_Average, aes(x=Emissions, y=Life.E.Average, country=CountryCode, size=GDP)) + 
  geom_flag() + 
  scale_country() +
  scale_radius(range = c(10, 23)) + 
  labs(x = "GHG Emissions Per Capita (Kilotonnes CO2 Equivalents)", y = "Life Expectancy", country = "Country Code", size = "GDP per Capita") +
  ggtitle("Per Capita GHG Emissions and Life Expectancy") + 
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

#hierarchical cluster analysis 

EMF_Cluster = EMF_Average[,c(4,5,8:12)]
rownames(EMF_Cluster) = EMF_Average$region #change this to country code if you find a way to include flags in the dendogram 
HC_Clust = hclust(dist(EMF_Cluster))
#put the clusters into a dataframe so they can be used for the map
clus4 = as.data.frame(cutree(HC_Clust,4))
colnames(clus4)="Cluster"
par(mfrow=c(1,1))
plot(HC_Clust, hang=0.1, cex=0.4)
dend=as.dendrogram(HC_Clust)
dend1 = color_labels(dend, col = c("purple","black","red","blue"),k=4)
par(cex=0.7)
plot(dend1, main = "Color-coded Cluster Dendrogram")

#make world map based on cluster data 

EMF_Average$Cluster = clus4$Cluster
Emissions_map <- inner_join(world,EMF_Average, by = "region")

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

ggplot(data=Emissions_map, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  scale_fill_gradient(low="blue",  high="red") +
  geom_polygon(aes(fill = Cluster)) +
  ggtitle("Color-coded Clusters") +
  plain

### Next show the averages of each cluster in terms of each variable 
#pca analysis with the average for each country, separated by cluster
#This will give us an idea of the characteristics of each cluster in the hierarchical cluster we created 

label_vars=EMF_Average[,c(1,3)]
Emissions_pca_vars=EMF_Average[,c(4,5,8:12)]

pca_emissions = prcomp(Emissions_pca_vars, scale=TRUE)
pca_emissions

#plotting pca analysis 

autoplot(pca_emissions, data=Emissions_pca_vars,loadings=TRUE, col=ifelse(EMF_Average$Cluster==1,"blue",ifelse(EMF_Average$Cluster==2,"purple", ifelse(EMF_Average$Cluster==3,"black","red"))), loadings.label=TRUE) + 
  ggtitle("Principal Component Analysis") +
  theme(plot.title = element_text(size=14, face = "bold", hjust = 0.5))

#Showing the percentage of variance explained by each predictor 

pve=(pca_emissions$sdev^2)/sum(pca_emissions$sdev^2)
par(mfrow=c(1,2))
plot(pve,ylim=c(0,1), col="blue")
plot(cumsum(pve), ylim=c(0,1), col="red")

#Showing the mean of each variable for each cluster 

describeBy(EMF_Average$Emissions, EMF_Average$Cluster, mat=TRUE)
describeBy(EMF_Average$GDP, EMF_Average$Cluster, mat=TRUE)
describeBy(EMF_Average$Life.E.Average, EMF_Average$Cluster, mat=TRUE)
describeBy(EMF_Average$Electricity.Usage.Per.Capita, EMF_Average$Cluster, mat=TRUE)

stargazer(aggregate(EMF_Average[,c(2,4,5,12)], list(EMF_Average$Cluster), mean), type = "html", title = "Mean by Cluster")

#View important datasets
View(Emissions_Merged_Full)
View(EMF_Average)
View(EMF_Average_FirstPred)
View(EMF_Cluster)
View(clus4)



