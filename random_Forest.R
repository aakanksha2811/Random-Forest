#Libraries needed

library(ggplot2)
library(ggthemes)
library(corrplot)
library(reshape2)
library(dplyr)
library(randomForest)

#Load in our dataset

glass<-read.csv("glass.csv", header = TRUE, sep = ",")

# summary statistics
str(glass)

summary(glass)

#Scatterplot Matrix of Variables
plot(glass)

#Correlation Heatmap of Variables
corrplot(cor(glass))

#Baseline Random Forest Model
glassRF<-randomForest((RI)~.-Type,glass,ntree=150)
glassRF

# Get importance
importance    <- importance(glassRF)

varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_classic()

#Density Plot

d= density(glass$RI)
plot(d,main ="Density of Refractive Index")
polygon(d,col="red",border="blue")