# 607-Assignment1


## Overview

Your task is to study the Mushrooms dataset and the associated description of the data (i.e. “data dictionary”). This famous dataset can be found in the UCI repository. Your deliverable is the R code to perform the transformation tasks.
A typical problem (which is beyond the scope of this assignment!) is to answer the question, 
“Which other attribute or attributes are the best predictors of whether a particular mushroom is poisonous or edible?”

## Data Tidying and Transformation

Import Mushrooms data and load library:
```{r}
library(ggplot2)
```

```{r}
mushroom <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"), header=FALSE)
```

Add meaningful column names:
```{r}
names(mushroom)=c("class", "capShape", "capSurface", "capColor", "bruises", "odor", "gillAttachment", "gillSpacing", "gillSize","gillColor", "stalkShape", "stalkRoot", "stalkSurfaceAboveRing", "stalkSurfaceBelowRing", "stalkColorAboveRing", "stalkColorBelowRing", "veilType", "veilColor", "ringNumber", "ringType", "sporePrintColor", "population", "habitat")

```

Replace abbreviations used in the data:
  
```{r}
levels(mushroom$class) <- c("edible", "poisonous")
levels(mushroom$capShape) <- c("bell", "conical", "flat", "knobbed", "sunken", "convex")
levels(mushroom$capSurface) <- c("fibrous", "grooves", "smooth", "scaly")
levels(mushroom$capColor) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", "green", "purple", "white", "yellow")
levels(mushroom$bruises) <- c("false", "true")
levels(mushroom$odor) <- c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy")
levels(mushroom$gillAttachment) <- c("attached", "free")
levels(mushroom$gillSpacing) <- c("close", "crowded")
levels(mushroom$gillSize) <- c("broad", "narrow")
levels(mushroom$gillColor) <- c("buff", "red", "gray", "chocolate", "black", "brown", "orange", "pink", "green", "purple", "white", "yellow")
levels(mushroom$stalkShape) <- c("enlarging","tapering")     
levels(mushroom$stalkRoot) <- c("missing", "bulbous", "club", "equal", "rooted")
levels(mushroom$stalkSurfaceAboveRing) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushroom$stalkSurfaceBelowRing) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushroom$stalkColorAboveRing) <- c("buff", "cinnamon", "red", "gray", "brown", "orange", "pink", "white", "yellow")
levels(mushroom$stalkColorBelowRing) <- c("buff", "cinnamon", "red", "gray", "brown", "orange", "pink", "white", "yellow")
levels(mushroom$veilType) <- c("partial")
levels(mushroom$veilColor) <- c("brown", "orange", "white", "yellow")
levels(mushroom$ringNumber) <- c("none", "one", "two")
levels(mushroom$ringType) <- c("evanescent", "flaring", "large", "none", "pendant")
levels(mushroom$sporePrintColor) <- c("buff", "chocolate", "black", "brown", "orange", "green", "purple", "white", "yellow")
levels(mushroom$population) <- c("abundant", "clustered", "numerous", "scattered", "several", "solitary")
levels(mushroom$habitat) <- c("woods", "grasses", "leaves", "meadows","paths", "urban", "waste")

```

## Investigation

Let's investigate whether color is a good indicator of whether a mushroom is edible or poisonous.
Create a data frame with a subset of the columns in the dataset: 
                              
```{r}
mushroomSubset <- subset(mushroom, select=c(class, gillColor, stalkColorAboveRing, stalkColorBelowRing, sporePrintColor))

summary(mushroomSubset)     
```

Let's further separate the subset by class:

```{r}
mushroomColorP <- subset(mushroom, class=="poisonous", select=c(gillColor, stalkColorAboveRing, stalkColorBelowRing, sporePrintColor))

summary(mushroomColorP)

mushroomColorE <- subset(mushroom, class=="edible", select=c(gillColor, stalkColorAboveRing, stalkColorBelowRing,sporePrintColor))

summary(mushroomColorE)
```

Let's visualize the subsets with stacked bar graphs:

```{r}
plot1 <- ggplot(mushroom, aes(x=class,fill = sporePrintColor)) + 
  geom_bar(position='stack')+
  geom_text(stat='count', aes(label=..count..), position=position_stack(vjust=0.1))  +
  ggtitle("Edible vs Poisonous: Spore Print Color")

plot1

plot2 <- ggplot(mushroom, aes(x=class,fill = stalkColorBelowRing)) + 
  geom_bar(position='stack')+
  geom_text(stat='count', aes(label=..count..), position=position_stack(vjust=0.1)) +
  ggtitle("Edible vs Poisonous: Stalk Color Below Ring")

plot2

plot3 <- ggplot(mushroom, aes(x=class,fill = stalkColorAboveRing)) + 
  geom_bar(position='stack')+
  geom_text(stat='count', aes(label=..count..), position=position_stack(vjust=0.1)) +
  ggtitle("Edible vs Poisonous: Stalk Color Above Ring")

plot3

plot4 <- ggplot(mushroom, aes(x=class,fill = gillColor)) + 
  geom_bar(position='stack')+
  geom_text(stat='count', aes(label=..count..), position=position_stack(vjust=0.1)) +
  ggtitle("Edible vs Poisonous: Gill Color")

plot4

```

After comparing the 4 bar graph plots, it appears that most edible mushrooms are white above and below the stalk. They also tend to have white or brown gills and brown or black spore prints. On the other hand, poisonous mushroom tend to be pink or white above and below the stalk. Most of them have buff gills and white or chocolate spore prints. However, these tendencies are not a rule. There does not seem to be one color that is exclusively associated with one class.

Is there a better characteristic that determines whether a mushroom is edible or poisonous besides color?

Let's investigate odor. Create another subset of the original mushroom data with only class and odor:

```{r}
odorSubet <- subset(mushroom, select=c(class, odor))

summary(odorSubet)

plot5 <- ggplot(mushroom, aes(x=class,fill = odor)) + 
  geom_bar(position='stack')+
  geom_text(stat='count', aes(label=..count..), position=position_stack(vjust=0.1)) +
  ggtitle("Edible vs Poisonous: Odor")

plot5
```



```{r}
odor <- table(mushroom$class, mushroom$odor)

odor
```

As seen in the table and bar graph, all poisonous mushrooms have a creosote, foul, musty, pungent, spicy, or fishy odor. All of the edible mushrooms have an almond or anise odor. The only odor category with overlap of the two classes is "none". However, only 36 mushrooms with no odor are poisonous compared to 3408 edible mushrooms with no odor. Therefore,  odor seems to be a better indicator of whether a mushroom is poisonous than color. 

Adding all the mushroom counts together with unpleasant odors (creosote, foul, musty, pungent, spicy, or fishy) and dividing that sum by total number of poisonous mushrooms in the data set we have:

```{r}
(192+2160+36+256+576+576)/3916
```
 Doing the same with the mushrooms with pleasant odors (almond and anise) we have:
```{r}
(400+400)/4208
```

Calculating the chance that a mushroom with no odor is poisonous we have:
```{r}
120/3408
```


Thus, using unpleasant odor correctly classifies about 96.94% of mushrooms in the data set as poisonous. However, using pleasant odor only correctly classifies about 19.01% of edible mushrooms. There is about a 3.52% chance that a mushroom with no odor is poisonous. Overall, odor seems to be a good starting place to determine whether a mushroom is poisonous.

